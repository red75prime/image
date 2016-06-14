use std::borrow::Cow;
use std::error::Error;
use std::io::{Read, Seek, SeekFrom, self};
use std::iter::{Iterator, repeat, Rev};
use std::slice::ChunksMut;
use byteorder::{ReadBytesExt, LittleEndian};

use image::{
    DecodingResult,
    ImageResult,
    ImageDecoder,
    ImageError
};
use color::ColorType;

static SIGNATURE: &'static [u8] = b"#?RADIANCE";
const SIGNATURE_LENGTH: usize = 10;

/// An Radiance HDR decoder
pub struct HDRDecoder<R> {
    r: R,

    width: u32,
    height: u32,
    exposure: f32,
    color_correction: (f32,f32,f32),
    software: String,
    pixel_aspect_ratio: f32,
    primaries: ((f32, f32), (f32, f32), (f32, f32), (f32, f32)),
}

impl<R: Read + Seek> HDRDecoder<R> {

    /// Creates a new decoder that decodes from the stream ```r```
    /// or returns error if it cannot parse a header
    pub fn new(reader: R) -> ImageResult<HDRDecoder<R>> {
        HDRDecoder::with_strictness(reader, false)
    }    

    /// Creates a new decoder that decodes from the stream ```r```
    /// or returns error if it cannot parse a header
    /// strict enables strict mode
    pub fn with_strictness(mut reader: R, strict: bool) -> ImageResult<HDRDecoder<R>> {  
        // default values for attributes  
        let mut exposure = 1.;
        let mut color_correction = (1., 1., 1.);
        let mut software = "".into();
        let mut pixel_aspect_ratio = 1.;
        let mut primaries = ((0.640, 0.330), (0.290, 0.600), (0.150, 0.060), (0.333, 0.333));

        { // scope to make borrowck happy
            let r = &mut reader;
            let mut signature = [0; SIGNATURE_LENGTH];

            try!(r.read_exact(&mut signature));

            if signature != SIGNATURE {
                return Err(ImageError::FormatError("Radiance HDR signature not found".to_string()));
            } // no else

            // skip signature line ending
            try!(read_line_u8(r));

            // read header data until empty line
            loop {
                match try!(read_line_u8(r)) {
                    None => {
                        // EOF before end of header
                        return Err(ImageError::NotEnoughData);
                    },
                    Some(line) => {
                        if line.len() == 0 {
                            // end of header
                            break; 
                        } else if line[0] == b'#' { // line[0] will not panic, line.len() == 0 is false here  
                            // skip comments
                            continue;
                        } // no else
                        let line = String::from_utf8_lossy(&line[..]);
                        match split_at_first(&line, "=") {
                            Some(("FORMAT", val)) => {
                                if val.trim() != "32bit_rle_rgbe" {
                                    // XYZE isn't supported yet
                                    return Err(ImageError::UnsupportedError(val.into()));
                                }
                            },
                            Some(("EXPOSURE", val)) => {
                                let multiplier = try!(val.trim().parse::<f32>().into_image_error("Cannot parse EXPOSURE value:")); 
                                exposure *= multiplier;
                            },
                            Some(("SOFTWARE", val)) => {
                                software = val.into();
                            },
                            None => {
                                if strict {
                                    // TODO: limit reported line length  
                                    return Err(ImageError::FormatError(format!("Malformed attribute '{}'", line)));
                                } else {
                                    // skip malformed attribute
                                }
                            },
                            _ => {
                                // TODO: process other attributes
                            },
                        } // match attributes
                    }, // <= Some(line)
                } // match read_line_u8()
            } // loop
        } // scope to end borrow of reader   
        // parse dimensions
        let (width, height) =
            match try!(read_line_u8(&mut reader)) {
                None => {
                    // EOF instead of image dimensions
                    return Err(ImageError::NotEnoughData);
                },
                Some(dimensions) => {
                    let dimensions = String::from_utf8_lossy(&dimensions[..]);
                    try!(parse_dimensions_line(&dimensions, strict))
                },
            };

        Ok(HDRDecoder {
            r: reader,

            width: width,
            height: height,
            exposure: exposure,
            color_correction: color_correction,
            software: software,
            pixel_aspect_ratio: pixel_aspect_ratio,
            primaries: primaries,
        })
    }
}

// Parses dimension line "-Y height +X widht"
// returns (width, height) or error
fn parse_dimensions_line<'a>(line: &Cow<'a, str>, strict: bool) -> ImageResult<(u32,u32)> {
    let mut dim_parts = line.split_whitespace();
    let err = "Malformed dimensions line";
    let c1_tag = try!(dim_parts.next().ok_or(ImageError::FormatError(err.into())));
    let c1_str = try!(dim_parts.next().ok_or(ImageError::FormatError(err.into())));
    let c2_tag = try!(dim_parts.next().ok_or(ImageError::FormatError(err.into())));
    let c2_str = try!(dim_parts.next().ok_or(ImageError::FormatError(err.into())));
    if strict {
        if let Some(_) = dim_parts.next() {
            // extra data in dimensions line
            return Err(ImageError::FormatError(err.into()));
        } // no else
    } // no else
    // dimensions line is in the form "-Y 10 +X 20"
    // There are 8 possible orientations: +Y +X, +X -Y and so on
    match (c1_tag, c2_tag) {
        ("-Y", "+X") => {
            // Common orientation (top-down, left-right)
            // c1_str is heigth, c2_str is width
            let height = try!(c1_str.parse::<u32>().into_image_error(err));
            let width = try!(c2_str.parse::<u32>().into_image_error(err));
            Ok((width, height))
        },
        _ => {
            Err(ImageError::UnsupportedError(format!("Unsupported orientation {} {}", c1_tag, c2_tag)))
        }
    } // final expression. Returns value 
}

trait IntoImageError<T> {
    fn into_image_error(self, description: &str) -> ImageResult<T>;
}

impl<T> IntoImageError<T> for ::std::result::Result<T, ::std::num::ParseFloatError> {
    fn into_image_error(self, description: &str) -> ImageResult<T> {
        self.map_err(|err| ImageError::FormatError(format!("{} {}", description, err.description())))
    }
}

impl<T> IntoImageError<T> for ::std::result::Result<T, ::std::num::ParseIntError> {
    fn into_image_error(self, description: &str) -> ImageResult<T> {
        self.map_err(|err| ImageError::FormatError(format!("{} {}", description, err.description())))
    }
}

// Splits string into (before separator, after separator) tuple
// or None if separator isn't found 
fn split_at_first<'a>(s: &'a Cow<'a, str>, separator: &str) -> Option<(&'a str, &'a str)> {
    match s.find(separator) {
        None => None,
        Some(0) => None,
        Some(p) if p >= s.len()-separator.len() => None,
        Some(p) => Some((&s[..p], &s[(p+separator.len())..])),
    } 
}

#[test]
fn split_at_first_test() {
    assert_eq!(split_at_first(&Cow::Owned("".into()), "="), None);
    assert_eq!(split_at_first(&Cow::Owned("=".into()), "="), None);
    assert_eq!(split_at_first(&Cow::Owned("= ".into()), "="), None);
    assert_eq!(split_at_first(&Cow::Owned(" = ".into()), "="), Some((" ", " ")));
    assert_eq!(split_at_first(&Cow::Owned("EXPOSURE= ".into()), "="), Some(("EXPOSURE"," ")));
    assert_eq!(split_at_first(&Cow::Owned("EXPOSURE= =".into()), "="), Some(("EXPOSURE"," =")));
    assert_eq!(split_at_first(&Cow::Owned("EXPOSURE== =".into()), "=="), Some(("EXPOSURE"," =")));
    assert_eq!(split_at_first(&Cow::Owned("EXPOSURE".into()), ""), None);
}

// Reads inpult until b"\n" or EOF
// Returns vector of read bytes NOT including end of line characters
//   or return None to indicate end of file
fn read_line_u8<R: Read + Seek>(r: &mut R) -> ::std::io::Result<Option<Vec<u8>>> {
    let mut ret = Vec::with_capacity(16);
    let mut no_data = true;

    let byte_buf = &mut [0];
    loop {
        let bytes_read = try!(r.read(byte_buf));
        if bytes_read == 0 {
            // EOF
            return if no_data { Ok(None) } else { Ok(Some(ret)) };
        } // no else
        no_data = false;
        // HDR format doesn't specify encoding of end-of-line
        // C implementation uses fgets
        // Let's assume it is '\n'
        if byte_buf[0] == b'\n' {
            // "\n" line ending
            return Ok(Some(ret));
        } // end of EOL processing
        ret.push(byte_buf[0]);
    }
}

#[test]
fn read_line_u8_test() {
    let buf: Vec<_> = (&b"One\nTwo\nThree\nFour\n\n\n"[..]).into();
    let input = &mut ::std::io::Cursor::new(buf);
    assert_eq!(&read_line_u8(input).unwrap().unwrap()[..], &b"One"[..]);
    assert_eq!(&read_line_u8(input).unwrap().unwrap()[..], &b"Two"[..]);
    assert_eq!(&read_line_u8(input).unwrap().unwrap()[..], &b"Three"[..]);
    assert_eq!(&read_line_u8(input).unwrap().unwrap()[..], &b"Four"[..]);
    assert_eq!(&read_line_u8(input).unwrap().unwrap()[..], &b""[..]);
    assert_eq!(&read_line_u8(input).unwrap().unwrap()[..], &b""[..]);
    assert_eq!(read_line_u8(input).unwrap(), None);
}
