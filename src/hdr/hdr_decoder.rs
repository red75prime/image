use std::borrow::Cow;
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

    width: i32,
    height: i32,
    exposure: f32,
    color_correction: (f32,f32,f32),
    software: String,
    pixel_aspect_ratio: f32,
    primaries: ((f32, f32), (f32, f32), (f32, f32), (f32, f32)),
}

impl<R: Read + Seek> HDRDecoder<R> {
    /// Creates a new decoder that decodes from the stream ```r```
    /// or returns error
    pub fn new(mut reader: R) -> ImageResult<HDRDecoder<R>> {  
        let r = &mut reader;
        let mut signature = [0; SIGNATURE_LENGTH];

        try!(r.read_exact(&mut signature));

        if signature != SIGNATURE {
            return Err(ImageError::FormatError("Radiance HDR signature not found".to_string()));
        } // no else

        // skip signature line ending
        try!(read_line_u8(r));

        let mut width = 0;
        let mut height = 0;
        // default values for attributes  
        let mut exposure = 1.;
        let mut color_correction = (1., 1., 1.);
        let mut software = "".into();
        let mut pixel_aspect_ratio = 1.;
        let mut primaries = ((0.640, 0.330), (0.290, 0.600), (0.150, 0.060), (0.333, 0.333));

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
                    }
                    let line = String::from_utf8_lossy(&line[..]);
                    match split_at_first(&line, "=") {
                        Some(("FORMAT", val)) => {
                            if val.trim() != "32bit_rle_rgbe" {
                                // XYZE isn't supported yet
                                return Err(ImageError::UnsupportedError(val.into()));
                            }
                        },
                        Some(("EXPOSURE", val)) => {
                            let multiplier = try!(val.trim().parse::<f32>().map_err(|pe|io::Error::new(io::ErrorKind::InvalidData, pe))); 
                            exposure *= multiplier;
                        },
                        Some(("SOFTWARE", val)) => {
                            software = val.into();
                        }
                        _ => {
                            // TODO: process other attributes
                        }
                    }
                },
            }
        }
        // TODO: Read width and height

        unimplemented!();
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

// Reads inpult until b"\n", b"\r\n", b"\r" or EOF
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
        // All files so far had "\n" line ending, but just to be sure
        // let's process common line endings
        if byte_buf[0] == b'\n' {
            // "\n" line ending
            return Ok(Some(ret));
        } else if byte_buf[0] == b'\r' { 
            let bytes_read = try!(r.read(byte_buf));
            if bytes_read == 0 {
                // "\r" line ending
                return Ok(Some(ret));
            } // no else
            if byte_buf[0] == b'\n' {
                // "\r\n" line ending
                return Ok(Some(ret));
            } else {
                // "\r" line ending
                // Unconsume byte
                try!(r.seek(SeekFrom::Current(-1)));
                return Ok(Some(ret));
            }
        } // end of EOL processing
        ret.push(byte_buf[0]);
    }
}

#[test]
fn read_line_u8_test() {
    let buf: Vec<_> = (&b"One\nTwo\r\nThree\rFour\n\n\r"[..]).into();
    let input = &mut ::std::io::Cursor::new(buf);
    assert_eq!(&read_line_u8(input).unwrap().unwrap()[..], &b"One"[..]);
    assert_eq!(&read_line_u8(input).unwrap().unwrap()[..], &b"Two"[..]);
    assert_eq!(&read_line_u8(input).unwrap().unwrap()[..], &b"Three"[..]);
    assert_eq!(&read_line_u8(input).unwrap().unwrap()[..], &b"Four"[..]);
    assert_eq!(&read_line_u8(input).unwrap().unwrap()[..], &b""[..]);
    assert_eq!(&read_line_u8(input).unwrap().unwrap()[..], &b""[..]);
    assert_eq!(read_line_u8(input).unwrap(), None);
}
