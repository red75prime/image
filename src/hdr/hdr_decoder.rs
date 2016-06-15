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
#[derive(Debug)]
pub struct HDRDecoder<R> {
    r: R,

    width: u32,
    height: u32,
    exposure: f32,
    color_correction: (f32,f32,f32),
    software: String,
    pixel_aspect_ratio: f32,
    custom_attributes: Vec<(String,String)>, // key, value
}

#[repr(C)] #[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct RGBE8Pixel {
    pub r: u8,
    pub g: u8,
    pub b: u8,
    pub e: u8,
}

impl<R: Read + Seek> HDRDecoder<R> {

    /// Reads Radiance HDR image header from stream ```r```
    /// if the header is valid, creates HDRDecoder 
    /// strict mode is enabled
    pub fn new(reader: R) -> ImageResult<HDRDecoder<R>> {
        HDRDecoder::with_strictness(reader, true)
    }    

    /// Reads Radiance HDR image header from stream ```r```
    /// if the header is valid, creates HDRDecoder 
    /// strict enables strict mode
    /// Warning! Reading wrong file in non-strict mode
    ///   could consume file size worth of memory in the process.
    ///   Memory will be freed on decoder drop.
    pub fn with_strictness(mut reader: R, strict: bool) -> ImageResult<HDRDecoder<R>> {  
        
        let mut attributes = HeaderInfo::new();  

        { // scope to make borrowck happy
            let r = &mut reader;
            if strict {
                let mut signature = [0; SIGNATURE_LENGTH];
                try!(r.read_exact(&mut signature));
                if signature != SIGNATURE {
                    return Err(ImageError::FormatError("Radiance HDR signature not found".to_string()));
                } // no else
                // skip signature line ending
                try!(read_line_u8(r));
            } else {
                // Old Radiance HDR files (*.pic) don't use signature
                // Let them be parsed in non-strict mode
            }
            // read header data until empty line
            loop {
                match try!(read_line_u8(r)) {
                    None => { // EOF before end of header
                        return Err(ImageError::FormatError("EOF in header".into()));
                    },
                    Some(line) => {
                        if line.len() == 0 {
                            // end of header
                            break; 
                        } else if line[0] == b'#' { // line[0] will not panic, line.len() == 0 is false here  
                            // skip comments
                            continue;
                        } // no else
                        // process attribute line
                        let line = String::from_utf8_lossy(&line[..]);
                        try!(attributes.update_header_info(&line, strict));
                    }, // <= Some(line)
                } // match read_line_u8()
            } // loop
        } // scope to end borrow of reader   
        // parse dimensions
        let (width, height) =
            match try!(read_line_u8(&mut reader)) {
                None => {
                    // EOF instead of image dimensions
                    return Err(ImageError::FormatError("EOF in dimensions line".into()));
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
            exposure: attributes.exposure.unwrap_or(1.),
            color_correction: attributes.color_correction.unwrap_or((1., 1., 1.)),
            software: attributes.software.unwrap_or("".into()),
            pixel_aspect_ratio: attributes.pixel_aspect_ratio.unwrap_or(1.),
            custom_attributes: attributes.custom_attributes,
        })
    } // end with_strictness

    /// Values of pixels should be divided by this to get physical radiance in watts/steradian/m^2
    pub fn exposure(&self) -> f32 {
        self.exposure
    } 

    /// Correction for R, G, B channels. Pixel color values should be divided by corresponing value
    pub fn color_correction(&self) -> (f32, f32, f32) {
        self.color_correction
    }

    /// Software used to create the picture
    /// Not useful. All hdr files I've seen use comments to indentify software
    pub fn software(&self) -> &String {
        &self.software
    }

    /// Height of pixel divided by width
    pub fn pixel_aspect_ratio(&self) -> f32 {
        self.pixel_aspect_ratio
    }

    pub fn custom_attributes(&self) -> &[(String, String)] {
        self.custom_attributes.as_slice()
    }

    /// Consumes decoder and returns a vector of decompressed RGBE8 pixels
    pub fn read_image_native(mut self) -> ImageResult<Vec<RGBE8Pixel>> {
        // Don't read anything if image is empty 
        if self.width == 0 || self.height ==0 {
            return Ok(vec![]);
        }
        // expression self.width > 0 && self.height > 0 is true from now to the end of this method
        let pixel_count = self.width as usize * self.height as usize;
        let mut ret = Vec::<RGBE8Pixel>::with_capacity(pixel_count);
        unsafe {
            // RGBE8Pixel doesn't implement Drop, so it's Ok to drop half-initialized ret
            ret.set_len(pixel_count);
        } // ret contains uninitialized data, so now it's my responsibility to return fully initialized ret
        for y in 0 .. self.height as usize {
            // first 4 bytes in scanline allow to determine compression method
            let y_off = y * self.width as usize;
            try!(read_scanline(&mut self.r, &mut ret[y_off .. y_off + self.width as usize]));
        }
        Ok(ret)
    }
}

impl<R> ImageDecoder for HDRDecoder<R> {
    fn dimensions(&mut self) -> ImageResult<(u32, u32)> {
        Ok((self.width, self.height))
    }

    fn colortype(&mut self) -> ImageResult<ColorType> {
        Ok(ColorType::RGBE(8))
    }

    fn row_len(&mut self) -> ImageResult<usize> {
        Ok(4*(self.width as usize))
    }

    fn read_scanline(&mut self, buf: &mut [u8]) -> ImageResult<u32> {
        unimplemented!()
    }

    fn read_image(&mut self) -> ImageResult<DecodingResult> {
        unimplemented!()
    }
}

impl<R: Read + Seek> IntoIterator for HDRDecoder<R> {
    type Item = ImageResult<RGBE8Pixel>;
    type IntoIter = HDRImageDecoderIterator<R>;

    fn into_iter(self) -> Self::IntoIter {
        // scanline buffer
        let mut buf = Vec::with_capacity(self.width as usize);
        unsafe {
            // dropping half-initialized vector of RGBE8Pixel is safe
            // and I took care to hide half-initialized vector from a user
            buf.set_len(self.width as usize);
        }
        HDRImageDecoderIterator {
            r: self.r,
            scanline_cnt: self.height as usize,
            buf: buf,
            col: 0,
            scanline: 0,
            error_encountered: false,
        }
    }
}

pub struct HDRImageDecoderIterator<R: Read> {
    r: R,
    scanline_cnt: usize,
    buf: Vec<RGBE8Pixel>, // scanline buffer
    col: usize, // current position in scanline
    scanline: usize, // current scanline
    error_encountered: bool,
}

impl<R: Read> HDRImageDecoderIterator<R> {
    // Advances counter to the next pixel
    #[inline]
    fn advance(&mut self) {
        self.col += 1;
        if self.col == self.buf.len() {
            self.col = 0;
            self.scanline += 1;
        }
    }
}

impl<R: Read> Iterator for HDRImageDecoderIterator<R> {
    type Item = ImageResult<RGBE8Pixel>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.buf.len() == 0 || self.scanline == self.scanline_cnt {
            // No more pixels
            return None;
        } // no else
        if self.error_encountered {
            self.advance();
            // Error was encountered. Keep producing errors.
            // ImageError can't implement Clone, so just dump some error
            return Some(Err(ImageError::ImageEnd));
        } // no else
        if self.col == 0 {
            // fill scanline buffer
            match read_scanline(&mut self.r, &mut self.buf[..]) {
                Ok(_) => {},
                Err(err) => {
                    self.error_encountered = true;
                    self.advance();
                    return Some(Err(err));
                }
            }
        } // no else
        let ret = self.buf[self.col];
        self.advance();
        Some(Ok(ret))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let total_cnt = self.buf.len() * self.scanline_cnt;
        let cur_cnt = self.buf.len() * self.scanline + self.col;
        let remaining = total_cnt - cur_cnt;
        (remaining, Some(remaining))
    }
}

impl<R: Read> ExactSizeIterator for HDRImageDecoderIterator<R> {} 

// Precondition: buf.len() > 0
fn read_scanline<R: Read>(r: &mut R, buf: &mut [RGBE8Pixel]) -> ImageResult<()> {
    assert!(buf.len()>0);
    let width = buf.len();
    // first 4 bytes in scanline allow to determine compression method
    let fb = try!(read_rgbe(r)); 
    if fb.r == 2 && fb.g == 2 && fb.b < 128 {
        // denormalized pixel value (2,2,<128,_) indicates new per component RLE method
        // decode_component guaranties that offset is within 0 .. width
        // therefore we can skip bounds checking here, but we will not
        try!(decode_component(r, width, |offset, value| buf[offset].r = value ));
        try!(decode_component(r, width, |offset, value| buf[offset].g = value ));
        try!(decode_component(r, width, |offset, value| buf[offset].b = value ));
        try!(decode_component(r, width, |offset, value| buf[offset].e = value ));
    } else { 
        // old RLE method (it was considered old around 1991, should it be here?)
        try!(decode_old_rle(r, fb, buf));
    }
    Ok(())
}

#[inline(always)]
fn read_byte<R: Read>(r: &mut R) -> io::Result<u8> {
    let mut buf = [0u8];
    try!(r.read_exact(&mut buf[..]));
    Ok(buf[0])
}

// Guaranties that first parameter of set_component will be within pos .. pos+width
#[inline]
fn decode_component<R: Read, S: FnMut(usize, u8)>(r: &mut R, width: usize, mut set_component: S) -> ImageResult<()> {
    let mut buf = [0; 128];
    let mut pos = 0;
    while pos < width {
        // increment position by a number of decompressed values
        pos += {
            let rl = try!(read_byte(r));
            if rl <= 128 {
                // sanity check
                if pos + rl as usize > width {
                    return Err(ImageError::FormatError("Wrong length of decoded scanline".into()));
                }
                // read values
                try!(r.read_exact(&mut buf[0..rl as usize]));
                for (offset, &value) in buf[0..rl as usize].iter().enumerate() {
                    set_component(pos + offset, value);
                };
                rl as usize
            } else {
                // run
                let rl = rl - 128;
                // sanity check
                if pos + rl as usize > width {
                    return Err(ImageError::FormatError("Wrong length of decoded scanline".into()));
                }
                // fill with same value
                let value = try!(read_byte(r));
                for offset in 0..rl as usize {
                    set_component(pos + offset, value);
                };
                rl as usize
            }
        };
    }
    if pos != width {
        return Err(ImageError::FormatError("Wrong length of decoded scanline".into()));
    }
    Ok(())
}

// Decodes scanline, places it into buf
// Precondition: buf.len() > 0
// fb - first 4 bytes of scanline
fn decode_old_rle<R: Read>(r: &mut R, fb: RGBE8Pixel, buf: &mut [RGBE8Pixel]) -> ImageResult<()> {
    assert!(buf.len() > 0);
    let width = buf.len();
    // convenience function. 
    // returns run length if pixel is a run length marker
    #[inline]  
    fn rl_marker(pix : RGBE8Pixel) -> Option<usize> {
        if pix.r == 1 && pix.g == 1 && pix.b == 1 {
            Some(pix.e as usize)
        } else {
            None
        }
    }
    // first pixel in scanline should not be run length marker
    // it is error if it is
    if let Some(_) = rl_marker(fb) {
        return Err(ImageError::FormatError("First pixel of a scanline shouldn't be run length marker".into()));
    } 
    buf[0] = fb; // set first pixel of scanline

    let mut x_off = 1; // current offset from beginning of a scanline
    let mut rl_mult = 1; // current run length multiplier
    let mut prev_pixel = fb;
    while x_off < width {
        let pix = try!(read_rgbe(r));
        // it's harder to forget to increase x_off if I write this this way.
        x_off += {
            if let Some(rl) = rl_marker(pix) {
                // rl_mult takes care of consecutive RL markers
                let rl = rl * rl_mult; 
                rl_mult *= 256;
                if x_off + rl <= width {
                    // do run
                    for x in x_off .. x_off + rl {
                        buf[x] = prev_pixel;
                    } 
                } else {
                    return Err(ImageError::FormatError("Wrong length of decoded scanline".into()));
                };
                rl // value to increase x_off by
            } else {
                rl_mult = 1; // chain of consecutive RL markers is broken
                prev_pixel = pix;
                buf[x_off] = pix;
                1 // value to increase x_off by
            }
        };
    }
    if x_off != width {
        return Err(ImageError::FormatError("Wrong length of decoded scanline".into()));
    }
    Ok(())
}

fn read_rgbe<R: Read>(r: &mut R) -> io::Result<RGBE8Pixel> {
    let mut buf = [0u8; 4];
    try!(r.read_exact(&mut buf[..]));
    // It's actually safe: RGBE8Pixel is repr(C) and it doesn't implement Drop
    Ok(unsafe{ ::std::mem::transmute(buf) })
}

#[derive(Debug)]
struct HeaderInfo {
    exposure: Option<f32>,
    color_correction: Option<(f32,f32,f32)>,
    software: Option<String>,
    pixel_aspect_ratio: Option<f32>,
    //primaries: Option<((f32, f32), (f32, f32), (f32, f32), (f32, f32))>,
    gamma: Option<f32>,
    custom_attributes: Vec<(String, String)>,    
}

impl HeaderInfo {
    fn new() -> HeaderInfo {
        HeaderInfo {
            exposure: None,
            color_correction: None,
            software: None,
            pixel_aspect_ratio: None,
            //primaries: None,
            gamma: None,
            custom_attributes: vec![],
        }
    }

    // Updates header info, in strict mode returns error for malformed lines (no '=' separator)
    // unknown attributes are skipped
    fn update_header_info<'a>(&mut self, line: &Cow<'a, str>, strict: bool) -> ImageResult<()> {
        // split line at first '=' 
        // old Radiance HDR files (*.pic) feature tabs in key, so                vvv trim
        let maybe_key_value = split_at_first(&line, "=").map(|(key, value)| (key.trim(), value));
        // save all header lines in custom_attributes
        match maybe_key_value {
            Some((key, val)) => self.custom_attributes.push((key.to_owned(), val.to_owned())),
            None             => self.custom_attributes.push(("".into(), line.clone().into_owned())),
        }
        // parse known attributes
        match maybe_key_value {
            Some(("FORMAT", val)) => {
                if val.trim() != "32-bit_rle_rgbe" {
                    // XYZE isn't supported yet
                    return Err(ImageError::UnsupportedError(limit_string_len(val, 20)));
                }
            },
            Some(("EXPOSURE", val)) => {
                match val.trim().parse::<f32>() {
                    Ok(v) => {
                        self.exposure = Some(self.exposure.unwrap_or(1.)*v); // all encountered exposure values should be multplied 
                    },
                    Err(parse_error) => {
                        if strict {
                            return Err(ImageError::FormatError(format!("Cannot parse EXPOSURE value: {}", parse_error.description())));
                        } // no else, skip this line in non-strict mode
                    },
                };
            },
            Some(("PIXASPECT", val)) => {
                match val.trim().parse::<f32>() {
                    Ok(v) => {
                        self.pixel_aspect_ratio = Some(self.pixel_aspect_ratio.unwrap_or(1.)*v); // all encountered exposure values should be multplied 
                    },
                    Err(parse_error) => {
                        if strict {
                            return Err(ImageError::FormatError(format!("Cannot parse PIXASPECT value: {}", parse_error.description())));
                        } // no else, skip this line in non-strict mode
                    },
                };
            },
            Some(("GAMMA", val)) => {
                match val.trim().parse::<f32>() {
                    Ok(v) => {
                        self.gamma = Some(v);  
                    },
                    Err(parse_error) => {
                        if strict {
                            return Err(ImageError::FormatError(format!("Cannot parse GAMMA value: {}", parse_error.description())));
                        } // no else, skip this line in non-strict mode
                    },
                };
            },
            Some(("SOFTWARE", val)) => {
                self.software = Some(val.into());
            },
            Some(("COLORCORR", val)) => {
                let mut rgbcorr = [1., 1., 1.];
                match parse_space_separated_f32(val, &mut rgbcorr, "COLORCORR") {
                    Ok(extra_numbers) => {
                        if strict && extra_numbers {
                            return Err(ImageError::FormatError("Extra numbers in COLORCORR".into()));
                        } // no else, just ignore extra numbers
                        let (rc, gc, bc) = self.color_correction.unwrap_or((1., 1., 1.));
                        self.color_correction = Some((rc*rgbcorr[0], gc*rgbcorr[1], bc*rgbcorr[2]));
                    },
                    Err(err) => {
                        if strict {
                            return Err(err);
                        } // no else, skip malformed line in non-strict mode 
                    },
                }
            },
            None => {
                    // skip malformed attribute
                    // old Radiance HDR files (*.pic) contain commands in a header
            },
            _ => {
                // skip unknown attribute
            },
        } // match attributes
        Ok(())
    }
}

fn parse_space_separated_f32(line: &str, vals: &mut [f32], name: &str) -> ImageResult<bool> {
    let mut nums = line.split_whitespace();
    for val in vals.iter_mut() {
        if let Some(num) = nums.next() {
            match num.parse::<f32>() {
                Ok(v) => *val = v,
                Err(err) => {
                    return Err(ImageError::FormatError(format!("f32 parse error in {}: {}", name, err.description())));
                }
            }
        } else {
            // not enough numbers in line
            return Err(ImageError::FormatError(format!("Not enough numbers in {}", name)));
        }
    }
    Ok(nums.next().is_some())
}

// Parses dimension line "-Y height +X width"
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
            // c1_str is height, c2_str is width
            let height = try!(c1_str.parse::<u32>().into_image_error(err));
            let width = try!(c2_str.parse::<u32>().into_image_error(err));
            Ok((width, height))
        },
        _ => {
            Err(ImageError::FormatError(
                    format!("Unsupported orientation {} {}", 
                        limit_string_len(c1_tag, 4), 
                        limit_string_len(c2_tag, 4))))
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


// Returns string with no more than len+3 characters
fn limit_string_len(s: &str, len: usize) -> String {
    let s_char_len = s.chars().count();
    if s_char_len > len {
        s.chars().take(len).chain("...".chars()).collect()
    } else {
        s.into()
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

    loop {
        // read_byte uses Read::read_exact, so I don't need to bother myself about EINTR
        match read_byte(r) {
            Ok(byte) => {
                no_data = false;
                // HDR format doesn't specify encoding of end-of-line
                // C implementation uses fgets
                // Let's assume it is '\n'
                if byte == b'\n' {
                    // "\n" line ending
                    return Ok(Some(ret));
                } // end of EOL processing
                ret.push(byte);
            },
            Err(ref err) if err.kind() == io::ErrorKind::UnexpectedEof => {
                // EOF
                return if no_data { Ok(None) } else { Ok(Some(ret)) };
            },
            Err(err) => return Err(err), // report all other errors
        }
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
