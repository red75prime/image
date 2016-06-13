//!  Decoding of Radiance HDR Images
//!
//!  A decoder for Radiance HDR images
//!
//!  # Related Links
//!  * http://radsite.lbl.gov/radiance/refer/filefmts.pdf
//!  * http://www.cs.virginia.edu/~jcw5q/apps/imageview/src/libimageviewer/hdr.c
//!

pub use self::hdr_decoder::HDRDecoder;

mod hdr_decoder;
