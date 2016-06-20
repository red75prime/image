use std::io::{Write, Result};
use color::{Rgb, FromColor};
use hdr::{SIGNATURE, RGBE8Pixel};

pub struct HDREncoder<W: Write> {
    w: W,
}

impl<W: Write> HDREncoder<W> {
    pub fn new(w: W) -> HDREncoder<W> {
        HDREncoder {
            w: w,
        }
    }

    pub fn encode<T>(mut self, data: &[T], width: usize, height: usize) -> Result<()> 
    where Rgb<f32>: FromColor<T> {
        assert!(data.len() >= width*height);
        let w = &mut self.w;
        try!(w.write_all(SIGNATURE));
        try!(w.write_all(b"\n"));
        try!(w.write_all(b"# Rust Radiance HDR encoder\n\n"));
        try!(w.write_all(format!("-Y {} +X {}", height, width).as_bytes()));
        for scanline in data.chunks(width) {
            for pix in scanline {
                 
            }
        }
        Ok(())
    }
}

pub fn to_rgbe8(pix: Rgb<f32>) -> RGBE8Pixel {
    let pix = pix.data;
    let mx = f32::max(pix[0], f32::max(pix[1], pix[2]));
    if mx <= 0. {
        RGBE8Pixel { c: [0, 0, 0], e: 0}
    } else {
        // let (frac, exp) = mx.frexp(); // unstable yet
        let exp = mx.log2().floor() as i32 + 1;
        let mul = f32::powi(2., exp);
        let mut conv = [0u8; 3];
        for (cv, &sv) in conv.iter_mut().zip(pix.iter()) {
            *cv = f32::trunc( sv / mul * 256. ) as u8;
        }
        RGBE8Pixel{ c: conv, e: (exp + 128) as u8}
    }
}

#[test]
fn to_rgbe8_test() {
    use hdr::{RGBE8Pixel, rgbe8};
    let test_cases = vec![rgbe8(0, 0, 0, 0), rgbe8(1, 1, 128, 128)];
    for &pix in &test_cases {
       assert_eq!(pix, to_rgbe8(pix.to_hdr())); 
    }
    for mc in 128..255 { // TODO: use inclusive range when stable
        let pix = rgbe8(mc, mc, mc, 100);
        assert_eq!(pix, to_rgbe8(pix.to_hdr())); 
        let pix = rgbe8(mc, 0, mc, 130);
        assert_eq!(pix, to_rgbe8(pix.to_hdr())); 
        let pix = rgbe8(0, 0, mc, 140);
        assert_eq!(pix, to_rgbe8(pix.to_hdr())); 
        let pix = rgbe8(1, 0, mc, 150);
        assert_eq!(pix, to_rgbe8(pix.to_hdr())); 
        let pix = rgbe8(1, mc, 10, 128);
        assert_eq!(pix, to_rgbe8(pix.to_hdr())); 
        for c in 0..255 {
            // Radiance HDR seems to be pre IEEE 754.
            // exponent can be -128 (represented as 0u8), so some colors cannot be represented in normalized f32
            // Let's exclude exponent value of -128 (0u8) from testing
            let pix = rgbe8(1, mc, c, if c == 0 { 1 } else { c });
            assert_eq!(pix, to_rgbe8(pix.to_hdr())); 
        }
    }
    fn relative_dist(a: Rgb<f32>, b: Rgb<f32>) -> f32 {
        // maximal difference divided by maximal value
        let max_diff = a.data.iter().zip(b.data.iter()).fold(0., |diff, (&a, &b)| f32::max(diff, (a-b).abs()));
        let max_val = a.data.iter().chain(b.data.iter()).fold(0., |maxv, &a| f32::max(maxv, a));
        if max_val == 0. { 0. } else { max_diff / max_val }
    }
   let test_values = vec![0.000_001, 0.000_02, 0.000_3, 0.004, 0.05, 0.6, 7., 80., 900., 1_000., 20_000., 300_000.];
    for &r in &test_values {
        for &g in &test_values {
            for &b in &test_values {
                let c1 = Rgb([r, g, b]);
                let c2 = to_rgbe8(c1).to_hdr();
                let rel_dist = relative_dist(c1, c2);
                // Maximal value is normalized to the range 128..256, thus we have 1/128 precision 
                assert!(rel_dist <= 1./128., "Relative distance ({}) exceeds 1/128 for {:?} and {:?}", rel_dist, c1 ,c2);
            }
        }
    }
}
