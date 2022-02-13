use std::intrinsics::rotate_right;

const CRYPT_DATA: [u8; 17] = [0x8C, 0x53, 0xB8, 0xA7, 0x9E, 0x0F, 0x0A, 0xCB, 0x28, 0x62, 0x2D, 0x50, 0x7E, 0x05, 0x3D, 0x4E, 0x35];

fn crypt3(block: &mut [u8])
{
    let mut len = block.len();
    let mut i = 0;
    let mut j = 0;
    while len > 0 {
        block[i] ^= CRYPT_DATA[j].wrapping_add(len as u8);
        j = (j + 1) % CRYPT_DATA.len();
        len -= 1;
        i += 1;
    }
}

pub fn decrypt(block: &mut [u8], version: u16) {
    if version < 300 { return; }
    if version >= 330 { crypt3(block); }

    let mut full_size = block.len() as i32;
    let mut i = 0;
    loop {
        let size;
        if full_size < 0x0800 {
            size = full_size;
            full_size = 0;
        } else {
            size = 0x7ff;
            full_size -= 0x7ff;
        }
        let mut xor_value = 0xdb24;
        let mut rotate_count = 0;
        let mut dx = size >> 1;
        while dx > 0 {
            let cur_word = (block[i + 1] as u16) << 8 | block[i] as u16;
            let dl = dx as u8;
            rotate_count = ((xor_value & 0xFF) + (dl as u16)) & 0xFF;
            let outx = rotate_right(cur_word, rotate_count.into()) ^ xor_value;
            block[i] = (outx as u8) ^ dl;
            i += 1;
            block[i] = (outx >> 8) as u8 ^ dl;
            i += 1;
            xor_value = cur_word;
            dx -= 1;
        }

        if size % 2 == 1 {
            block[i] = rotate_right(block[i] ^ (xor_value as u8), rotate_count as u8);
            i += 1;
        }
        if size == 0x7ff && block[i - 1] == 0 {
            i += 1;
            full_size -= 1;
        }
        if full_size <= 0 { break; }
    }
}


pub fn encode_rle(src: &[u8]) -> Vec<u8> {
    let mut result = Vec::new();
    let mut i = 0;
    while i < src.len() {
        let cur = src[i];
        i += 1;
        if cur == 0 {
            let mut count = 1;
            if i < src.len() && count < 255 && src[i] == 0 {
                i += 1;
                count += 1;
            }
            result.push(0);
            result.push(count as u8);
        } else {
            result.push(cur);
        }
    }
    result
}

pub fn decode_rle(src: &[u8]) -> Vec<u8> {
    let mut result = Vec::new();
    let mut i = 0;
    while i < src.len() {
        let cur = src[i];
        i += 1;

        result.push(cur);
        if cur == 0 {
            if i >= src.len() {
                break;
            }
            let count = src[i];
            i += 1;
            for _ in 1..count {
                result.push(0);
            }
        }
    }
    result
}

#[cfg(test)]
mod tests {
    use crate::decode::{decode_rle, encode_rle};
    use super::*;

    #[test]
    fn test_cypt3_simple() {
        let o_buffer = b"Hello World here I'am.";
        let mut buffer = o_buffer.clone();
        crypt3(&mut buffer);
        assert_ne!(buffer, *o_buffer);
        crypt3(&mut buffer);
        assert_eq!(buffer, *o_buffer);
    }

    #[test]
    fn test_decrypt_simple() {
        let mut buffer: [u8; 11] = [188, 113, 184, 117, 181, 219, 236, 219, 189, 187, 189];
        decrypt(&mut buffer, 300);
        let expected: [u8; 11] = [25, 0, 0, 0, 0, 0, 0, 0, 0, 7, 0];
        assert_eq!(buffer, expected);
    }

    fn test_rle(data: &[u8]) {
        let encoded = encode_rle(data);
        let res = decode_rle(&encoded);
        assert_ne!(encoded.len(), res.len(), "array length should differ."); // assert there are some 0 in the input data.
        assert_eq!(res.len(), data.len(), "array size mismatch.");
        for i in 0..data.len() {
            assert_eq!(res[i], data[i], "data mismatch.");
        }
    }

    #[test]
    fn test_revert_rle() {
        test_rle(&[0, 1, 2, 3, 4, 5]);
        test_rle(&[0, 1, 2, 0, 3, 4, 5, 0, 0]);
        test_rle(&[0, 0, 0, 0, 0, 0, 0, 0, 0]);
        test_rle(&[1, 2, 3, 4, 0, 6, 7, 0, 9]);
    }
}
