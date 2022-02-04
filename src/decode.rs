use std::intrinsics::rotate_right;



pub fn decode(block: &mut [u8]) {
    let mut xor_value: u16 = 0xdb24;
    let mut j: u16 = (block.len() / 2).try_into().unwrap();
    let mut i = 0;
    let mut rotate_count: u8 = 0;

    while j > 0 {
        let cur_word: u16 = (block[i + 1] as u16) << 8 | block[i] as u16;
        let pos_xor: u16 = j;
        rotate_count = (xor_value + pos_xor) as u8;
        let outx = (rotate_right(cur_word, rotate_count.into()) as u16) ^ xor_value;
        block[i] = (outx ^ pos_xor) as u8;
        i += 1;
        block[i] = (outx >> 8 ^ pos_xor) as u8;
        i += 1;
        xor_value = cur_word;
        j -= 1;
    }

    if block.len() % 2 == 1 {
        block[i] = rotate_right(block[i] ^ (xor_value as u8), rotate_count);
    }
}

#[cfg(test)]
mod tests {
    use super::decode;

    #[test]
    fn test_decode_simple() {
        let mut buffer: [u8; 11] = [188, 113, 184, 117, 181, 219, 236, 219, 189, 187, 189];
        decode(&mut buffer);
        let expected: [u8; 11] = [25, 0, 0, 0, 0, 0, 0, 0, 0, 7, 0];
        assert_eq!(buffer, expected);
    }

}
