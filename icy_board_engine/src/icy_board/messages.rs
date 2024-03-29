use std::{
    fs,
    io::{Cursor, Read},
    str::FromStr,
};

use byteorder::{LittleEndian, ReadBytesExt};
use icy_ppe::{datetime::IcbDate, tables::CP437_TO_UNICODE, Res};

const FROM_TO_LEN: usize = 25;
const PASSWORD_LEN: usize = 12;
const DATE_LEN: usize = 8;
const TIME_LEN: usize = 5;

#[derive(Clone, Debug)]
pub struct MessageIndex {
    pub offset: u32,
    pub num: u32,
    pub to: String,
    pub from: String,
    pub status: u8,
    pub date: u16,
    pub reserved: [u8; 3],
}

fn convert_block(buf: &[u8]) -> String {
    let mut str = String::new();
    for c in buf {
        if *c == 0 {
            break;
        }
        str.push(CP437_TO_UNICODE[*c as usize]);
    }
    str
}
fn convert_str(buf: &[u8]) -> String {
    let mut str = convert_block(buf);
    while str.ends_with([' ']) {
        str.pop();
    }
    str
}

fn gen_string(str: &str, num: usize) -> Vec<u8> {
    let mut buf = Vec::new();
    for c in str.chars().take(num) {
        buf.push(c as u8);
    }
    while buf.len() < num {
        buf.push(b' ');
    }
    buf
}

impl MessageIndex {
    pub fn deserialize(cursor: &mut Cursor<&Vec<u8>>) -> Res<Self> {
        let offset = cursor.read_u32::<LittleEndian>()?;
        let num = cursor.read_u32::<LittleEndian>()?;

        let mut buf = [0; FROM_TO_LEN];
        cursor.read_exact(&mut buf)?;
        let to = convert_str(&buf);

        cursor.read_exact(&mut buf)?;

        let from = convert_str(&buf);

        let status = cursor.read_u8()?;

        let date = cursor.read_u16::<LittleEndian>()?;

        let mut reserved = [0; 3];
        cursor.read_exact(&mut reserved)?;

        Ok(Self {
            offset,
            num,
            to,
            from,
            status,
            date,
            reserved,
        })
    }

    pub fn load(file: &str) -> Res<Vec<Self>> {
        let buf = fs::read(file)?;
        let mut cursor = Cursor::new(&buf);
        let mut messages = Vec::new();
        while cursor.position() < cursor.get_ref().len() as u64 {
            messages.push(Self::deserialize(&mut cursor)?);
        }
        Ok(messages)
    }

    pub fn serialize(&self, buf: &mut Vec<u8>) {
        buf.extend(&self.offset.to_le_bytes());
        buf.extend(&self.num.to_le_bytes());

        buf.extend(&gen_string(&self.to, FROM_TO_LEN));
        buf.extend(&gen_string(&self.from, FROM_TO_LEN));
        buf.push(self.status);
        buf.extend(&self.date.to_le_bytes());
        buf.extend(&self.reserved);
    }
}

#[derive(Clone, Debug)]
pub struct MessageHeader {
    pub status: u8,
    pub msg_number: i32,
    pub ref_number: i32,
    pub num_blocks: u8,
    pub date: String,
    pub time: String,
    pub to_field: String,
    pub reply_date: IcbDate,
    pub reply_time: String,
    pub reply_status: u8,
    pub from_field: String,
    pub subj_field: String,
    pub password: String,
    pub active_flag: u8,
    pub echo_flag: u8,
    pub reserved: u32,
    pub extended_status: u8,
    pub net_tag: u8,
}
impl MessageHeader {
    pub fn deserialize(cursor: &mut Cursor<&Vec<u8>>) -> Res<Self> {
        let status = cursor.read_u8()?;
        let msg_number = convert_from_pcb_number(cursor.read_u32::<LittleEndian>()?);
        let ref_number = convert_from_pcb_number(cursor.read_u32::<LittleEndian>()?);
        let num_blocks = cursor.read_u8()?;

        let mut buf = [0; DATE_LEN];
        cursor.read_exact(&mut buf)?;
        let date = convert_str(&buf);
        let mut buf = [0; TIME_LEN];
        cursor.read_exact(&mut buf)?;
        let time = convert_str(&buf);
        let mut buf = [0; FROM_TO_LEN];
        cursor.read_exact(&mut buf)?;
        let to_field = convert_str(&buf);

        let reply_date =
            IcbDate::from_pcboard(convert_from_pcb_number(cursor.read_u32::<LittleEndian>()?));

        let mut buf = [0; TIME_LEN];
        cursor.read_exact(&mut buf)?;
        let reply_time = convert_str(&buf);

        let reply_status = cursor.read_u8()?;
        let mut buf = [0; FROM_TO_LEN];
        cursor.read_exact(&mut buf)?;
        let from_field = convert_str(&buf);
        let mut buf = [0; FROM_TO_LEN];
        cursor.read_exact(&mut buf)?;
        let subj_field = convert_str(&buf);
        let mut buf = [0; PASSWORD_LEN];
        cursor.read_exact(&mut buf)?;
        let password = convert_str(&buf);

        let active_flag = cursor.read_u8()?;
        let echo_flag = cursor.read_u8()?;
        let reserved = cursor.read_u32::<LittleEndian>()?;
        let extended_status = cursor.read_u8()?;
        let net_tag = cursor.read_u8()?;

        Ok(Self {
            status,
            msg_number,
            ref_number,
            num_blocks,
            date,
            time,
            to_field,
            reply_date,
            reply_time,
            reply_status,
            from_field,
            subj_field,
            password,
            active_flag,
            echo_flag,
            reserved,
            extended_status,
            net_tag,
        })
    }
}
pub struct Message {
    pub header: MessageHeader,
    pub extended_header: Vec<ExtendedHeader>,
    pub text: String,
}

impl Message {
    pub fn deserialize(cursor: &mut Cursor<&Vec<u8>>) -> Res<Message> {
        let mut text = String::new();

        let header = MessageHeader::deserialize(cursor).unwrap();

        let mut buf = vec![0; 128 * (header.num_blocks as usize - 1)];
        cursor.read_exact(&mut buf)?;
        let mut i = 0;

        let mut extended_header = Vec::new();
        while i < buf.len() {
            if buf[i] == 0xFF && buf[i + 1] == 0x40 {
                extended_header.push(ExtendedHeader::deserialize(&buf[i..])?);
                i += 0x48;
                continue;
            }
            text = Self::convert_msg(&buf[i..]);
            break;
        }

        Ok(Message {
            header,
            extended_header,
            text,
        })
    }
    fn convert_msg(buf: &[u8]) -> String {
        let mut str = String::new();
        for c in buf {
            if *c == 0 {
                break;
            }
            if *c == 0x0D || *c == 0xE3 {
                str.push('\n');
            } else {
                str.push(CP437_TO_UNICODE[*c as usize]);
            }
        }
        str
    }
}

#[derive(Debug)]
pub enum ExtendedHeaderInformation {
    To,
    From,
    Subject,
    Attach,
    List,
    Route,
    Origin,
    Reqrr,
    Ackrr,
    Ackname,
    Packout,
    To2,
    From2,
    Forward,
    Ufollow,
    Unewsgr,
}

impl FromStr for ExtendedHeaderInformation {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "TO" => Ok(Self::To),
            "FROM" => Ok(Self::From),
            "SUBJECT" => Ok(Self::Subject),
            "ATTACH" => Ok(Self::Attach),
            "LIST" => Ok(Self::List),
            "ROUTE" => Ok(Self::Route),
            "ORIGIN" => Ok(Self::Origin),
            "REQRR" => Ok(Self::Reqrr),
            "ACKRR" => Ok(Self::Ackrr),
            "ACKNAME" => Ok(Self::Ackname),
            "PACKOUT" => Ok(Self::Packout),
            "TO2" => Ok(Self::To2),
            "FROM2" => Ok(Self::From2),
            "FORWARD" => Ok(Self::Forward),
            "UFOLLOW" => Ok(Self::Ufollow),
            "UNEWSGR" => Ok(Self::Unewsgr),
            _ => Err(()),
        }
    }
}

impl ExtendedHeaderInformation {
    pub fn to_str(&self) -> &'static str {
        match self {
            Self::To => "TO     ",
            Self::From => "FROM   ",
            Self::Subject => "SUBJECT",
            Self::Attach => "ATTACH ",
            Self::List => "LIST   ",
            Self::Route => "ROUTE  ",
            Self::Origin => "ORIGIN ",
            Self::Reqrr => "REQRR  ",
            Self::Ackrr => "ACKRR  ",
            Self::Ackname => "ACKNAME",
            Self::Packout => "PACKOUT",
            Self::To2 => "TO2    ",
            Self::From2 => "FROM2  ",
            Self::Forward => "FORWARD",
            Self::Ufollow => "UFOLLOW",
            Self::Unewsgr => "UNEWSGR",
        }
    }
}

pub struct ExtendedHeader {
    pub info: ExtendedHeaderInformation,
    pub content: String,
    pub status: u8,
}

impl ExtendedHeader {
    // const ID:u16 = 0x40FF;
    const FUNC_LEN: usize = 7;
    const DESC_LEN: usize = 60;

    pub fn deserialize(buf: &[u8]) -> Res<Self> {
        // let _id = u16::from_le_bytes([buf[0], buf[1]]);
        let mut i = 2;
        let function =
            ExtendedHeaderInformation::from_str(&convert_str(&buf[i..i + Self::FUNC_LEN])).unwrap();
        i += Self::FUNC_LEN + 1; // skip ':'

        let content = convert_str(&buf[i..i + Self::DESC_LEN]);
        i += Self::DESC_LEN;

        let status = buf[i];
        Ok(Self {
            info: function,
            content,
            status,
        })
    }
}

fn convert_from_pcb_number(n: u32) -> i32 {
    let index = (n >> 24) as u8;
    if index == 0 || index == 0x80 {
        return 0;
    }

    let is_neg = (n & 0x80_0000) != 0;
    let low_number = (n & 0x7F_FFFF) as i32 | 0x80_0000;
    let shift = 24 - index.wrapping_sub(0x80);
    let result = low_number >> shift;

    if is_neg {
        return -result;
    }
    result
}
