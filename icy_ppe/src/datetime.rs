use core::fmt;
#[derive(Default, Debug, Clone, PartialEq)]
pub struct IcbDate {
    month: u8,
    day: u8,
    year: u16,
}

const DAYS: [[i64; 12]; 2] = [
    [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334],
    [0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335],
];

impl IcbDate {
    pub fn new(month: u8, day: u8, year: u16) -> Self {
        Self { month, day, year }
    }

    pub fn get_month(&self) -> u8 {
        self.month
    }

    pub fn get_day(&self) -> u8 {
        self.day
    }

    pub fn get_year(&self) -> u16 {
        self.year
    }

    pub fn parse(str: &str) -> Self {
        let (month, day, year) = if str.len() == 8 {
            let month = str[0..2].parse::<i32>().unwrap_or_default();
            let day = str[2..4].parse::<i32>().unwrap_or_default();
            let year = str[4..].parse::<i32>().unwrap_or_default();
            (month, day, year)
        } else if str.len() == 6 {
            let month = str[0..2].parse::<i32>().unwrap_or_default();
            let day = str[2..4].parse::<i32>().unwrap_or_default();
            let year = 1900 + str[4..].parse::<i32>().unwrap_or_default();
            (month, day, year)
        } else {
            let parts = str
                .split(|c| c == '-' || c == '/' || c == '.' || c == ' ')
                .map(|c| c.parse::<i32>().unwrap_or_default())
                .collect::<Vec<i32>>();
            if parts.len() != 3 || parts[0] == 0 || parts[1] == 0 {
                return IcbDate::new(0, 0, 0);
            }
            let month = parts[0];
            let day = parts[1];
            let mut year = parts[2];
            if year < 100 {
                if year < 79 {
                    year += 2000;
                } else {
                    year += 1900;
                }
            }
            (month, day, year)
        };

        Self {
            month: month as u8,
            day: day as u8,
            year: year as u16,
        }
    }

    pub fn from_pcboard(jd: i32) -> Self {
        juilian_to_date(jd as i64)
    }

    pub fn to_pcboard_date(&self) -> i32 {
        let mut year = self.year as i64;
        // correct pcboard design decision
        if (1900..1979).contains(&year) {
            year = year.saturating_add(100);
        }

        let mut res = 36525 * year;
        if res % 100 == 0 && self.month < 3 {
            res = res.saturating_sub(1);
        }
        res = (res.saturating_sub(1900 * 36525)) / 100;
        res += self.day as i64 + DAYS[0][(self.month as usize).saturating_sub(1)];

        res as i32
    }

    pub fn to_julian_date(&self) -> u64 {
        let year = self.year as i64;
        let mut res = 36525 * year;
        if res % 100 == 0 && self.month < 3 {
            res -= 1;
        }
        res = (res - (1900 * 36525)) / 100;
        res += self.day as i64 + DAYS[0][self.month as usize - 1];

        res as u64
    }

    pub fn to_country_date(&self) -> String {
        self.to_string()
    }
}

impl fmt::Display for IcbDate {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:02}-{:02}-{:02}", self.month, self.day, self.year)
    }
}

fn juilian_to_date(jd: i64) -> IcbDate {
    if jd == 0 {
        return IcbDate::new(0, 0, 0);
    }

    let mut year = 100 * jd / 36525;
    let mut jd = jd - (year * 36525) / 100;

    let tmp = year * 36525;
    let day_table = if tmp % 100 == 0 && (year != 0 && year != 1900) {
        jd += 1;
        DAYS[1]
    } else {
        DAYS[0]
    };

    let mut month = 0;
    for (m, day) in day_table.iter().enumerate() {
        if *day < jd {
            month = m;
        } else {
            break;
        }
    }
    let day = jd - day_table[month];

    if year >= 100 {
        year -= 100;
    }

    IcbDate::new(month as u8 + 1, day as u8, year as u16)
}

#[test]
fn test_to_julian_date() {
    let date = IcbDate::parse("12-30-1976");
    assert_eq!(64648, date.to_pcboard_date());
}

#[test]
fn test_pcb_date() {
    let date = IcbDate::from_pcboard(64648);
    assert_eq!(format!("{date}"), "12-30-76");
}

#[test]
fn test_parse_date() {
    let date = IcbDate::parse("12-30-1976");
    assert_eq!(format!("{date}"), "12-30-1976");

    let date = IcbDate::parse("12/30/1976");
    assert_eq!(format!("{date}"), "12-30-1976");

    let date = IcbDate::parse("12301976");
    assert_eq!(format!("{date}"), "12-30-1976");
}
