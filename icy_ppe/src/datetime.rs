const DAYS: [[u16; 12]; 2] = [
    [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334],
    [0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335],
];

pub fn juilian_to_date(jd: u16) -> String {
    if jd == 0 {
        return "00-00-00".to_string();
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
            break;
        }
    }
    let day = jd - day_table[month];

    if year >= 100 {
        year -= 100;
    }

    format!("{:02}-{:02}-{:02}", month + 1, day, year)
}
