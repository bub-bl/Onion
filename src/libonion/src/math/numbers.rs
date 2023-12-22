use std::str::FromStr;

#[derive(Clone, PartialEq, Debug)]
pub enum Number {
    UnsignedInteger(u64),
    SignedInteger(i64),
    Float(f64),
}

impl FromStr for Number {
    type Err = ();

    // Try to parse number to unsigned integer, signed integer or float
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.parse::<u64>() {
            Ok(u) => Ok(Number::UnsignedInteger(u)),
            Err(_) => match s.parse::<i64>() {
                Ok(i) => Ok(Number::SignedInteger(i)),
                Err(_) => match s.parse::<f64>() {
                    Ok(f) => Ok(Number::Float(f)),
                    Err(_) => Err(()),
                },
            },
        }
    }
}
