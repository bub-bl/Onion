use palette::rgb::Rgba;
use palette::Hsla;

#[derive(Clone, PartialEq, Debug)]
pub enum Color {
    Hsla(Hsla),
    Rgba(Rgba),
    Hex(String),
}
