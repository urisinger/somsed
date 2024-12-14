use iced::{
    widget::{text, Text},
    Font,
};

pub fn error<'a>() -> Text<'a> {
    with_codepoint('\u{e241}')
}

fn with_codepoint<'a>(codepoint: char) -> Text<'a> {
    const FONT: Font = Font::with_name("dcg-icons-2024-08-02");

    text(codepoint).font(FONT)
}
