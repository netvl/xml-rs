use std::io;
use std::borrow::Cow;
use std::result::Result as StdResult;

pub type Result<T> = StdResult<T, Error>;

#[derive(Fail, Debug)]
pub enum Error {
    #[fail(display = "I/O error")]
    Io {
        #[cause]
        cause: io::Error,
    },
    #[fail(display = "Parsing error: {}", reason)]
    Parsing {
        reason: Cow<'static, str>,
    },
}
