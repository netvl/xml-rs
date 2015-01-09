#![macro_use]

//! Contains several macros used in this crate.

macro_rules! for_each(
    ($e:ident in $it:expr ; $body:expr) => (
        loop {
            match $it {
                Some($e) => $body,
                None => break
            }
        }
    )
);

macro_rules! gen_setters(
    ($target:ty, $($field:ident : $t:ty),+) => ($(
        impl $target {
            /// Sets the field to the provided value and returns updated config object.
            pub fn $field(mut self, value: $t) -> $target {
                self.$field = value;
                self
            }
        }
    )+)
);
