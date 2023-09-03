#![macro_use]

//! Contains several macros used in this crate.

macro_rules! gen_setter {
    ($(#[$comments:meta])* $target:ty, $field:ident : into $t:ty) => {
        impl $target {
            $(#[$comments])*
            ///
            /// See [`ParserConfig`][crate::ParserConfig] fields docs for details
            #[inline]
            pub fn $field<T: Into<$t>>(mut self, value: T) -> $target {
                self.$field = value.into();
                self
            }
        }
    };
    ($(#[$comments:meta])* $target:ty, $field:ident : val $t:ty) => {
        impl $target {
            $(#[$comments])*
            ///
            /// See [`ParserConfig`][crate::ParserConfig] fields docs for details
            #[inline]
            pub fn $field(mut self, value: $t) -> $target {
                self.$field = value;
                self
            }
        }
    };
    ($(#[$comments:meta])* $target:ty, $field:ident : delegate $t:ty) => {
        impl $target {
            $(#[$comments])*
            ///
            /// See [`ParserConfig`][crate::ParserConfig] fields docs for details
            #[inline]
            pub fn $field(mut self, value: $t) -> $target {
                self.c.$field = value;
                self
            }
        }
    };
    ($(#[$comments:meta])* $target:ty, $field:ident : c2 $t:ty) => {
        impl $target {
            $(#[$comments])*
            ///
            /// See [`ParserConfig2`][crate::reader::ParserConfig] fields docs for details
            #[inline]
            #[must_use]
            pub fn $field(self, value: $t) -> ParserConfig2 {
                ParserConfig2 {
                    c: self,
                    ..Default::default()
                }
                .$field(value)
            }
        }
    };
}

macro_rules! gen_setters {
    ($target:ty, $($(#[$comments:meta])* $field:ident : $k:tt $tpe:ty),+) => ($(
        gen_setter! { $(#[$comments])* $target, $field : $k $tpe }
    )+)
}
