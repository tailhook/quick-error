//! A macro which makes errors easy to write
//!
//! Minimum type is like this:
//!
//! ```rust
//! #[macro_use] extern crate quick_error;
//! # fn main() {}
//!
//! quick_error! {
//!     #[derive(Debug)]
//!     pub enum SomeError {
//!         Variant1 {}
//!     }
//! }
//! ```
//! Both ``pub`` and non-public types may be declared, and all meta attributes
//! (such as ``#[derive(Debug)]``) are forwarded as is. The `Debug` must be
//! implemented (but you may do that yourself if you like). The documentation
//! comments ``/// something`` (as well as other meta attrbiutes) on variants
//! are allowed.
//!
//! You may add arbitrary parameters to any struct variant:
//!
//! ```rust
//! # #[macro_use] extern crate quick_error;
//! # fn main() {}
//! #
//! quick_error! {
//!     #[derive(Debug)]
//!     pub enum SomeError {
//!         /// IO Error
//!         Io(err: std::io::Error) {}
//!         /// Utf8 Error
//!         Utf8(err: std::str::Utf8Error) {}
//!     }
//! }
//! ```
//!
//! Note unlike in normal Enum decarations you declare names of fields (which
//! are omitted from type). How they can be used is outlined below.
//!
//! Now you might have noticed trailing braces `{}`. They are used to define
//! implementations. By default:
//!
//! * `Error::description()` returns variant name as static string
//! * `Error::cause()` returns None (even if type wraps some value)
//! * `Display` outputs `description()`
//! * No `From` implementations are defined
//!
//! To define description simply add `description(value)` inside braces:
//!
//! ```rust
//! # #[macro_use] extern crate quick_error;
//! # fn main() {}
//! #
//! quick_error! {
//!     #[derive(Debug)]
//!     pub enum SomeError {
//!         Io(err: std::io::Error) {
//!             description(err.description())
//!         }
//!         Utf8(err: std::str::Utf8Error) {
//!             description("utf8 error")
//!         }
//!     }
//! }
//! ```
//!
//! Normal rules for borrowing apply. So most of the time description either
//! returns constant string or forwards description from enclosed type.
//!
//! To change `cause` method to return some error, add `cause(value)`, for
//! example:
//!
//! ```rust
//! # #[macro_use] extern crate quick_error;
//! # fn main() {}
//! #
//! quick_error! {
//!     #[derive(Debug)]
//!     pub enum SomeError {
//!         Io(err: std::io::Error) {
//!             cause(err)
//!             description(err.description())
//!         }
//!         Utf8(err: std::str::Utf8Error) {
//!             description("utf8 error")
//!         }
//!     }
//! }
//! ```
//! Note you don't need to wrap value in `Some`, its implicit. In case you want
//! `None` returned just omit the `cause`. You can't return `None`
//! conditionally.
//!
//! To change how each clause is `Display`ed add `display(pattern,..args)`,
//! for example:
//!
//! ```rust
//! # #[macro_use] extern crate quick_error;
//! # fn main() {}
//! #
//! quick_error! {
//!     #[derive(Debug)]
//!     pub enum SomeError {
//!         Io(err: std::io::Error) {
//!             display("I/O error: {}", err)
//!         }
//!         Utf8(err: std::str::Utf8Error) {
//!             display("Utf8 error, valid up to {}", err.valid_up_to())
//!         }
//!     }
//! }
//! ```
//!
//! If you need a reference to the error when `Display`ing, you can instead use
//! `display(x) -> (pattern, ..args)`, where `x` sets the name of the reference.
//!
//! ```rust
//! # #[macro_use] extern crate quick_error;
//! # fn main() {}
//! #
//! use std::error::Error; // put methods like `description()` of this trait into scope
//!
//! quick_error! {
//!     #[derive(Debug)]
//!     pub enum SomeError {
//!         Io(err: std::io::Error) {
//!             display(x) -> ("{}: {}", x.description(), err)
//!         }
//!         Utf8(err: std::str::Utf8Error) {
//!             display(self_) -> ("{}, valid up to {}", self_.description(), err.valid_up_to())
//!         }
//!     }
//! }
//! ```
//!
//! To convert to the type from any other, use one of the three forms of
//! `from` clause.
//!
//! For example, to convert simple wrapper use bare `from()`:
//!
//! ```rust
//! # #[macro_use] extern crate quick_error;
//! # fn main() {}
//! #
//! quick_error! {
//!     #[derive(Debug)]
//!     pub enum SomeError {
//!         Io(err: std::io::Error) {
//!             from()
//!         }
//!     }
//! }
//! ```
//!
//! This implements ``From<io::Error>``.
//!
//! To convert to singleton enumeration type (discarding the value), use
//! the `from(type)` form:
//!
//! ```rust
//! # #[macro_use] extern crate quick_error;
//! # fn main() {}
//! #
//! quick_error! {
//!     #[derive(Debug)]
//!     pub enum SomeError {
//!         FormatError {
//!             from(std::fmt::Error)
//!         }
//!     }
//! }
//! ```
//!
//! And the most powerful form is `from(var: type) -> (arguments...)`. It
//! might be used to convert to type with multiple arguments or for arbitrary
//! value conversions:
//!
//! ```rust
//! # #[macro_use] extern crate quick_error;
//! # fn main() {}
//! #
//! quick_error! {
//!     #[derive(Debug)]
//!     pub enum SomeError {
//!         FailedOperation(s: &'static str, errno: i32) {
//!             from(errno: i32) -> ("os error", errno)
//!             from(e: std::io::Error) -> ("io error", e.raw_os_error().unwrap())
//!         }
//!         /// Converts from both kinds of utf8 errors
//!         Utf8(err: std::str::Utf8Error) {
//!             from()
//!             from(err: std::string::FromUtf8Error) -> (err.utf8_error())
//!         }
//!     }
//! }
//! ```
//!
//! All forms of `from`, `display`, `description`, `cause` clauses can be
//! combined and put in arbitrary order. Only `from` may be used multiple times
//! in single variant of enumeration. Docstrings are also okay.
//! Empty braces can be omitted as of quick_error 0.1.3.
//!

/// Main macro that does all the work
#[macro_export]
macro_rules! quick_error {
    (   $(#[$meta:meta])*
        pub enum $name:ident { $($chunks:tt)* }
    ) => {
        quick_error!(SORT [pub enum $name $(#[$meta])* ]
            enum [] items [] buf []
            queue [ $($chunks)* ]);
    };
    (   $(#[$meta:meta])*
        enum $name:ident { $($chunks:tt)* }
    ) => {
        quick_error!(SORT [enum $name $(#[$meta])* ]
            enum [] items [] buf []
            queue [ $($chunks)* ]);
    };
    // Queue is empty, can do the work
    (SORT [enum $name:ident $(#[$meta:meta])* ]
        enum [ $( $(#[$emeta:meta])*
                  => $eitem:ident $( ( $($etyp:ty),* ) )* )* ]
        items [ $( $iitem:ident $(( $($ivar:ident : $ityp:ty),* ))*
                                { $($ifuncs:tt)* } )* ]
        buf [ ]
        queue [ ]
    ) => {
        $(#[$meta])*
        enum $name {
           $(
               $(#[$emeta])*
               $eitem $(( $($etyp),* ))*,
           )*
        }
        quick_error!(IMPLEMENTATIONS $name { $(
           $iitem $(( $($ivar: $ityp),* ))* { $($ifuncs)* }
           )* });
        $(
            quick_error!(ERROR_CHECK $($ifuncs)*);
        )*
    };
    (SORT [pub enum $name:ident $(#[$meta:meta])* ]
        enum [ $( $(#[$emeta:meta])*
                  => $eitem:ident $( ( $($etyp:ty),* ) )* )* ]
        items [ $( $iitem:ident $(( $($ivar:ident : $ityp:ty),* ))*
                                { $($ifuncs:tt)* } )* ]
        buf [ ]
        queue [ ]
    ) => {
        $(#[$meta])*
        pub enum $name {
           $(
               $(#[$emeta])*
               $eitem $(( $($etyp),* ))*,
           )*
        }
        quick_error!(IMPLEMENTATIONS $name { $(
           $iitem $(( $($ivar: $ityp),* ))* { $($ifuncs)* }
           )* });
        $(
            quick_error!(ERROR_CHECK $($ifuncs)*);
        )*
    };
    // Add meta to buffer
    (SORT [$($def:tt)*]
        enum [ $( $(#[$emeta:meta])*
                  => $eitem:ident $(( $($etyp:ty),* ))* )* ]
        items [ $( $iitem:ident $(( $($ivar:ident : $ityp:ty),* ))*
                                { $($ifuncs:tt)* } )* ]
        buf [ $( #[$bmeta:meta] )* ]
        queue [ #[$qmeta:meta] $($tail:tt)* ]
    ) => {
        quick_error!(SORT [$($def)* ]
            enum [$( $(#[$emeta])* => $eitem $(( $($etyp),* ))* )*]
            items [ $( $iitem $(( $($ivar:$ityp),* ))* { $($ifuncs)* } )* ]
            buf [ $( #[$bmeta] )* #[$qmeta] ]
            queue [ $($tail)* ]);
    };
    // Add ident to buffer
    (SORT [$($def:tt)*]
        enum [ $( $(#[$emeta:meta])*
                  => $eitem:ident $(( $($etyp:ty),* ))* )* ]
        items [ $( $iitem:ident $(( $($ivar:ident : $ityp:ty),* ))*
                                { $($ifuncs:tt)* } )* ]
        buf [ $( #[$bmeta:meta] )* ]
        queue [ $qitem:ident $($tail:tt)* ]
    ) => {
        quick_error!(SORT [$($def)* ]
            enum [ $( $(#[$emeta])* => $eitem $(( $($etyp),* ))* )* ]
            items [ $( $iitem $(( $($ivar:$ityp),* ))* { $($ifuncs)* } )* ]
            buf [ $(#[$bmeta])* => $qitem ]
            queue [ $($tail)* ]);
    };
    // Flush buffer on meta after ident
    (SORT [$($def:tt)*]
        enum [ $( $(#[$emeta:meta])*
                  => $eitem:ident $(( $($etyp:ty),* ))* )* ]
        items [ $( $iitem:ident $(( $($ivar:ident : $ityp:ty),* ))*
                                { $($ifuncs:tt)* } )* ]
        buf [ $( #[$bmeta:meta] )*
            => $bitem:ident $(( $($bvar:ident : $btyp:ty),* ))* ]
        queue [ #[$qmeta:meta] $($tail:tt)* ]
    ) => {
        quick_error!(SORT [$($def)* ]
            enum [$( $(#[$emeta])* => $eitem $(( $($etyp),* ))* )*
                     $(#[$bmeta])* => $bitem $(( $($btyp),* ))*]
            items [ $( $iitem $(( $($ivar:$ityp),* ))* { $($ifuncs)* } )*
                     $bitem $(( $($bvar:$btyp),* ))* {} ]
            buf [ #[$qmeta] ]
            queue [ $($tail)* ]);
    };
    // Add parenthesis
    (SORT [$($def:tt)*]
        enum [ $( $(#[$emeta:meta])*
                  => $eitem:ident $(( $($etyp:ty),* ))* )* ]
        items [ $( $iitem:ident $(( $($ivar:ident : $ityp:ty),* ))*
                                { $($ifuncs:tt)* } )* ]
        buf [ $( #[$bmeta:meta] )* => $bitem:ident ]
        queue [ ( $( $qvar:ident : $qtyp:ty ),* ) $($tail:tt)* ]
    ) => {
        quick_error!(SORT [$($def)* ]
            enum [$( $(#[$emeta])* => $eitem $(( $($etyp),* ))* )*]
            items [ $( $iitem $(( $($ivar:$ityp),* ))* { $($ifuncs)* } )* ]
            buf [ $( #[$bmeta] )* => $bitem ( $( $qvar:$qtyp ),* ) ]
            queue [ $($tail)* ]);
    };
    // Add braces and flush always on braces
    (SORT [$($def:tt)*]
        enum [ $( $(#[$emeta:meta])*
                  => $eitem:ident $(( $($etyp:ty),* ))* )* ]
        items [ $( $iitem:ident $(( $($ivar:ident : $ityp:ty),* ))*
                                { $($ifuncs:tt)* } )* ]
        buf [ $( #[$bmeta:meta] )*
                 => $bitem:ident $(( $($bvar:ident : $btyp:ty),* ))* ]
        queue [ { $($qfuncs:tt)* } $($tail:tt)* ]
    ) => {
        quick_error!(SORT [$($def)* ]
            enum [$( $(#[$emeta])* => $eitem $(( $($etyp),* ))* )*
                     $(#[$bmeta])* => $bitem $(( $($btyp),* ))* ]
            items [ $( $iitem $(( $($ivar:$ityp),* ))* { $($ifuncs)* } )*
                     $bitem $(( $($bvar:$btyp),* ))* { $($qfuncs)* } ]
            buf [ ]
            queue [ $($tail)* ]);
    };
    // Flush buffer on double ident
    (SORT [$($def:tt)*]
        enum [ $( $(#[$emeta:meta])*
                  => $eitem:ident $(( $($etyp:ty),* ))* )* ]
        items [ $( $iitem:ident $(( $($ivar:ident : $ityp:ty),* ))*
                                { $($ifuncs:tt)* } )* ]
        buf [ $( #[$bmeta:meta] )*
                 => $bitem:ident $(( $($bvar:ident : $btyp:ty),* ))* ]
        queue [ $qitem:ident $($tail:tt)* ]
    ) => {
        quick_error!(SORT [$($def)* ]
            enum [$( $(#[$emeta])* => $eitem $(( $($etyp),* ))* )*
                     $(#[$bmeta])* => $bitem $(( $($btyp),* ))*]
            items [ $( $iitem $(( $($ivar:$ityp),* ))* { $($ifuncs)* } )*
                     $bitem $(( $($bvar:$btyp),* ))* {} ]
            buf [ => $qitem ]
            queue [ $($tail)* ]);
    };
    // Flush buffer on end
    (SORT [$($def:tt)*]
        enum [ $( $(#[$emeta:meta])*
                  => $eitem:ident $(( $($etyp:ty),* ))* )* ]
        items [ $( $iitem:ident $(( $($ivar:ident : $ityp:ty),* ))*
                                { $($ifuncs:tt)* } )* ]
        buf [ $( #[$bmeta:meta] )*
            => $bitem:ident $(( $($bvar:ident : $btyp:ty),* ))* ]
        queue [ ]
    ) => {
        quick_error!(SORT [$($def)* ]
            enum [$( $(#[$emeta])* => $eitem $(( $($etyp),* ))* )*
                     $(#[$bmeta])* => $bitem $(( $($btyp),* ))* ]
            items [ $( $iitem $(( $($ivar:$ityp),* ))* { $($ifuncs)* } )*
                     $bitem $(( $($bvar:$btyp),* ))* {} ]
            buf [ ]
            queue [ ]);
    };
    (IMPLEMENTATIONS
        $name:ident {
            $(
                $item:ident $( ( $($var:ident : $typ:ty),* ) )* { $($funcs:tt)* }
            )*
        }
    ) => {
        #[allow(unused)]
        impl ::std::fmt::Display for $name {
            fn fmt(&self, fmt: &mut ::std::fmt::Formatter)
                -> ::std::fmt::Result
            {
                match self {
                    $(
                        &$name::$item $( ( $(ref $var),* ) )* => {
                            let display_fn = quick_error!(FIND_DISPLAY_IMPL
                                $name $item
                                { $($funcs)* });

                            display_fn(self, fmt)
                        }
                    )*
                }
            }
        }
        #[allow(unused)]
        impl ::std::error::Error for $name {
            fn description(&self) -> &str {
                match self {
                    $(
                        &$name::$item $( ( $(ref $var),* ) )* => {
                            quick_error!(FIND_DESCRIPTION_IMPL
                                $item self fmt [ $( ( $($var)* ) )* ]
                                { $($funcs)* })
                        }
                    )*
                }
            }
            fn cause(&self) -> Option<&::std::error::Error> {
                match self {
                    $(
                        &$name::$item $( ( $(ref $var),* ) )* => {
                            quick_error!(FIND_CAUSE_IMPL
                                $item [ $( ( $($var)* ) )* ]
                                { $($funcs)* })
                        }
                    )*
                }
            }
        }
        $(
            quick_error!(FIND_FROM_IMPL
                $name $item [ $( ( $($var:$typ)* ) )* ]
                { $($funcs)* });
        )*
    };
    (FIND_DISPLAY_IMPL $name:ident $item:ident
        { display($self_:tt) -> ($($exprs:tt)*) $($tail:tt)* }
    ) => {
        |quick_error!(IDENT $self_): &$name, f: &mut ::std::fmt::Formatter| { write!(f, $($exprs)*) }
    };
    (FIND_DISPLAY_IMPL $name:ident $item:ident
        { display($pattern:expr) $($tail:tt)* }
    ) => {
        |_, f: &mut ::std::fmt::Formatter| { write!(f, $pattern) }
    };
    (FIND_DISPLAY_IMPL $name:ident $item:ident
        { display($pattern:expr, $($exprs:tt)*) $($tail:tt)* }
    ) => {
        |_, f: &mut ::std::fmt::Formatter| { write!(f, $pattern, $($exprs)*) }
    };
    (FIND_DISPLAY_IMPL $name:ident $item:ident
        { $t:tt $($tail:tt)* }
    ) => {
        quick_error!(FIND_DISPLAY_IMPL
            $name $item
            { $($tail)* })
    };
    (FIND_DISPLAY_IMPL $name:ident $item:ident
        { }
    ) => {
        |self_: &$name, f: &mut ::std::fmt::Formatter| {
            write!(f, "{}", ::std::error::Error::description(self_))
        }
    };
    (FIND_DESCRIPTION_IMPL $item:ident $me:ident $fmt:ident
        [ $( ( $($var:ident)* ) )* ]
        { description($expr:expr) $($tail:tt)* }
    ) => {
        $expr
    };
    (FIND_DESCRIPTION_IMPL $item:ident $me:ident $fmt:ident
        [ $( ( $($var:ident)* ) )* ]
        { $t:tt $($tail:tt)* }
    ) => {
        quick_error!(FIND_DESCRIPTION_IMPL
            $item $me $fmt [ $( ( $($var)* ) )* ]
            { $($tail)* })
    };
    (FIND_DESCRIPTION_IMPL $item:ident $me:ident $fmt:ident
        [ $( ( $($var:ident)* ) )* ]
        { }
    ) => {
        stringify!($item)
    };
    (FIND_CAUSE_IMPL $item:ident
        [ $( ( $($var:ident)* ) )* ]
        { cause($expr:expr) $($tail:tt)* }
    ) => {
        Some($expr)
    };
    (FIND_CAUSE_IMPL $item:ident
        [ $( ( $($var:ident)* ) )* ]
        { $t:tt $($tail:tt)* }
    ) => {
        quick_error!(FIND_CAUSE_IMPL
            $item [ $( ( $($var)* ) )* ]
            { $($tail)* })
    };
    (FIND_CAUSE_IMPL $item:ident
        [ $( ( $($var:ident)* ) )* ]
        { }
    ) => {
        None
    };
    (FIND_FROM_IMPL $name:ident $item:ident
        [ $( ( $($var:ident : $typ:ty)* ) )* ]
        { from() $($tail:tt)* }
    ) => {
        $( $(
            impl From<$typ> for $name {
                fn from($var: $typ) -> $name {
                    $name::$item($var)
                }
            }
        )* )*
        quick_error!(FIND_FROM_IMPL
            $name $item [ $( ( $($var:$typ)* ) )* ]
            { $($tail)* });
    };
    (FIND_FROM_IMPL $name:ident $item:ident
        [ ]
        { from($ftyp:ty) $($tail:tt)* }
    ) => {
        impl From<$ftyp> for $name {
            fn from(_discarded_error: $ftyp) -> $name {
                $name::$item
            }
        }
        quick_error!(FIND_FROM_IMPL
            $name $item [  ]
            { $($tail)* });
    };
    (FIND_FROM_IMPL $name:ident $item:ident
        [ $( ( $($var:ident : $typ:ty)* ) )* ]
        { from($fvar:ident : $ftyp:ty) -> ($($expr:expr),*) $($tail:tt)* }
    ) => {
        impl From<$ftyp> for $name {
            fn from($fvar: $ftyp) -> $name {
                $name::$item($($expr),*)
            }
        }
        quick_error!(FIND_FROM_IMPL
            $name $item [ $( ( $($var:$typ)* ) )* ]
            { $($tail)* });
    };
    (FIND_FROM_IMPL $name:ident $item:ident
        [ $( ( $($var:ident : $typ:ty)* ) )* ]
        { $t:tt $($tail:tt)* }
    ) => {
        quick_error!(FIND_FROM_IMPL
            $name $item[ $( ( $($var:$typ)* ) )* ]
            { $($tail)* });
    };
    (FIND_FROM_IMPL $name:ident $item:ident
        [ $( ( $($var:ident : $typ:ty)* ) )* ]
        { }
    ) => {
    };
    // This one should match all allowed sequences in "funcs" but not match
    // anything else.
    // This is to contrast FIND_* clauses which just find stuff they need and
    // skip everything else completely
    (ERROR_CHECK display($self_:tt) -> ($($exprs:tt)*) $($tail:tt)*)
    => { quick_error!(ERROR_CHECK $($tail)*); };
    (ERROR_CHECK display($pattern: expr) $($tail:tt)*)
    => { quick_error!(ERROR_CHECK $($tail)*); };
    (ERROR_CHECK display($pattern: expr, $($exprs:tt)*) $($tail:tt)*)
    => { quick_error!(ERROR_CHECK $($tail)*); };
    (ERROR_CHECK description($expr:expr) $($tail:tt)*)
    => { quick_error!(ERROR_CHECK $($tail)*); };
    (ERROR_CHECK cause($expr:expr) $($tail:tt)*)
    => { quick_error!(ERROR_CHECK $($tail)*); };
    (ERROR_CHECK from() $($tail:tt)*)
    => { quick_error!(ERROR_CHECK $($tail)*); };
    (ERROR_CHECK from($ftyp:ty) $($tail:tt)*)
    => { quick_error!(ERROR_CHECK $($tail)*); };
    (ERROR_CHECK from($fvar:ident : $ftyp:ty) -> ($($e:expr),*) $($tail:tt)*)
    => { quick_error!(ERROR_CHECK $($tail)*); };
    (ERROR_CHECK) => {};
    // Utility functions
    (IDENT $ident: ident) => { $ident }
}

#[cfg(test)]
mod test {
    use std::io;
    use std::error::Error;

    quick_error! {
        #[derive(Debug)]
        pub enum Bare {
            One
            Two
        }
    }

    #[test]
    fn bare_item_direct() {
        assert_eq!(format!("{}", Bare::One), "One".to_string());
        assert_eq!(format!("{:?}", Bare::One), "One".to_string());
        assert_eq!(Bare::One.description(), "One".to_string());
        assert!(Bare::One.cause().is_none());
    }
    #[test]
    fn bare_item_trait() {
        let err: &Error = &Bare::Two;
        assert_eq!(format!("{}", err), "Two".to_string());
        assert_eq!(format!("{:?}", err), "Two".to_string());
        assert_eq!(err.description(), "Two".to_string());
        assert!(err.cause().is_none());
    }

    quick_error! {
        #[derive(Debug)]
        pub enum IoWrapper {
            /// I/O Error
            Io(err: io::Error) {
                from()
                description(err.description())
                display("I/O error: {err}", err=err)
                cause(err)
            }
            Other(descr: &'static str) {
                description(descr)
                display("Error: {}", descr)
            }
            /// I/O error with some context
            IoAt(place: &'static str, err: io::Error) {
                cause(err)
                display(self_) -> ("{} {}: {}", self_.description(), place, err)
                description("io error at")
                from(s: String) -> ("idea",
                                    io::Error::new(io::ErrorKind::Other, s))
            }
            Discard {
                from(&'static str)
            }
            Singleton {
                display("Just a string")
            }
        }
    }

    #[test]
    fn io_wrapper_err() {
        let io1 = IoWrapper::Io(
            io::Error::new(io::ErrorKind::Other, "some error"));
        assert_eq!(format!("{}", io1), "I/O error: some error".to_string());
        assert_eq!(format!("{:?}", io1),
            "Io(Error { repr: Custom(Custom { kind: Other, \
             error: StringError(\"some error\") }) })".to_string());
        assert_eq!(io1.description(), "some error");
        assert_eq!(io1.cause().unwrap().description(), "some error");
    }

    #[test]
    fn io_wrapper_trait_str() {
        let err: &Error = &IoWrapper::Other("hello");
        assert_eq!(format!("{}", err), "Error: hello".to_string());
        assert_eq!(format!("{:?}", err), "Other(\"hello\")".to_string());
        assert_eq!(err.description(), "hello".to_string());
        assert!(err.cause().is_none());
    }

    #[test]
    fn io_wrapper_trait_two_fields() {
        let io1 = IoWrapper::Io(
            io::Error::new(io::ErrorKind::Other, "some error"));
        let err: &Error = &IoWrapper::IoAt("file",
            io::Error::new(io::ErrorKind::NotFound, io1));
        assert_eq!(format!("{}", err),
            "io error at file: I/O error: some error".to_string());
        assert_eq!(format!("{:?}", err), "IoAt(\"file\", Error { \
            repr: Custom(Custom { kind: NotFound, \
                error: Io(Error { repr: Custom(Custom { \
                    kind: Other, error: StringError(\"some error\") \
            }) }) }) })".to_string());
        assert_eq!(err.description(), "io error at");
        assert_eq!(err.cause().unwrap().description(), "some error");
    }

    #[test]
    fn io_wrapper_from() {
        let io1: IoWrapper = From::from(io::Error::from_raw_os_error(2));
        assert_eq!(format!("{}", io1),
            "I/O error: No such file or directory (os error 2)".to_string());
        let descr = io1.cause().unwrap().description();
        assert!(descr == "os error" // rust <= 1.6
            || descr == "entity not found" // rust 1.7 (probably, nightly)
            );
    }

    #[test]
    fn io_wrapper_custom_from() {
        let io1: IoWrapper = From::from("Stringy".to_string());
        assert_eq!(format!("{}", io1), "io error at idea: Stringy".to_string());
        assert_eq!(io1.cause().unwrap().description(), "Stringy");
    }

    #[test]
    fn io_wrapper_discard() {
        let io1: IoWrapper = From::from("hello");
        assert_eq!(format!("{}", io1), "Discard".to_string());
        assert!(io1.cause().is_none());
    }

    #[test]
    fn io_wrapper_signleton() {
        let io1: IoWrapper = IoWrapper::Singleton;
        assert_eq!(format!("{}", io1), "Just a string".to_string());
    }

}
