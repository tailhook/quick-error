//! A macro which makes errors easy to write
//!
//! Minimum type is like this:
//!
//! ```rust
//!
//! #[macro_use] extern crate quick_error;
//!
//! quick_error! {
//!     #[derive(Debug)]
//!     pub enum SomeError {
//!         Variant1 {}
//!     }
//! }
//! ```
//! Both ``pub`` and non-public types may be declared, and all meta attributes
//! (such as ``#[derive(Debug)]``) are forwarded as is.
//!
//! You may add arbitrary parameters to any struct variant:
//!
//! ```rust
//! quick_error! {
//!     #[derive(Debug)]
//!     pub enum SomeError {
//!         Io(err: io::Error) {}
//!         Sys(errno: nix::Errno) {}
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
//! quick_error! {
//!     #[derive(Debug)]
//!     pub enum SomeError {
//!         Io(err: io::Error) {
//!             description(err.description())
//!         }
//!         Sys(errno: nix::Errno) {
//!             description("system error")
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
//! quick_error! {
//!     #[derive(Debug)]
//!     pub enum SomeError {
//!         Io(err: io::Error) {
//!             cause(err)
//!             description(err.description())
//!         }
//!         Sys(errno: nix::Errno) {
//!             description("system error")
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
//! quick_error! {
//!     #[derive(Debug)]
//!     pub enum SomeError {
//!         Io(err: io::Error) {
//!             display("I/O error: {}", err)
//!         }
//!         Sys(errno: nix::Errno) {
//!             display("System error, errno ({:x})", errno)
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
//! quick_error! {
//!     #[derive(Debug)]
//!     pub enum SomeError {
//!         Io(err: io::Error) {
//!             from()
//!         }
//!     }
//! ```
//!
//! This implements ``From<io::Error>``.
//!
//! To convert to singleton enumeration type (discarding the value), use
//! the `from(type)` form:
//!
//! ```rust
//! quick_error! {
//!     #[derive(Debug)]
//!     pub enum SomeError {
//!         FormatError {
//!             from(std::fmt::Error)
//!         }
//!     }
//! ```
//!
//! And the most powerful form is `from(var: type) -> (arguments...)`. It
//! might be used to convert to type with multiple arguments or for arbitrary
//! value conversions:
//!
//! ```rust
//! quick_error! {
//!     #[derive(Debug)]
//!     pub enum SomeError {
//!         FailedOperation(s: &'static str, errno: i32) {
//!             from(errno: i32) -> ("os error", i32)
//!             from(e: io::Error) -> ("io error", e.raw_os_error().unwrap())
//!         }
//!         /// Converts from both kinds of utf8 errors
//!         Utf8(err: std::str::Utf8Error) {
//!             from()
//!             from(err: std::string::FromUtf8Error) -> (err.utf8_error())
//!         }
//!     }
//! ```
//!
//! All forms of `from`, `display`, `description`, `cause` clauses can be
//! combined and put in arbitrary order. Only `from` may be used multiple times
//! in single variant of enumeration. Docstrings are also okay.
//! Empty braces can't be omitted.
//!


/// Main macro that does all the work
#[macro_export]
macro_rules! quick_error {
    (
        $(#[$meta:meta])*
        pub enum $name:ident {
           $(
               $item:ident $( ( $($var:ident : $typ:ty),* ) )* { $($funcs:tt)* }
           )*
        }
    ) => {
        $(#[$meta])*
        pub enum $name {
           $( $item $( ( $($typ),* ) )*, )*
        }
        quick_error!(IMPLEMENTATIONS $name { $(
           $item $( ( $($var: $typ),* ) )* { $($funcs)* }
           )* });
        $(
            quick_error!(ERROR_CHECK $($funcs)*);
        )*
    };
    (
        $(#[$meta:meta])*
        enum $name:ident {
           $(
               $item:ident $( ( $($var:ident : $typ:ty),* ) )* { $($funcs:tt)* }
           )*
        }
    ) => {
        $(#[$meta])*
         enum $name {
            $( $item $( ( $($typ),* ) )*, )*
         }
         quick_error!(IMPLEMENTATIONS $name { $(
            $item $( ( $($var: $typ),* ) )* { $($funcs)* }
            )* });
        $(
            quick_error!(ERROR_CHECK  $($funcs)*);
        )*
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
                            quick_error!(FIND_DISPLAY_IMPL
                                $item self fmt [ $( ( $($var)* ) )* ]
                                { $($funcs)* })
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
        $(
            quick_error!(FIND_CONTEXT_IMPL
                $name $item [ $( ( $({ $var:$typ })* ) )* ]
                { $($funcs)* });
        )*
    };
    (FIND_DISPLAY_IMPL $item:ident $me:ident $fmt:ident
        [ $( ( $($var:ident)* ) )* ]
        { display($($exprs:expr),*) $($tail:tt)* }
    ) => {
        write!($fmt, $($exprs),*)
    };
    (FIND_DISPLAY_IMPL $item:ident $me:ident $fmt:ident
        [ $( ( $($var:ident)* ) )* ]
        { $t:tt $($tail:tt)* }
    ) => {
        quick_error!(FIND_DISPLAY_IMPL
            $item $me $fmt [ $( ( $($var)* ) )* ]
            { $($tail)* })
    };
    (FIND_DISPLAY_IMPL $item:ident $me:ident $fmt:ident
        [ $( ( $($var:ident)* ) )* ]
        { }
    ) => {
        write!($fmt, "{}", ::std::error::Error::description($me))
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
    (FIND_CONTEXT_IMPL $name:ident $item:ident
        [ $( ( { $fvar:ident : $ftyp:ty } $({ $var:ident : $typ:ty })* ) )* ]
        { context($trait_name:ident :: $meth:ident) $($tail:tt)* }
    ) => {
        pub trait $trait_name<T> {
            fn $meth(self, $( $($var: $typ),* ),*) -> Result<T, $name>;
        }
        impl<T> $trait_name<T> for Result<T, $( $ftyp ),*> {
            fn $meth(self, $( $($var: $typ),* ),*) -> Result<T, $name> {
                self.map_err(|e| $name::$item(e, $( $( $var ),* ),*))
            }
        }
    };
    (FIND_CONTEXT_IMPL $name:ident $item:ident
        [ $( ( $({ $var:ident : $typ:ty })* ) )* ]
        { $t:tt $($tail:tt)* }
    ) => {
        quick_error!(FIND_CONTEXT_IMPL
            $name $item[ $( ( $({ $var:$typ })* ) )* ]
            { $($tail)* });
    };
    (FIND_CONTEXT_IMPL $name:ident $item:ident
        [ $( ( $({ $var:ident : $typ:ty })* ) )* ]
        { }
    ) => {
    };
    // This one should match all allowed sequences in "funcs" but not match
    // anything else.
    // This is to contrast FIND_* clauses which just find stuff they need and
    // skip everything else completely
    (ERROR_CHECK display($($exprs:expr),*) $($tail:tt)*)
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
    (ERROR_CHECK context($trait_name:ident :: $meth:ident) $($tail:tt)*)
    => { quick_error!(ERROR_CHECK $($tail)*); };
    (ERROR_CHECK) => {};
}

#[cfg(test)]
mod test {
    use std::io;
    use std::io::Write;
    use std::fs::File;
    use std::error::Error;

    quick_error! {
        #[derive(Debug)]
        pub enum Bare {
            One {}
            Two {}
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
            Io(err: io::Error) {
                from()
                description(err.description())
                display("I/O error: {}", err)
                cause(err)
            }
            Other(descr: &'static str) {
                description(descr)
                display("Error: {}", descr)
            }
            IoAt(err: io::Error, place: &'static str) {
                cause(err)
                display("Error at {}: {}", place, err)
                description("io error at")
                from(s: String) -> (io::Error::new(io::ErrorKind::Other, s),
                                    "idea")
                context(With::with)
            }
            Discard {
                from(&'static str)
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
        let err: &Error = &IoWrapper::IoAt(
            io::Error::new(io::ErrorKind::NotFound, io1),
            "file");
        assert_eq!(format!("{}", err),
            "Error at file: I/O error: some error".to_string());
        assert_eq!(format!("{:?}", err), "IoAt(Error { \
            repr: Custom(Custom { kind: NotFound, \
                error: Io(Error { repr: Custom(Custom { \
                    kind: Other, error: StringError(\"some error\") \
            }) }) }) }, \"file\")".to_string());
        assert_eq!(err.description(), "io error at");
        assert_eq!(err.cause().unwrap().description(), "some error");
    }

    #[test]
    fn io_wrapper_from() {
        let io1: IoWrapper = From::from(io::Error::from_raw_os_error(2));
        assert_eq!(format!("{}", io1),
            "I/O error: No such file or directory (os error 2)".to_string());
        assert_eq!(io1.cause().unwrap().description(), "os error");
    }

    #[test]
    fn io_wrapper_custom_from() {
        let io1: IoWrapper = From::from("Stringy".to_string());
        assert_eq!(format!("{}", io1), "Error at idea: Stringy".to_string());
        assert_eq!(io1.cause().unwrap().description(), "Stringy");
    }

    #[test]
    fn io_wrapper_discard() {
        let io1: IoWrapper = From::from("hello");
        assert_eq!(format!("{}", io1), "Discard".to_string());
        assert!(io1.cause().is_none());
    }

    #[test]
    fn io_wrapper_with() {
        fn tryio() -> Result<(), IoWrapper> {
            try!(File::create("/dev/full")
                .and_then(|mut f| f.write(b"test"))
                .with("/dev/null"));
            Ok(())
        }
        let err = tryio().unwrap_err();
        assert_eq!(format!("{}", err),
            "Error at /dev/null: No space left on device (os error 28)"
            .to_string());
    }

}
