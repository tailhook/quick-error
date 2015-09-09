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
            fn cause(&self) -> Option<&Error> {
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
    (ERROR_CHECK) => {};
}

#[cfg(test)]
mod test {
    use std::io;
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
            IoAt(place: &'static str, err: io::Error) {
                cause(err)
                display("Error at {}: {}", place, err)
                description("io error at")
                from(s: String) -> ("idea",
                                    io::Error::new(io::ErrorKind::Other, s))
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
        let err: &Error = &IoWrapper::IoAt("file",
            io::Error::new(io::ErrorKind::NotFound, io1));
        assert_eq!(format!("{}", err),
            "Error at file: I/O error: some error".to_string());
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

}
