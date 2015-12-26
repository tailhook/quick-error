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
            items [] buf []
            queue [ $($chunks)* ]);
    };
    (   $(#[$meta:meta])*
        enum $name:ident { $($chunks:tt)* }
    ) => {
        quick_error!(SORT [enum $name $(#[$meta])* ]
            items [] buf []
            queue [ $($chunks)* ]);
    };
    // Queue is empty, can do the work
    (SORT [enum $name:ident $( #[$meta:meta] )*]
        items [$($( #[$imeta:imeta] )*
                  => $iitem:ident: $imode:tt [$( $ivar:ident: $ityp:ty ),*]
                                {$( $ifuncs:tt )*} )* ]
        buf [ ]
        queue [ ]
    ) => {
        quick_error!(ENUM_DEFINITION [enum $name $( #[$meta] )*]
            body []
            queue [$($( #[$imeta] )*
                      => $iitem: $imode [$( $ivar: $ityp ),*] )*]
        );
        quick_error!(IMPLEMENTATIONS $name {$(
           $iitem: $imode [$( $ivar: $ityp ),*] {$( $ifuncs )*}
           )*});
        $(
            quick_error!(ERROR_CHECK $imode $($ifuncs)*);
        )*
    };
    (SORT [pub enum $name:ident $( #[$meta:meta] )*]
        items [$($( #[$imeta:meta] )*
                  => $iitem:ident: $imode:tt [$( $ivar:ident: $ityp:ty ),*]
                                {$( $ifuncs:tt )*} )* ]
        buf [ ]
        queue [ ]
    ) => {
        quick_error!(ENUM_DEFINITION [pub enum $name $( #[$meta] )*]
            body []
            queue [$($( #[$imeta] )*
                      => $iitem: $imode [$( $ivar: $ityp ),*] )*]
        );
        quick_error!(IMPLEMENTATIONS $name {$(
           $iitem: $imode [$( $ivar: $ityp ),*] {$( $ifuncs )*}
           )*});
        $(
            quick_error!(ERROR_CHECK $imode $($ifuncs)*);
        )*
    };
    // Add meta to buffer
    (SORT [$( $def:tt )*]
        items [$($( #[$imeta:meta] )*
                  => $iitem:ident: $imode:tt [$( $ivar:ident: $ityp:ty ),*]
                                {$( $ifuncs:tt )*} )* ]
        buf [$( #[$bmeta:meta] )*]
        queue [ #[$qmeta:meta] $( $tail:tt )*]
    ) => {
        quick_error!(SORT [$( $def )*]
            items [$( $(#[$imeta])* => $iitem: $imode [$( $ivar:$ityp ),*] {$( $ifuncs )*} )*]
            buf [$( #[$bmeta] )* #[$qmeta] ]
            queue [$( $tail )*]);
    };
    // Add ident to buffer
    (SORT [$( $def:tt )*]
        items [$($( #[$imeta:meta] )*
                  => $iitem:ident: $imode:tt [$( $ivar:ident: $ityp:ty ),*]
                                {$( $ifuncs:tt )*} )* ]
        buf [$( #[$bmeta:meta] )*]
        queue [ $qitem:ident $( $tail:tt )*]
    ) => {
        quick_error!(SORT [$( $def )*]
            items [$( $(#[$imeta])*
                      => $iitem: $imode [$( $ivar:$ityp ),*] {$( $ifuncs )*} )*]
            buf [$(#[$bmeta])* => $qitem : UNIT [ ] ]
            queue [$( $tail )*]);
    };
    // Flush buffer on meta after ident
    (SORT [$( $def:tt )*]
        items [$($( #[$imeta:meta] )*
                  => $iitem:ident: $imode:tt [$( $ivar:ident: $ityp:ty ),*]
                                {$( $ifuncs:tt )*} )* ]
        buf [$( #[$bmeta:meta] )*
            => $bitem:ident: $bmode:tt [$( $bvar:ident: $btyp:ty ),*] ]
        queue [ #[$qmeta:meta] $( $tail:tt )*]
    ) => {
        quick_error!(SORT [$( $def )*]
            enum [$( $(#[$emeta])* => $eitem $(( $($etyp),* ))* )*
                     $(#[$bmeta])* => $bitem: $bmode $(( $($btyp),* ))*]
            items [$($( #[$imeta:imeta] )*
                      => $iitem: $imode [$( $ivar:$ityp ),*] {$( $ifuncs )*} )*
                     $bitem: $bmode [$( $bvar:$btyp ),*] {} ]
            buf [ #[$qmeta] ]
            queue [$( $tail )*]);
    };
    // Add tuple enum-variant
    (SORT [$( $def:tt )*]
        items [$($( #[$imeta:meta] )*
                  => $iitem:ident: $imode:tt [$( $ivar:ident: $ityp:ty ),*]
                                {$( $ifuncs:tt )*} )* ]
        buf [$( #[$bmeta:meta] )* => $bitem:ident: UNIT [ ] ]
        queue [($( $qvar:ident: $qtyp:ty ),+) $( $tail:tt )*]
    ) => {
        quick_error!(SORT [$( $def )*]
            items [$( $(#[$imeta])* => $iitem: $imode [$( $ivar:$ityp ),*] {$( $ifuncs )*} )*]
            buf [$( #[$bmeta] )* => $bitem: TUPLE [$( $qvar:$qtyp ),*] ]
            queue [$( $tail )*]
        );
    };
    // Add struct enum-variant
    (SORT [$( $def:tt )*]
        items [$($( #[$imeta:meta] )*
                  => $iitem:ident: $imode:tt [$( $ivar:ident: $ityp:ty ),*]
                                {$( $ifuncs:tt )*} )* ]
        buf [$( #[$bmeta:meta] )* => $bitem:ident: UNIT [ ] ]
        queue [{ $( $qvar:ident: $qtyp:ty ),+ } $( $tail:tt )*]
    ) => {
        quick_error!(SORT [$( $def )*]
            items [$( $(#[$imeta])* => $iitem: $imode [$( $ivar:$ityp ),*] {$( $ifuncs )*} )*]
            buf [$( #[$bmeta] )* => $bitem: STRUCT [$( $qvar:$qtyp ),*] ]
            queue [$( $tail )*]);
    };
    // Add braces and flush always on braces
    (SORT [$( $def:tt )*]
        items [$($( #[$imeta:meta] )*
                  => $iitem:ident: $imode:tt [$( $ivar:ident: $ityp:ty ),*]
                                {$( $ifuncs:tt )*} )* ]
        buf [$( #[$bmeta:meta] )*
                 => $bitem:ident: $bmode:tt [$( $bvar:ident: $btyp:ty ),*] ]
        queue [ {$( $qfuncs:tt )*} $( $tail:tt )*]
    ) => {
        quick_error!(SORT [$( $def )*]
            items [$( $(#[$imeta])* => $iitem: $imode [$( $ivar:$ityp ),*] {$( $ifuncs )*} )*
                      $(#[$bmeta])* => $bitem: $bmode [$( $bvar:$btyp ),*] {$( $qfuncs )*} ]
            buf [ ]
            queue [$( $tail )*]);
    };
    // Flush buffer on double ident
    (SORT [$( $def:tt )*]
        items [$($( #[$imeta:meta] )*
                  => $iitem:ident: $imode:tt [$( $ivar:ident: $ityp:ty ),*]
                                {$( $ifuncs:tt )*} )* ]
        buf [$( #[$bmeta:meta] )*
                 => $bitem:ident: $bmode:tt [$( $bvar:ident: $btyp:ty ),*] ]
        queue [ $qitem:ident $( $tail:tt )*]
    ) => {
        quick_error!(SORT [$( $def )*]
            items [$( $(#[$imeta])* => $iitem: $imode [$( $ivar:$ityp ),*] {$( $ifuncs )*} )*
                     $(#[$bmeta])* => $bitem: $bmode [$( $bvar:$btyp ),*] {} ]
            buf [ => $qitem : UNIT [ ] ]
            queue [$( $tail )*]);
    };
    // Flush buffer on end
    (SORT [$( $def:tt )*]
        items [$($( #[$imeta:meta] )*
                  => $iitem:ident: $imode:tt [$( $ivar:ident: $ityp:ty ),*]
                                {$( $ifuncs:tt )*} )* ]
        buf [$( #[$bmeta:meta] )*
            => $bitem:ident: $bmode:tt [$( $bvar:ident: $btyp:ty ),*] ]
        queue [ ]
    ) => {
        quick_error!(SORT [$( $def )*]
            items [$( $(#[$imeta])* => $iitem: $imode [$( $ivar:$ityp ),*] {$( $ifuncs )*} )*
                     $(#[$bmeta])* => $bitem: $bmode [$( $bvar:$btyp ),*] {} ]
            buf [ ]
            queue [ ]);
    };
    // Public enum (Queue Empty)
    (ENUM_DEFINITION [pub enum $name:ident $( #[$meta:meta] )*]
        body [$($( #[$imeta:meta] )*
            => $iitem:ident ($(($( $ttyp:ty ),+))*) {$({$( $svar:ident: $styp:ty ),*})*} )* ]
        queue [ ]
    ) => {
        $(#[$meta])*
        pub enum $name {
            $(
                $(#[$imeta])*
                $iitem $(($( $ttyp ),*))* $({$( $svar: $styp ),*})*,
            )*
        }
    };
    // Private enum (Queue Empty)
    (ENUM_DEFINITION [enum $name:ident $( #[$meta:meta] )*]
        body [$($( #[$imeta:meta] )*
            => $iitem:ident ($(($( $ttyp:ty ),+))*) {$({$( $svar:ident: $styp:ty ),*})*} )* ]
        queue [ ]
    ) => {
        $(#[$meta])*
        enum $name {
            $(
                $(#[$imeta])*
                $iitem $(($( $ttyp ),*))* $({$( $svar: $styp ),*})*,
            )*
        }
    };
    // Unit variant
    (ENUM_DEFINITION [$( $def:tt )*]
        body [$($( #[$imeta:meta] )*
            => $iitem:ident ($(($( $ttyp:ty ),+))*) {$({$( $svar:ident: $styp:ty ),*})*} )* ]
        queue [$( #[$qmeta:meta] )*
            => $qitem:ident: UNIT [ ] $( $queue:tt )*]
    ) => {
        quick_error!(ENUM_DEFINITION [ $($def)* ]
            body [$($( #[$imeta] )* => $iitem ($(($( $ttyp ),+))*) {$({$( $svar: $styp ),*})*} )*
                    $( #[$qmeta] )* => $qitem () {} ]
            queue [ $($queue)* ]
        );
    };
    // Tuple variant
    (ENUM_DEFINITION [$( $def:tt )*]
        body [$($( #[$imeta:meta] )*
            => $iitem:ident ($(($( $ttyp:ty ),+))*) {$({$( $svar:ident: $styp:ty ),*})*} )* ]
        queue [$( #[$qmeta:meta] )*
            => $qitem:ident: TUPLE [$( $qvar:ident: $qtyp:ty ),+] $( $queue:tt )*]
    ) => {
        quick_error!(ENUM_DEFINITION [ $($def)* ]
            body [$($( #[$imeta] )* => $iitem ($(($( $ttyp ),+))*) {$({$( $svar: $styp ),*})*} )*
                    $( #[$qmeta] )* => $qitem (($( $qtyp ),*)) {} ]
            queue [ $($queue)* ]
        );
    };
    // Struct variant
    (ENUM_DEFINITION [$( $def:tt )*]
        body [$($( #[$imeta:meta] )*
            => $iitem:ident ($(($( $ttyp:ty ),+))*) {$({$( $svar:ident: $styp:ty ),*})*} )* ]
        queue [$( #[$qmeta:meta] )*
            => $qitem:ident: STRUCT [$( $qvar:ident: $qtyp:ty ),*] $( $queue:tt )*]
    ) => {
        quick_error!(ENUM_DEFINITION [ $($def)* ]
            body [$($( #[$imeta] )* => $iitem ($(($( $ttyp ),+))*) {$({$( $svar: $styp ),*})*} )*
                    $( #[$qmeta] )* => $qitem () {{$( $qvar: $qtyp ),*}} ]
            queue [ $($queue)* ]
        );
    };
    (IMPLEMENTATIONS
        $name:ident {$(
            $item:ident: $imode:tt [$( $var:ident: $typ:ty ),*] {$( $funcs:tt )*}
        )*}
    ) => {
        #[allow(unused)]
        impl ::std::fmt::Display for $name {
            fn fmt(&self, fmt: &mut ::std::fmt::Formatter)
                -> ::std::fmt::Result
            {
                match *self {
                    $(
                        quick_error!(ITEM_PATTERN
                            $name $item: $imode [$( ref $var ),*]
                        ) => {
                            let display_fn = quick_error!(FIND_DISPLAY_IMPL
                                $name $item: $imode
                                {$( $funcs )*});

                            display_fn(self, fmt)
                        }
                    )*
                }
            }
        }
        #[allow(unused)]
        impl ::std::error::Error for $name {
            fn description(&self) -> &str {
                match *self {
                    $(
                        quick_error!(ITEM_PATTERN
                            $name $item: $imode [$( ref $var ),*]
                        ) => {
                            quick_error!(FIND_DESCRIPTION_IMPL
                                $item: $imode self fmt [$( $var ),*]
                                {$( $funcs )*})
                        }
                    )*
                }
            }
            fn cause(&self) -> Option<&::std::error::Error> {
                match *self {
                    $(
                        quick_error!(ITEM_PATTERN
                            $name $item: $imode [$( ref $var ),*]
                        ) => {
                            quick_error!(FIND_CAUSE_IMPL
                                $item: $imode [$( $var ),*]
                                {$( $funcs )*})
                        }
                    )*
                }
            }
        }
        $(
            quick_error!(FIND_FROM_IMPL
                $name $item: $imode [$( $var:$typ ),*]
                {$( $funcs )*});
        )*
    };
    (FIND_DISPLAY_IMPL $name:ident $item:ident: $imode:tt
        { display($self_:tt) -> ($( $exprs:tt )*) $( $tail:tt )*}
    ) => {
        |quick_error!(IDENT $self_): &$name, f: &mut ::std::fmt::Formatter| { write!(f, $( $exprs )*) }
    };
    (FIND_DISPLAY_IMPL $name:ident $item:ident: $imode:tt
        { display($pattern:expr) $( $tail:tt )*}
    ) => {
        |_, f: &mut ::std::fmt::Formatter| { write!(f, $pattern) }
    };
    (FIND_DISPLAY_IMPL $name:ident $item:ident: $imode:tt
        { display($pattern:expr, $( $exprs:tt )*) $( $tail:tt )*}
    ) => {
        |_, f: &mut ::std::fmt::Formatter| { write!(f, $pattern, $( $exprs )*) }
    };
    (FIND_DISPLAY_IMPL $name:ident $item:ident: $imode:tt
        { $t:tt $( $tail:tt )*}
    ) => {
        quick_error!(FIND_DISPLAY_IMPL
            $name $item: $imode
            {$( $tail )*})
    };
    (FIND_DISPLAY_IMPL $name:ident $item:ident: $imode:tt
        { }
    ) => {
        |self_: &$name, f: &mut ::std::fmt::Formatter| {
            write!(f, "{}", ::std::error::Error::description(self_))
        }
    };
    (FIND_DESCRIPTION_IMPL $item:ident: $imode:tt $me:ident $fmt:ident
        [$( $var:ident ),*]
        { description($expr:expr) $( $tail:tt )*}
    ) => {
        $expr
    };
    (FIND_DESCRIPTION_IMPL $item:ident: $imode:tt $me:ident $fmt:ident
        [$( $var:ident ),*]
        { $t:tt $( $tail:tt )*}
    ) => {
        quick_error!(FIND_DESCRIPTION_IMPL
            $item: $imode $me $fmt [$( $var ),*]
            {$( $tail )*})
    };
    (FIND_DESCRIPTION_IMPL $item:ident: $imode:tt $me:ident $fmt:ident
        [$( $var:ident ),*]
        { }
    ) => {
        stringify!($item)
    };
    (FIND_CAUSE_IMPL $item:ident: $imode:tt
        [$( $var:ident ),*]
        { cause($expr:expr) $( $tail:tt )*}
    ) => {
        Some($expr)
    };
    (FIND_CAUSE_IMPL $item:ident: $imode:tt
        [$( $var:ident ),*]
        { $t:tt $( $tail:tt )*}
    ) => {
        quick_error!(FIND_CAUSE_IMPL
            $item: $imode [$( $var ),*]
            { $($tail)* })
    };
    (FIND_CAUSE_IMPL $item:ident: $imode:tt
        [$( $var:ident ),*]
        { }
    ) => {
        None
    };
    (FIND_FROM_IMPL $name:ident $item:ident: $imode:tt
        [$( $var:ident: $typ:ty ),*]
        { from() $( $tail:tt )*}
    ) => {
        $(
            impl From<$typ> for $name {
                fn from($var: $typ) -> $name {
                    $name::$item($var)
                }
            }
        )*
        quick_error!(FIND_FROM_IMPL
            $name $item: $imode [$( $var:$typ ),*]
            {$( $tail )*});
    };
    (FIND_FROM_IMPL $name:ident $item:ident: UNIT
        [ ]
        { from($ftyp:ty) $( $tail:tt )*}
    ) => {
        impl From<$ftyp> for $name {
            fn from(_discarded_error: $ftyp) -> $name {
                $name::$item
            }
        }
        quick_error!(FIND_FROM_IMPL
            $name $item: UNIT [  ]
            {$( $tail )*});
    };
    (FIND_FROM_IMPL $name:ident $item:ident: TUPLE
        [$( $var:ident: $typ:ty ),*]
        { from($fvar:ident: $ftyp:ty) -> ($( $texpr:expr ),*) $( $tail:tt )*}
    ) => {
        impl From<$ftyp> for $name {
            fn from($fvar: $ftyp) -> $name {
                $name::$item($( $texpr ),*)
            }
        }
        quick_error!(FIND_FROM_IMPL
            $name $item: TUPLE [$( $var:$typ ),*]
            { $($tail)* });
    };
    (FIND_FROM_IMPL $name:ident $item:ident: STRUCT
        [$( $var:ident: $typ:ty ),*]
        { from($fvar:ident: $ftyp:ty) -> {$( $tvar:ident: $texpr:expr ),*} $( $tail:tt )*}
    ) => {
        impl From<$ftyp> for $name {
            fn from($fvar: $ftyp) -> $name {
                $name::$item {
                    $( $tvar: $texpr ),*
                }
            }
        }
        quick_error!(FIND_FROM_IMPL
            $name $item: STRUCT [$( $var:$typ ),*]
            { $($tail)* });
    };
    (FIND_FROM_IMPL $name:ident $item:ident: $imode:tt
        [$( $var:ident: $typ:ty ),*]
        { $t:tt $( $tail:tt )*}
    ) => {
        quick_error!(FIND_FROM_IMPL
            $name $item: $imode [$( $var:$typ ),*]
            {$( $tail )*}
        );
    };
    (FIND_FROM_IMPL $name:ident $item:ident: $imode:tt
        [$( $var:ident: $typ:ty ),*]
        { }
    ) => {
    };
    (ITEM_BODY $(#[$imeta:imeta])* $item:ident: UNIT
    ) => { };
    (ITEM_BODY $(#[$imeta:imeta])* $item:ident: TUPLE
        [$( $typ:ty ),*]
    ) => {
        ($( $typ ),*)
    };
    (ITEM_BODY $(#[$imeta:imeta])* $item:ident: STRUCT
        [$( $var:ident: $typ:ty ),*]
    ) => {
        {$( $var:$typ ),*}
    };
    (ITEM_PATTERN $name:ident $item:ident: UNIT []
    ) => {
        $name::$item
    };
    (ITEM_PATTERN $name:ident $item:ident: TUPLE
        [$( ref $var:ident ),*]
    ) => {
        $name::$item ($( ref $var ),*)
    };
    (ITEM_PATTERN $name:ident $item:ident: STRUCT
        [$( ref $var:ident ),*]
    ) => {
        $name::$item {$( ref $var ),*}
    };
    // This one should match all allowed sequences in "funcs" but not match
    // anything else.
    // This is to contrast FIND_* clauses which just find stuff they need and
    // skip everything else completely
    (ERROR_CHECK $imode:tt display($self_:tt) -> ($( $exprs:tt )*) $( $tail:tt )*)
    => { quick_error!(ERROR_CHECK $imode $($tail)*); };
    (ERROR_CHECK $imode:tt display($pattern: expr) $( $tail:tt )*)
    => { quick_error!(ERROR_CHECK $imode $($tail)*); };
    (ERROR_CHECK $imode:tt display($pattern: expr, $( $exprs:tt )*) $( $tail:tt )*)
    => { quick_error!(ERROR_CHECK $imode $($tail)*); };
    (ERROR_CHECK $imode:tt description($expr:expr) $( $tail:tt )*)
    => { quick_error!(ERROR_CHECK $imode $($tail)*); };
    (ERROR_CHECK $imode:tt cause($expr:expr) $($tail:tt)*)
    => { quick_error!(ERROR_CHECK $imode $($tail)*); };
    (ERROR_CHECK $imode:tt from() $($tail:tt)*)
    => { quick_error!(ERROR_CHECK $imode $($tail)*); };
    (ERROR_CHECK $imode:tt from($ftyp:ty) $($tail:tt)*)
    => { quick_error!(ERROR_CHECK $imode $($tail)*); };
    (ERROR_CHECK TUPLE from($fvar:ident: $ftyp:ty) -> ($( $e:expr ),*) $( $tail:tt )*)
    => { quick_error!(ERROR_CHECK TUPLE $($tail)*); };
    (ERROR_CHECK STRUCT from($fvar:ident: $ftyp:ty) -> {$( $v:ident: $e:expr ),*} $( $tail:tt )*)
    => { quick_error!(ERROR_CHECK STRUCT $($tail)*); };
    (ERROR_CHECK $imode:tt ) => {};
    // Utility functions
    (IDENT $ident:ident) => { $ident }
}


#[cfg(test)]
mod test {
    use std::num::ParseFloatError;
    use std::str::Utf8Error;
    use std::string::FromUtf8Error;
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
        #[derive(Debug, PartialEq)]
        pub enum Wrapper {
            /// ParseFloat Error
            ParseFloatError(err: ParseFloatError) {
                from()
                description(err.description())
                display("parse float error: {err}", err=err)
                cause(err)
            }
            Other(descr: &'static str) {
                description(descr)
                display("Error: {}", descr)
            }
            /// FromUtf8 Error
            TupleFromUtf8Error(err: Utf8Error, source: Vec<u8>) {
                cause(err)
                display(me) -> ("{desc} at index {pos}: {err}", desc=me.description(), pos=err.valid_up_to(), err=err)
                description("utf8 error")
                from(err: FromUtf8Error) -> (err.utf8_error().clone(), err.into_bytes())
            }
            // Utf8 Error
            StructUtf8Error{ err: Utf8Error, hint: Option<&'static str> } {
                cause(err)
                display(me) -> ("{desc} at index {pos}: {err}", desc=me.description(), pos=err.valid_up_to(), err=err)
                description("utf8 error")
                from(err: Utf8Error) -> { err: err, hint: None }
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
    fn wrapper_err() {
        let cause = "one and a half times pi".parse::<f32>().unwrap_err();
        let err = Wrapper::ParseFloatError(cause.clone());
        assert_eq!(format!("{}", err), format!("parse float error: {}", cause));
        assert_eq!(format!("{:?}", err), format!("ParseFloatError({:?})", cause));
        assert_eq!(err.description(), cause.description());
        assert_eq!(format!("{:?}", err.cause().unwrap()), format!("{:?}", cause));
    }

    #[test]
    fn wrapper_trait_str() {
        let desc = "hello";
        let err: &Error = &Wrapper::Other(desc);
        assert_eq!(format!("{}", err), format!("Error: {}", desc));
        assert_eq!(format!("{:?}", err), format!("Other({:?})", desc));
        assert_eq!(err.description(), desc);
        assert!(err.cause().is_none());
    }

    #[test]
    fn wrapper_trait_two_fields() {
        let invalid_utf8: Vec<u8> = vec![0, 159, 146, 150];
        let cause = String::from_utf8(invalid_utf8.clone()).unwrap_err().utf8_error();
        let err: &Error = &Wrapper::TupleFromUtf8Error(cause.clone(), invalid_utf8.clone());
        assert_eq!(format!("{}", err), format!("{desc} at index {pos}: {cause}", desc=err.description(), pos=cause.valid_up_to(), cause=cause));
        assert_eq!(format!("{:?}", err), format!("TupleFromUtf8Error({:?}, {:?})", cause, invalid_utf8));
        assert_eq!(err.description(), "utf8 error");
        assert_eq!(format!("{:?}", err.cause().unwrap()), format!("{:?}", cause));
    }

    #[test]
    fn wrapper_struct() {
        let invalid_utf8: Vec<u8> = vec![0, 159, 146, 150];
        let cause = String::from_utf8(invalid_utf8.clone()).unwrap_err().utf8_error();
        let err: &Error = &Wrapper::StructUtf8Error{ err: cause.clone(), hint: Some("nonsense") };
        assert_eq!(format!("{}", err), format!("{desc} at index {pos}: {cause}", desc=err.description(), pos=cause.valid_up_to(), cause=cause));
        assert_eq!(format!("{:?}", err), format!("StructUtf8Error {{ err: {:?}, hint: {:?} }}", cause, Some("nonsense")));
        assert_eq!(err.description(), "utf8 error");
        assert_eq!(format!("{:?}", err.cause().unwrap()), format!("{:?}", cause));
    }

    #[test]
    fn wrapper_from() {
        let cause = "one and a half times pi".parse::<f32>().unwrap_err();
        let err = Wrapper::ParseFloatError(cause.clone());
        let err_from: Wrapper = From::from(cause);
        assert_eq!(err_from, err);
    }

    #[test]
    fn wrapper_custom_from() {
        let invalid_utf8: Vec<u8> = vec![0, 159, 146, 150];
        let cause = String::from_utf8(invalid_utf8.clone()).unwrap_err();
        let err = Wrapper::TupleFromUtf8Error(cause.utf8_error().clone(), invalid_utf8);
        let err_from: Wrapper = From::from(cause);
        assert_eq!(err_from, err);
    }

    #[test]
    fn wrapper_struct_from() {
        let invalid_utf8: Vec<u8> = vec![0, 159, 146, 150];
        let cause = String::from_utf8(invalid_utf8.clone()).unwrap_err().utf8_error();
        let err = Wrapper::StructUtf8Error{ err: cause.clone(), hint: None };
        let err_from: Wrapper = From::from(cause);
        assert_eq!(err_from, err);
    }

    #[test]
    fn wrapper_discard() {
        let err: Wrapper = From::from("hello");
        assert_eq!(format!("{}", err), format!("Discard"));
        assert_eq!(format!("{:?}", err), format!("Discard"));
        assert_eq!(err.description(), "Discard");
        assert!(err.cause().is_none());
    }

    #[test]
    fn wrapper_singleton() {
        let err: Wrapper = Wrapper::Singleton;
        assert_eq!(format!("{}", err), format!("Just a string"));
        assert_eq!(format!("{:?}", err), format!("Singleton"));
        assert_eq!(err.description(), "Singleton");
        assert!(err.cause().is_none());
    }
}
