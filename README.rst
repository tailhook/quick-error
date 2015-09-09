===========
Quick Error
===========

:Documentation: http://tailhook.github.io/quick-error/

A macro which makes error types pleasant to write.

Features:
* Define enum type with arbitrary parameters (not struct variants for now)
* Concise notation of ``Display`` and ``Error`` traits
* Full control of ``Display`` and ``Error`` trait implementation
* Any number of ``From`` traits

Here is the comprehensive example:

.. code-block:: rust

    quick_error! {
        #[derive(Debug)]
        pub enum IoWrapper {
            Io(err: io::Error) {
                from()
                description("io error")
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
                from(s: String) -> ("some string",
                                    io::Error::new(io::ErrorKind::Other, s))
            }
            Discard {
                from(&'static str)
            }
        }
    }
