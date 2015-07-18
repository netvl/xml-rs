xml-rs, an XML library for Rust
===============================

[![Build Status](https://travis-ci.org/netvl/xml-rs.svg?branch=master)](https://travis-ci.org/netvl/xml-rs) [![crates.io](https://img.shields.io/crates/v/xml-rs.svg)](https://crates.io/crates/xml-rs)

[Documentation](https://netvl.github.io/xml-rs)

xml-rs is an XML library for [Rust](http://www.rust-lang.org/) programming language.
It is heavily inspired by Java stream-based XML API (StAX).

This library currently contains pull parser much like [StAX event reader](http://docs.oracle.com/javase/7/docs/api/javax/xml/stream/XMLEventReader.html).
It provides iterator API, so you can leverage Rust's existing iterators library features.

This parser is mostly full-featured, however, there are limitation:
* no other encodings but UTF-8 are supported yet, because no stream-based encoding library
  is available now; when (or if) one will be available, I'll try to make use of it;
* DTD validation is not supported, `<!DOCTYPE>` declarations are completely ignored; thus no
  support for custom entities too; internal DTD declarations are likely to cause parsing errors;
* attribute value normalization is not performed, and end-of-line characters are not normalized too.

Other than that the parser tries to be mostly XML-1.0-compliant.

What is planned (highest priority first):

0. XML emitter, that is, an analog of [StAX event writer](http://docs.oracle.com/javase/7/docs/api/javax/xml/stream/XMLEventWriter.html),
   including pretty printing;
1. parsing into a DOM tree and its serialization back to XML text;
2. SAX-like callback-based parser (fairly easy to implement over pull parser);
3. some kind of test infrastructure;
4. more convenience features, like filtering over produced events;
5. missing features required by XML standard (e.g. aforementioned normalization);
6. DTD validation;
7. (let's dream a bit) XML Schema validation.

Hopefully XML emitter will be implemented soon. This will allow easy stream processing, for example,
transformation of large XML documents.

Building and using
------------------

xml-rs uses [Cargo](http://crates.io), so just add a dependency section in your project's manifest:

```toml
[dependencies]
xml-rs = "*"
```

Parsing
-------

`xml::reader::EventReader` requires a `Read` instance to read from. When a proper stream-based encoding 
library is available, it is likely that xml-rs will be switched to use whatever character stream structure
this library would provide, but currently it is a `Reader`. However, there are several static methods
which allow to create a parser from string or a byte vector.

`EventReader` usage is very straightforward. Just provide a `Read` instance and then create an iterator
over events:

```rust
extern crate xml;

use std::fs::File;
use std::io::BufReader;

use xml::reader::EventReader;
use xml::reader::events::*;

fn indent(size: usize) -> String {
    const INDENT: &'static str = "    ";
    (0..size).map(|_| INDENT)
             .fold(String::with_capacity(size*INDENT.len()), |r, s| r + s)
}

fn main() {
    let file = File::open("file.xml").unwrap();
    let file = BufReader::new(file);

    let mut parser = EventReader::new(file);
    let mut depth = 0;
    for e in parser.events() {
        match e {
            XmlEvent::StartElement { name, .. } => {
                println!("{}+{}", indent(depth), name);
                depth += 1;
            }
            XmlEvent::EndElement { name } => {
                depth -= 1;
                println!("{}-{}", indent(depth), name);
            }
            XmlEvent::Error(e) => {
                println!("Error: {}", e);
                break;
            }
            _ => {}
        }
    }
}
```

`events()` should be called only once, that is, every instance of an iterator it returns will always
use the same underlying parser (TODO: make consuming iterator). Document parsing can end normally or with an 
error. Regardless of exact cause, the parsing process will be stopped, and iterator will terminate normally.

You can also have finer control over when to pull the next event from the parser using its own
`next()` method:

```rust
match parser.next() {
    ...
}
```

Upon end of the document or an error the parser will rememeber that last event and will always
return it in the result of `next()` call afterwards.

It is also possible to tweak parsing process a little using `xml::reader::ParserConfig` structure. See
its documentation for more information and examples.

Other things
------------

No performance tests or measurements are done. The implementation is rather naive, and no specific
optimizations are made. Hopefully the library is sufficiently fast to process documents of common size.

License
-------

This library is licensed under MIT license. Feel free to post found issues on GitHub issue tracker:
<http://github.com/netvl/xml-rs/issues>.

---
Copyright (C) Vladimir Matveev, 2014 

