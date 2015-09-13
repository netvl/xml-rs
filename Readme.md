xml-rs, an XML library for Rust
===============================

[![Build Status][build-status-img]](https://travis-ci.org/netvl/xml-rs)
[![crates.io][crates-io-img]](https://crates.io/crates/xml-rs)

[Documentation](https://netvl.github.io/xml-rs)

  [build-status-img]: https://travis-ci.org/netvl/xml-rs.svg?branch=master
  [crates-io-img]: https://img.shields.io/crates/v/xml-rs.svg

xml-rs is an XML library for [Rust](http://www.rust-lang.org/) programming language.
It is heavily inspired by Java [Streaming API for XML (StAX)][stax].

  [stax]: https://en.wikipedia.org/wiki/StAX

This library currently contains pull parser much like [StAX event reader][stax-reader].
It provides iterator API, so you can leverage Rust's existing iterators library features.

  [stax-reader]: http://docs.oracle.com/javase/8/docs/api/javax/xml/stream/XMLEventReader.html

It also provides a streaming document writer much like [StAX event writer][stax-writer].
This writer consumes its own set of events, but reader events can be converted to
writer events easily, and so it is possible to write XML transformation chains in a pretty
clean manner.

This parser is mostly full-featured, however, there are limitations:
* no other encodings but UTF-8 are supported yet, because no stream-based encoding library
  is available now; when (or if) one will be available, I'll try to make use of it;
* DTD validation is not supported, `<!DOCTYPE>` declarations are completely ignored; thus no
  support for custom entities too; internal DTD declarations are likely to cause parsing errors;
* attribute value normalization is not performed, and end-of-line characters are not normalized too.

Other than that the parser tries to be mostly XML-1.0-compliant.

Writer is also mostly full-featured with the following limitations:
* no support for encodings other than UTF-8, for the same reason as above;
* no support for emitting `<!DOCTYPE>` declarations;
* more validations of input are needed, for example, checking that namespace prefixes are bounded
  or comments are well-formed.

What is planned (highest priority first, approximately):

0. finish miscellaneous features of the writer;
1. parsing into a DOM tree and its serialization back to XML text;
2. SAX-like callback-based parser (fairly easy to implement over pull parser);
3. some kind of test infrastructure;
4. missing features required by XML standard (e.g. aforementioned normalization);
5. DTD validation;
6. (let's dream a bit) XML Schema validation.

  [stax-writer]: http://docs.oracle.com/javase/8/docs/api/javax/xml/stream/XMLEventWriter.html

Building and using
------------------

xml-rs uses [Cargo](http://crates.io), so just add a dependency section in your project's manifest:

```toml
[dependencies]
xml-rs = "*"
```

The package exposes a single crate called `xml`:

```rust
extern crate xml;
```

Reading XML documents
---------------------

`xml::reader::EventReader` requires a `Read` instance to read from. When a proper stream-based encoding
library is available, it is likely that xml-rs will be switched to use whatever character stream structure
this library would provide, but currently it is a `Read`.

Using `EventReader` is very straightforward. Just provide a `Read` instance to obtain an iterator
over events:

```rust
extern crate xml;

use std::fs::File;

use xml::reader::{EventReader, XmlEvent};

fn indent(size: usize) -> String {
    const INDENT: &'static str = "    ";
    (0..size).map(|_| INDENT)
             .fold(String::with_capacity(size*INDENT.len()), |r, s| r + s)
}

fn main() {
    let file = File::open("file.xml").unwrap();

    let parser = EventReader::new(file);
    let mut depth = 0;
    for e in parser {
        match e {
            Ok(XmlEvent::StartElement { name, .. }) => {
                println!("{}+{}", indent(depth), name);
                depth += 1;
            }
            Ok(XmlEvent::EndElement { name }) => {
                depth -= 1;
                println!("{}-{}", indent(depth), name);
            }
            Err(e) => {
                println!("Error: {}", e);
                break;
            }
            _ => {}
        }
    }
}
```

`EventReader` implements `IntoIterator` trait, so you can just use it in a `for` loop directly.
Document parsing can end normally or with an error. Regardless of exact cause, the parsing
process will be stopped, and iterator will terminate normally.

You can also have finer control over when to pull the next event from the parser using its own
`next()` method:

```rust
match parser.next() {
    ...
}
```

Upon the end of the document or an error the parser will remember that last event and will always
return it in the result of `next()` call afterwards. If iterator is used, then it will yield
error or end-of-document event once and will produce `None` afterwards.

It is also possible to tweak parsing process a little using `xml::reader::ParserConfig` structure.
See its documentation for more information and examples.

Writing XML documents
---------------------

xml-rs also provides a streaming writer much like StAX event writer. With it you can write an
XML document to any `Write` implementor.

```rust
extern crate xml;

use std::fs::File;
use std::io::{self, Write};

use xml::writer::{EventWriter, EmitterConfig, XmlEvent, Result};

fn handle_event<W: Write>(w: &mut EventWriter<W>, line: String) -> Result<()> {
    let line = line.trim();
    let event: XmlEvent = if line.starts_with("+") && line.len() > 1 {
        XmlEvent::start_element(&line[1..]).into()
    } else if line.starts_with("-") {
        XmlEvent::end_element().into()
    } else {
        XmlEvent::characters(&line).into()
    };
    w.write(event)
}

fn main() {
    let mut file = File::create("output.xml").unwrap();

    let mut input = io::stdin();
    let mut output = io::stdout();
    let mut writer = EmitterConfig::new().perform_indent(true).create_writer(&mut file);
    loop {
        print!("> "); output.flush().unwrap();
        let mut line = String::new();
        match input.read_line(&mut line) {
            Ok(0) => break,
            Ok(_) => match handle_event(&mut writer, line) {
                Ok(_) => {}
                Err(e) => panic!("Write error: {}", e)
            },
            Err(e) => panic!("Input error: {}", e)
        }
    }
}
```

The code example above also demonstrates how to create a writer out of its configuration.
Similar thing also works with `EventReader`.

The library provides an XML event building DSL which helps to construct complex events,
e.g. ones having namespace definitions. Some examples:

```rust
// <a:hello a:param="value" xmlns:a="urn:some:document">
XmlEvent::start_element("a:hello").attr("a:param", "value").ns("a", "urn:some:document")

// <hello b:config="name" xmlns="urn:default:uri">
XmlEvent::start_element("hello").attr("b:config", "value").default_ns("urn:defaul:uri")

// <![CDATA[some unescaped text]]>
XmlEvent::cdata("some unescaped text")
```

Of course, one can create `XmlEvent` enum variants directly instead of using the builder DSL.
There are more examples in `xml::writer::XmlEvent` documentation.

The writer has multiple configuration options; see `EmitterConfig` documentation for more
information.

Other things
------------

No performance tests or measurements are done. The implementation is rather naive, and no specific
optimizations are made. Hopefully the library is sufficiently fast to process documents of common size.
I intend to add benchmarks in future, but not until more important features are added.

Known issues
------------

All known issues are present on GitHub issue tracker: <http://github.com/netvl/xml-rs/issues>.
Feel free to post any found problems there.

License
-------

This library is licensed under MIT license.

---
Copyright (C) Vladimir Matveev, 2014-2015
