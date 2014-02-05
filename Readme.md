rust-xml, an XML library for Rust
=================================

`rust-xml` is an XML library for [Rust](http://www.rust-lang.org/) programming language.
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
0. XML emitter, that is, an analog of [StAX event writer](http://docs.oracle.com/javase/7/docs/api/javax/xml/stream/XMLEventReader.html),
   including pretty printing;
1. parsing into a DOM tree and its serialization back to XML text;
2. SAX-like callback-based parser (fairly easy to implement over pull parser);
3. more convenience features, like filtering over produced events;
4. missing features required by XML standard (e.g. aforementioned normalization);
5. DTD validation;
6. (let's dream for a bit) XML Schema validation.

Hopefully XML emitter will be implemented soon. This will allow easy stream processing, for example,
transformation of large XML documents.

This library is written for 0.9 version of Rust language. I have not decided yet on my strategy
of updating it to follow Rust development.

Parsing
-------

`xml::pull::Parser` requires a `Buffer` to read from. When proper stream-based encoding library
will be available, it is likely that it will be switched to use whatever character stream structure
this library will provide, but currently it is a `Buffer`. However, there are several static methods
which allow to create a parser from string or a byte vector.

`Parser` usage is very straightforward. Just provide a `Buffer` and then create an iterator
over events:

```rust
use std::io::File;
use std::io::buffered::BufferedReader;
use std::str;

use xml::pull::Parser;

fn indent(mut size: uint) -> ~str {
    let mut result = str::with_capacity(size*4);
    while size > 0 {
        result.push_str("    ");
        size -= 1;
    }
    result
}

fn main() {
    let file = File::open(&Path::new("file.xml"));
    let reader = BufferedReader::new(file);

    let mut parser = Parser::new(reader);
    let mut depth = 0;
    for e in parser.events() {
        match e {
            StartElement(name, _, _) => {
                println!("{}{}", indent(depth), name);
                depth += 1;
            }
            EndElement(name) => {
                depth -= 1;
                println!("{}/{}", indent(depth), name);
            }
            Error(e) => {
                println!("Error: {}", e.to_str());
                break
            }
        }
    }
}
```

`events()` should be called only once, that is, every instance of an iterator it returns will always
use the same underlying parser. Document parsing can end normally or with an error. Regardless of
exact cause, the parsing process will be stopped, and iterator will terminate normally.

You can also have finer control over when to pull the next event from the parser using its own
`next()` method:

    match parser.next() {
        ...
    }

Upon end of document or an error encounter the parser will rememeber that last event and will always
return it in the result of `next()` call afterwards.

It is also possible to tweak parsing process a little using `xml::pull::ParserConfig` structure. See
its documentation for more information and examples.

Other things
------------
This library is licensed under MIT license. Feel free to post found issues on github issue tracker:
[http://github.com/dpx-infinity/rust-xml/issues].

---
Copyright (C) Vladimir Matveev, 2014 

