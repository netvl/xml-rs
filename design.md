# Reader

Basic features:
 * [x] Parsing XML 1.0 documents and returning a stream of events
   - [ ] Support reading embedded DTD schemas
   - [ ] Support for embedded entities
 * [x] Support for namespaces and emitting namespace information in events
 * [ ] \[maybe\] push-based wrapper
 * Missing XML features
   - [ ] Support for different encodings
   - [ ] Attribute values normalization
   - [ ] EOL characters normalization

Advanced features:
 * [ ] DTD schema validation
 * [ ] XSD schema validation

# Writer

Basic features:
  * [x] Writing basic XML 1.0 documents in UTF-8
  * [x] Writing XML 1.0 documents with namespace support
  * [ ] Support for writing elements with empty body as empty elements
  * [ ] Pretty-printed and compact output
  * Misc features:
    - [ ] Support for different encodings
    - [ ] Support for writing CDATA as characters

# Other

DOM-based API:
 * [ ] Support for DOM-based API
