# Reader

Basic features:
 * [x] Parsing XML 1.0 documents and returning a stream of events
   - [x] Support for embedded entities
 * [x] Support for namespaces and emitting namespace information in events
 * Missing XML features
   - [x] Support for different encodings
   - [ ] Attribute values normalization
   - [ ] EOL characters normalization

# Writer

Basic features:
  * [x] Writing basic XML 1.0 documents in UTF-8
  * [x] Writing XML 1.0 documents with namespace support
  * [x] Support for writing elements with empty body as empty elements
  * [x] Pretty-printed and compact output
  * [ ] Writing XML document with embedded DTDs and DTD references
  * Misc features:
    - [ ] Support for different encodings
    - [x] Support for writing CDATA as characters
    - [ ] Checking events for invalid characters (e.g. `--` in comments)
    - [ ] Check for namespaces more correctly, i.e. check both for prefix and namespace URI
    - [ ] Support checking namespace prefix presence in the current namespace for events with prefix but without namespace
    - [ ] Support checking namespace prefix for events with both prefix and namespace URI
