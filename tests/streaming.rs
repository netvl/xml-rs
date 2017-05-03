extern crate xml;

use std::io::{Cursor, Write};

use xml::EventReader;
use xml::reader::XmlEvent;

macro_rules! assert_match {
    ($actual:expr, $expected:pat) => {
        match $actual {
            $expected => {},
            _ => panic!("assertion failed: `(left matches right)` \
                        (left: `{:?}`, right: `{}`", $actual, stringify!($expected))
        }
    };
    ($actual:expr, $expected:pat if $guard:expr) => {
        match $actual {
            $expected if $guard => {},
            _ => panic!("assertion failed: `(left matches right)` \
                        (left: `{:?}`, right: `{} if {}`",
                        $actual, stringify!($expected), stringify!($guard))
        }
    }
}

fn write_and_reset_position<W>(c: &mut Cursor<W>, data: &[u8]) where Cursor<W>: Write {
    let p = c.position();
    c.write_all(data).unwrap();
    c.set_position(p);
}

#[test]
fn reading_streamed_content() {
    let buf = Cursor::new(b"<root>".to_vec());
    let reader = EventReader::new(buf);

    let mut it = reader.into_iter();

    assert_match!(it.next(), Some(Ok(XmlEvent::StartDocument { .. })));
    assert_match!(it.next(), Some(Ok(XmlEvent::StartElement { ref name, .. })) if name.local_name == "root");

    write_and_reset_position(it.source_mut(), b"<child-1>content</child-1>");
    assert_match!(it.next(), Some(Ok(XmlEvent::StartElement { ref name, .. })) if name.local_name == "child-1");
    assert_match!(it.next(), Some(Ok(XmlEvent::Characters(ref c))) if c == "content");
    assert_match!(it.next(), Some(Ok(XmlEvent::EndElement { ref name })) if name.local_name == "child-1");

    write_and_reset_position(it.source_mut(), b"<child-2/>");
    assert_match!(it.next(), Some(Ok(XmlEvent::StartElement { ref name, .. })) if name.local_name == "child-2");
    assert_match!(it.next(), Some(Ok(XmlEvent::EndElement { ref name })) if name.local_name == "child-2");

    // doesn't seem to work because of how tags parsing is done
//    write_and_reset_position(it.source_mut(), b"some text");
//    assert_match!(it.next(), Some(Ok(XmlEvent::Characters(ref c))) if c == "some text");

    write_and_reset_position(it.source_mut(), b"</root>");
    assert_match!(it.next(), Some(Ok(XmlEvent::EndElement { ref name })) if name.local_name == "root");
    assert_match!(it.next(), Some(Ok(XmlEvent::EndDocument)));
    assert_match!(it.next(), None);
}
