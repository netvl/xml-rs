#![feature(test)]

extern crate test;
use test::Bencher;
use xml::EventReader;

#[bench]
fn bla(bencher: &mut Bencher) {
    let xml = std::fs::read("tests/documents/sample_1.xml").unwrap();
    bencher.iter(move || {
        let parser = EventReader::new(xml.as_slice());
        for e in parser {
            e.unwrap();
        }
    });
}
