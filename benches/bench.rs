#![feature(test)]

extern crate test;
use test::Bencher;
use xml::{EventReader, EventWriter};

#[bench]
fn read(bencher: &mut Bencher) {
    let xml = std::fs::read("tests/documents/sample_1.xml").unwrap();
    bencher.iter(move || {
        let parser = EventReader::new(xml.as_slice());
        for e in parser {
            e.unwrap();
        }
    });
}

#[bench]
fn write(bencher: &mut Bencher) {
    let xml = std::fs::read("tests/documents/sample_1.xml").unwrap();
    let events: Vec<_> = EventReader::new(xml.as_slice()).into_iter().map(|e| e.unwrap()).collect();
    let events: Vec<_> = events.iter().filter_map(|e| e.as_writer_event()).collect();

    bencher.iter(move || {
        let mut serializer = EventWriter::new(Vec::new());
        for e in &events {
            serializer.write((*e).clone()).unwrap();
        }
        serializer.into_inner()
    });
}
