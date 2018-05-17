#[macro_use]
extern crate criterion;
extern crate quickcheck;
extern crate rand;
extern crate xml;

use std::io::Cursor;

use xml::reader_ng::encodings::{DelimitingReader, Buffer, StrBuffer};
use xml::encoding_rs::UTF_8;

use criterion::Criterion;

fn make_decoder(data: Vec<u8>) -> DelimitingReader<'static, 'static, Cursor<Vec<u8>>> {
    DelimitingReader::new(
        Cursor::new(data),
        UTF_8,
        Buffer::new_owned(8192),
        StrBuffer::new_owned(8192)
    )
}

fn gen_data(size: usize) -> Vec<u8> {
    use quickcheck::Arbitrary;

    let mut gen = quickcheck::StdGen::new(rand::thread_rng(), size);

    let mut s = String::with_capacity(size);
    for _ in 0..s.capacity() {
        s.push(char::arbitrary(&mut gen));
    }

    s.into_bytes()
}

const KB: usize = 1024;

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function_over_inputs(
        "decode_utf8",
        |b, &size| b.iter_with_setup(
            || make_decoder(gen_data(size)),
            |mut reader| {
                let mut target = String::new();
                while reader.read_until('-', &mut target).unwrap() {
                }
            }
        ),
        vec![10*KB, 100*KB, 1000*KB]
    );
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
