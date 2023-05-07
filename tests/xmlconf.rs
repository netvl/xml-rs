//! W3C XML conformance test suite https://www.w3.org/XML/Test/

use std::collections::HashSet;
use std::path::Path;
use std::collections::HashMap;
use std::fs::File;
use std::io::BufReader;
use std::process::Command;
use std::sync::Mutex;

use xml::EventReader;
use xml::reader::XmlEvent;

static UNZIP: Mutex<()> = Mutex::new(());

fn ensure_unzipped() {
    let _g = UNZIP.lock().expect("unzip already failed");

    // test suite license only allows redistribution of unmodified zip!
    if !Path::new("tests/xmlconf").exists() {
        assert!(Command::new("unzip")
            .current_dir("tests")
            .arg("xmlts20130923.zip")
            .status().unwrap().success(), "must unzip");
    }
}

#[track_caller]
fn run_suite(suite_rel_path: &str) {
    ensure_unzipped();

    let suite_path = Path::new("tests/xmlconf").join(suite_rel_path);
    let known_failures_file_path = Path::new("tests").join(suite_path.with_extension("fail.txt").file_name().unwrap());
    let mut new_known_failures_file = if std::env::var("PRINT_SPEC").map_or(false, |val| val == "1") { Some(String::new()) } else { None };

    let known_broken_test_ids: HashSet<_> = std::fs::read_to_string(&known_failures_file_path).unwrap_or_default().lines()
        .map(|l| l.trim().split(' ').next().unwrap().to_string()).collect();

    let root = suite_path.parent().unwrap();
    let mut parsed = 0;

    let f = BufReader::new(File::open(&suite_path)
        .map_err(|e| format!("{}: {e}", suite_path.display())).unwrap());
    let r = EventReader::new(f);
    let mut desc = String::new();
    let mut attr = HashMap::<String, String>::new();
    for e in r {
        let e = e.expect("testsuite validity");
        match e {
            XmlEvent::Characters(chr) => {
                desc.push_str(&chr.replace('\n', " ").replace("  ", " ").replace("  ", " "));
            },
            XmlEvent::EndElement { name } if name.local_name == "TEST" => {
                let path = root.join(&attr["URI"]);
                let test_type = attr["TYPE"].as_str();
                let id = attr["ID"].as_str();

                let res = match test_type {
                    "valid" => expect_well_formed(&path, &desc),
                    "invalid" => expect_well_formed(&path, &desc), // invalid is still well-formed
                    "not-wf" | "error" => expect_ill_formed(&path, &desc),
                    other => unimplemented!("{other}?? type"),
                };

                if let Some(out) = new_known_failures_file.as_mut() {
                    if let Err(e) = res {
                        use std::fmt::Write;
                        write!(out, "{id} {e}").unwrap();
                    }
                } else {
                    let known_bad = known_broken_test_ids.contains(id);
                    match res {
                        Err(_) if known_bad => {},
                        Err(e) => panic!("{suite_rel_path} failed on {} ({id})\n{e}", path.display()),
                        Ok(()) if known_bad => panic!("expected {} ({id}) to fail, but it passes {test_type} of {suite_rel_path} now\n{desc}", path.display()),
                        Ok(()) => {},
                    };
                }

                parsed += 1;
            },
            XmlEvent::StartElement { name, attributes, namespace: _ } if name.local_name == "TEST" => {
                desc.clear();
                attr = attributes.into_iter().map(|a| (a.name.local_name, a.value)).collect();
            },
            _ => {},

        }
    }
    if let Some(out) = new_known_failures_file {
        std::fs::write(known_failures_file_path, out).unwrap();
    }
    assert!(parsed > 0);
}

#[track_caller]
fn expect_well_formed(xml_path: &Path, msg: &str) -> Result<(), Box<dyn std::error::Error>> {
    let f = BufReader::new(File::open(xml_path)?);
    let r = EventReader::new(f);
    let mut seen_any = false;
    for e in r {
        let e = e.map_err(|e| format!("{} {msg}; {e}\n", xml_path.file_name().and_then(|f| f.to_str()).unwrap()))?;
        if let XmlEvent::EndElement { .. } = e {
            seen_any = true;
        }
    }
    if !seen_any { Err("no elements found")? }
    Ok(())
}

#[track_caller]
fn expect_ill_formed(xml_path: &Path, msg: &str) -> Result<(), Box<dyn std::error::Error>> {
    let f = BufReader::new(File::open(xml_path)?);
    let r = EventReader::new(f);
    for e in r {
        if let Err(_) = e {
            return Ok(());
        }
    }
    Err(format!("{} {msg}\n", xml_path.file_name().and_then(|f| f.to_str()).unwrap()))?
}

#[test] fn eduni_errata_2e() {
    run_suite("eduni/errata-2e/errata2e.xml");
}

#[test] fn eduni_errata_3e() {
    run_suite("eduni/errata-3e/errata3e.xml");
}

#[test] fn eduni_errata_4e() {
    run_suite("eduni/errata-4e/errata4e.xml");
}

#[test] fn eduni_misc_ht() {
    run_suite("eduni/misc/ht-bh.xml");
}

#[test] fn eduni_namespaces_10() {
    run_suite("eduni/namespaces/1.0/rmt-ns10.xml");
}

#[test] fn eduni_namespaces_11() {
    run_suite("eduni/namespaces/1.1/rmt-ns11.xml");
}

#[test] fn eduni_namespaces_errata() {
    run_suite("eduni/namespaces/errata-1e/errata1e.xml");
}

#[test] fn eduni_xml_11() {
    run_suite("eduni/xml-1.1/xml11.xml");
}

#[test] fn ibm_oasis_valid() {
    run_suite("ibm/ibm_oasis_valid.xml");
}

#[test] fn ibm_xml_11() {
    run_suite("ibm/xml-1.1/ibm_valid.xml");
}

#[test] fn oasis() {
    run_suite("oasis/oasis.xml");
}

#[test] fn sun_valid() {
    run_suite("sun/sun-valid.xml");
}

#[test] fn sun_ill_formed() {
    run_suite("sun/sun-not-wf.xml");
}

#[ignore]
#[test] fn japanese() {
    run_suite("japanese/japanese.xml");
}

#[test] fn xmltest() {
    run_suite("xmltest/xmltest.xml");
}

