use std::fmt;
use std::fs::File;
use std::io::{BufReader, Write};
use std::path::{Path, PathBuf};
use std::str::FromStr;

use itertools::Itertools;

use regex::Regex;
use xml::event::XmlEvent;

#[test]
fn oasis_tests() {
    run_xml_tests("tests/xml_test_suite/oasis/oasis.xml");
}

fn run_xml_tests(suite_path: &str) {
    let wide_whitespace = Regex::new("\\n|\\s{2,}").unwrap();

    let suite_path = Path::new(suite_path);

    let input = File::open(suite_path).unwrap();
    let input = BufReader::new(input);
    let mut input = xml::reader::ReaderConfig::new().create_reader_from_buf_read(input);

    let mut current_test = None;
    let mut test_cases = Vec::new();
    loop {
        let event = input.next().unwrap();
        match event {
            XmlEvent::EndDocument => break,

            XmlEvent::StartElement { name, attributes } if current_test.is_none() && name.local_name == "TEST" => {
                let mut test_case = TestCase {
                    id: String::new(),
                    type_: TestCaseType::Valid,
                    sections: String::new(),
                    path: PathBuf::new(),
                    description: String::new(),
                };

                for attr in attributes {
                    match attr.name.local_name.as_ref() {
                        "ID" => test_case.id = attr.value.into_owned(),
                        "TYPE" => test_case.type_ = attr.value.parse().unwrap(),
                        "SECTIONS" => test_case.sections = attr.value.into_owned(),
                        "URI" => test_case.path = suite_path.parent().unwrap().join(attr.value.as_ref()),
                        _ => {}
                    }
                }

                current_test = Some(test_case);
            }

            XmlEvent::Text(data) if current_test.is_some() => {
                let description_part = wide_whitespace.replace_all(data.trim(), " ");
                current_test.as_mut().unwrap().description.push_str(&description_part);
            }

            XmlEvent::EndElement { name } if current_test.is_some() && name.local_name == "TEST" => {
                test_cases.push(current_test.take().unwrap());
            }

            _ => {
                // ignore
            }
        }
    }

    let total_cases = test_cases.len();

    let mut failed_cases = Vec::new();
    for test_case in test_cases {
        let result = run_xml_test_case(test_case);
        if result.error.is_some() {
            failed_cases.push(result);
        }
    }

    println!(
        "Passed {}/{} test cases",
        (total_cases - failed_cases.len()),
        total_cases
    );

    if !failed_cases.is_empty() {
        let msg = failed_cases
            .into_iter()
            .map(|test_case_result| format!("{}", test_case_result))
            .join("\n");
        panic!("There are failed test cases:\n\n{}", msg);
    }
}

fn run_xml_test_case(test_case: TestCase) -> TestCaseResult {
    print!(
        "Running test case {} (sections: {}) (expected result: {}) [{}]... ",
        test_case.id, test_case.sections, test_case.type_, test_case.description
    );
    std::io::stdout().flush().unwrap();

    let input = File::open(&test_case.path).unwrap();
    let input = BufReader::new(input);
    let mut input = xml::reader::ReaderConfig::new().create_reader_from_buf_read(input);

    let mut error = None;
    let mut events = Vec::new();
    match test_case.type_ {
        TestCaseType::Valid => {
            while let Some(e) = input.fused_next() {
                if let Err(ref e) = e {
                    error = Some(format!("Test case failed, expected no errors, got error: {:?}", e));
                }
                events.push(e.map(|e| e.into_owned()));
            }
        }
        TestCaseType::Error | TestCaseType::NotWellFormed => {
            let mut encountered_error = false;
            while let Some(e) = input.fused_next() {
                if let Err(_) = e {
                    encountered_error = true;
                }
                events.push(e.map(|e| e.into_owned()));
            }
            if !encountered_error {
                error = Some("Test case failed, expected an error but got no errors".into());
            }
        }
        TestCaseType::Invalid => {
            println!("Unsupported!");
            return TestCaseResult {
                test_case,
                error: None,
                events: Vec::new(),
            };
        }
    }

    if error.is_none() {
        println!("OK");
    } else {
        println!("Failed!");
    }

    TestCaseResult {
        test_case,
        error,
        events,
    }
}

#[derive(Copy, Clone, Debug)]
enum TestCaseType {
    Valid,
    Invalid,
    NotWellFormed,
    Error,
}

impl TestCaseType {
    fn label(self) -> &'static str {
        match self {
            TestCaseType::Valid => "valid",
            TestCaseType::Invalid => "invalid",
            TestCaseType::NotWellFormed => "not-wf",
            TestCaseType::Error => "error",
        }
    }
}

impl fmt::Display for TestCaseType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(self.label())
    }
}

impl FromStr for TestCaseType {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "valid" => Ok(TestCaseType::Valid),
            "not-wf" => Ok(TestCaseType::NotWellFormed),
            "invalid" => Ok(TestCaseType::Invalid),
            "error" => Ok(TestCaseType::Error),
            _ => Err(format!("Invalid test case type: {}", s)),
        }
    }
}

#[derive(Debug)]
struct TestCase {
    id: String,
    type_: TestCaseType,
    sections: String,
    path: PathBuf,
    description: String,
}

#[derive(Debug)]
struct TestCaseResult {
    test_case: TestCase,
    error: Option<String>,
    events: Vec<Result<XmlEvent<'static>, xml::reader::Error>>,
}

impl fmt::Display for TestCaseResult {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} [{}]: {}",
            self.test_case.id,
            self.test_case.description,
            match self.error {
                Some(ref e) => e.as_str(),
                None => "Test case successful",
            }
        )?;
        if !self.events.is_empty() {
            writeln!(f, "\nEvents:")?;
            for event in &self.events {
                writeln!(f, "  {:?}", event)?;
            }
        }
        Ok(())
    }
}
