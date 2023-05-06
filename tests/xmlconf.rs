//! W3C XML conformance test suite https://www.w3.org/XML/Test/

use std::collections::HashSet;
use std::ffi::OsStr;
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
fn run_suite(suite_rel_path: &str, known_broken_tests: &[&str]) {
    ensure_unzipped();

    let known_broken_tests = known_broken_tests.iter().map(|name| name.as_ref()).collect::<HashSet<&OsStr>>();

    let suite_path = Path::new("tests/xmlconf").join(suite_rel_path);
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
                desc.push_str(&chr.replace('\n', " "));
            },
            XmlEvent::EndElement { name } if name.local_name == "TEST" => {
                let path = root.join(&attr["URI"]);
                let test_type = attr["TYPE"].as_str();

                let res = match test_type {
                    "valid" => expect_well_formed(&path, &desc),
                    "invalid" => expect_well_formed(&path, &desc), // invalid is still well-formed
                    "not-wf" | "error" => expect_ill_formed(&path, &desc),
                    other => unimplemented!("{other}?? type"),
                };

                let id = attr["ID"].as_str();
                let known_bad = known_broken_tests.contains::<OsStr>(id.as_ref());

                match res {
                    Err(_) if known_bad => {},
                    Err(e) => panic!("{suite_rel_path} failed on {} ({id})\n{e}", path.display()),
                    Ok(()) if known_bad => panic!("expected {} ({id}) to fail, but it passes {test_type} of {suite_rel_path} now", path.display()),
                    Ok(()) => {},
                };
                parsed += 1;
            },
            XmlEvent::StartElement { name, attributes, namespace: _ } if name.local_name == "TEST" => {
                desc.clear();
                attr = attributes.into_iter().map(|a| (a.name.local_name, a.value)).collect();
            },
            _ => {},

        }
    }
    assert!(parsed > 0);
}

#[track_caller]
fn expect_well_formed(xml_path: &Path, msg: &str) -> Result<(), Box<dyn std::error::Error>> {
    let f = BufReader::new(File::open(xml_path)?);
    let r = EventReader::new(f);
    let mut seen_any = false;
    for e in r {
        let e = e.map_err(|e| format!("\"{}\", // {msg}; {e}", xml_path.display()))?;
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
    Err(format!("\"{}\", // {msg}", xml_path.display()))?
}

#[test] fn eduni_errata_2e() {
    run_suite("eduni/errata-2e/errata2e.xml", &[
        "rmt-e2e-15a", // Empty content can't contain an entity reference
        "rmt-e2e-15e", // Element content can contain entity reference if replacement text is whitespace
        "rmt-e2e-15f", // Element content can contain entity reference if replacement text is whitespace, even if it came from a character reference in the literal entity value
        "rmt-e2e-15h", // Element content can't contain entity reference if replacement text is character reference to whitespace
        "rmt-e2e-18", // External entity containing start of entity declaration is base URI for system identifier
        "rmt-e2e-19", // Parameter entities and character references are included-in-literal, but general entities are bypassed.
        "rmt-e2e-22", // UTF-8 entities may start with a BOM
        "rmt-e2e-24", // Either the built-in entity or a character reference can be used to represent greater-than after two close-square-brackets
        "rmt-e2e-34", // A non-deterministic content model is an error even if the element type is not used.
        "rmt-e2e-50", // All line-ends are normalized, even those not passed to the application. NB this can only be tested effectively in XML 1.1, since CR is in the S production; in 1.1 we can use NEL which isn't.
        "rmt-e2e-55", // A reference to an unparsed entity in an entity value is an error rather than forbidden (unless the entity is referenced, of course)
        "rmt-e2e-57", // A value other than preserve or default for xml:space is an error
        "rmt-e2e-61", // (From John Cowan) An encoding declaration in ASCII specifying an encoding that is not compatible with ASCII (so the document is not in its declared encoding).  It should generate a fatal error.
    ]);
}

#[test] fn eduni_errata_3e() {
    run_suite("eduni/errata-3e/errata3e.xml", &[
        "rmt-e3e-12", // E12.xml Default values for attributes may not contain references to external entities.
        "rmt-e3e-13", // E13.xml Even internal parameter entity references are enough to make undeclared entities into mere validity errors rather than well-formedness errors.
    ]);
}

#[test] fn eduni_errata_4e() {
    run_suite("eduni/errata-4e/errata4e.xml", &[
        "invalid-bo-1", // inclbom_be.xml Byte order mark in general entity should go away (big-endian)
        "invalid-bo-2", // inclbom_le.xml Byte order mark in general entity should go away (little-endian)
        "invalid-bo-3", // incl8bom.xml Byte order mark in general entity should go away (utf-8)
        "invalid-bo-4", // inclbombom_be.xml Two byte order marks in general entity produce only one (big-endian)
        "invalid-bo-5", // inclbombom_le.xml Two byte order marks in general entity produce only one (little-endian)
        "invalid-bo-6", // incl8bombom.xml Two byte order marks in general entity produce only one (utf-8)
        "invalid-sa-140", // 140.xml Character '&#x309a;' is a CombiningChar, not a Letter, but as of 5th edition, may begin a name (c.f. xmltest/not-wf/sa/140.xml).
        "invalid-sa-141", // 141.xml As of 5th edition, character #x0E5C is legal in XML names (c.f. xmltest/not-wf/sa/141.xml).
        "x-rmt-008b", // 008.xml a document with version=1.7, legal in XML 1.0 from 5th edition
        "x-ibm-1-0.5-valid-P04-ibm04v01.xml", // ibm04v01.xml This test case covers legal NameStartChars character ranges plus discrete legal characters for production 04.
        "x-ibm-1-0.5-valid-P05-ibm05v01.xml", // ibm05v01.xml This test case covers legal Element Names as per production 5.
        "x-ibm-1-0.5-valid-P05-ibm05v03.xml", // ibm05v03.xml This test case covers legal Attribute (Names) as per production 5.
    ]);
}

#[test] fn eduni_misc_ht() {
    run_suite("eduni/misc/ht-bh.xml", &[]);
}

#[test] fn eduni_namespaces_10() {
    run_suite("eduni/namespaces/1.0/rmt-ns10.xml", &[
        "rmt-ns10-004", // Namespace name test: a relative URI (deprecated)
        "rmt-ns10-005", // Namespace name test: a same-document relative URI (deprecated)
        "rmt-ns10-009", // Namespace equality test: plain repetition
        "rmt-ns10-010", // Namespace equality test: use of character reference
        "rmt-ns10-012", // Namespace inequality test: equal after attribute value normalization
        "rmt-ns10-030", // Reserved prefixes and namespaces: binding another prefix to the xml namespace
        "rmt-ns10-033", // Reserved prefixes and namespaces: binding another prefix to the xmlns namespace
        "rmt-ns10-036", // Attribute uniqueness: repeated attribute with different prefixes
        "rmt-ns10-042", // Colon in PI name
        "rmt-ns10-043", // Colon in entity name
        "rmt-ns10-044", // Colon in entity name
        "ht-ns10-047", // Reserved name: _not_ an error
    ]);
}

#[test] fn eduni_namespaces_11() {
    run_suite("eduni/namespaces/1.1/rmt-ns11.xml", &[
        "rmt-ns11-001", // 001.xml Namespace name test: a perfectly good http IRI that is not a URI
        "rmt-ns11-002", // 002.xml Namespace inequality test: different escaping of non-ascii letter
        "rmt-ns11-003", // 003.xml 1.1 style prefix unbinding
        "rmt-ns11-004", // 004.xml 1.1 style prefix unbinding and rebinding
    ]);
}

#[test] fn eduni_namespaces_errata() {
    run_suite("eduni/namespaces/errata-1e/errata1e.xml", &[
        "rmt-ns-e1.0-13a", // NE13a.xml The xml namespace must not be declared as the default namespace.
        "rmt-ns-e1.0-13b", // NE13b.xml The xmlns namespace must not be declared as the default namespace.
    ]);
}

#[test] fn eduni_xml_11() {
    run_suite("eduni/xml-1.1/xml11.xml", &[
        "rmt-001", // 001.xml External subset has later version number
        "rmt-002", // 002.xml External PE has later version number
        "rmt-006", // 006.xml Second-level external general entity has later version number than first-level, but not later than document, so not an error.
        "rmt-010", // 010.xml Contains a C1 control, legal in XML 1.0, illegal in XML 1.1
        "rmt-013", // 013.xml Contains a DEL, legal in XML 1.0, illegal in XML 1.1
        "rmt-014", // 014.xml Has a "long s" in a name, legal in XML 1.1, illegal in XML 1.0 thru 4th edition
        "rmt-016", // 016.xml Has a Byzantine Musical Symbol Kratimata in a name, legal in XML 1.1, illegal in XML 1.0 thru 4th edition
        "rmt-019", // 019.xml Has the last legal namechar in XML 1.1, illegal in XML 1.0 thru 4th edition
        "rmt-022", // 022.xml Has a NEL character; legal in both XML 1.0 and 1.1, but different canonical output because of normalization in 1.1
        "rmt-023", // 023.xml Has a NEL character; legal in both XML 1.0 and 1.1, but different canonical output because of normalization in 1.1
        "rmt-026", // 026.xml Has CR-NEL; legal in both XML 1.0 and 1.1, but different canonical output because of normalization in 1.1
        "rmt-027", // 027.xml Has CR-NEL; legal in both XML 1.0 and 1.1, but different canonical output because of normalization in 1.1
        "rmt-030", // 030.xml Has a NEL character in an NMTOKENS attribute; well-formed in both XML 1.0 and 1.1, but valid only in 1.1
        "rmt-031", // 031.xml Has a NEL character in an NMTOKENS attribute; well-formed in both XML 1.0 and 1.1, but valid only in 1.1
        "rmt-034", // 034.xml Has an NMTOKENS attribute containing a CR character that comes from a character reference in an internal entity.  Because CR is in the S production, this is valid in both XML 1.0 and 1.1.
        "rmt-035", // 035.xml Has an NMTOKENS attribute containing a CR character that comes from a character reference in an internal entity.  Because CR is in the S production, this is valid in both XML 1.0 and 1.1.
        "rmt-036", // 036.xml Has an NMTOKENS attribute containing a NEL character that comes from a character reference in an internal entity.  Because NEL is not in the S production (even though real NELs are converted to LF on input), this is invalid in both XML 1.0 and 1.1.
        "rmt-037", // 037.xml Has an NMTOKENS attribute containing a NEL character that comes from a character reference in an internal entity.  Because NEL is not in the S production (even though real NELs are converted to LF on input), this is invalid in both XML 1.0 and 1.1.
        "rmt-038", // 038.xml Contains a C0 control character (form-feed), illegal in both XML 1.0 and 1.1
        "rmt-039", // 039.xml Contains a C0 control character (form-feed), illegal in both XML 1.0 and 1.1
        "rmt-040", // 040.xml Contains a C1 control character (partial line up), legal in XML 1.0 but not 1.1
        "rmt-042", // 042.xml Contains a character reference to a C0 control character (form-feed), legal in XML 1.1 but not 1.0
        "rmt-046", // 046.xml Has a NEL character in element content whitespace; well-formed in both XML 1.0 and 1.1, but valid only in 1.1
        "rmt-047", // 047.xml Has a NEL character in element content whitespace; well-formed in both XML 1.0 and 1.1, but valid only in 1.1
        "rmt-050", // 050.xml Has element content whitespace containing a CR character that comes from a character reference in an internal entity.  Because CR is in the S production, this is valid in both XML 1.0 and 1.1.
        "rmt-051", // 051.xml Has element content whitespace containing a CR character that comes from a character reference in an internal entity.  Because CR is in the S production, this is valid in both XML 1.0 and 1.1.
        "rmt-052", // 052.xml Has element content whitespace containing a NEL character that comes from a character reference in an internal entity.  Because NEL is not in the S production (even though real NELs are converted to LF on input), this is invalid in both XML 1.0 and 1.1.
        "rmt-053", // 053.xml Has element content whitespace containing a NEL character that comes from a character reference in an internal entity.  Because NEL is not in the S production (even though real NELs are converted to LF on input), this is invalid in both XML 1.0 and 1.1.
        "rmt-054", // 054.xml Contains a character reference to a C0 control character (form-feed) in an entity value.  This will be legal (in XML 1.1) when the entity declaration is parsed, but what about when it is used?
    ]);
}

#[test] fn ibm_oasis_valid() {
    run_suite("ibm/ibm_oasis_valid.xml", &[
        "ibm-valid-P09-ibm09v01.xml", // ibm09v01.xml Empty EntityValue is legal
        "ibm-valid-P09-ibm09v02.xml", // ibm09v02.xml Tests a normal EnitityValue
        "ibm-valid-P09-ibm09v03.xml", // ibm09v03.xml Tests EnitityValue referencing a Parameter Entity
        "ibm-valid-P09-ibm09v04.xml", // ibm09v04.xml Tests EnitityValue referencing a General Entity
        "ibm-valid-P09-ibm09v05.xml", // ibm09v05.xml Tests EnitityValue with combination of GE, PE and text, the GE used is      declared in the student.dtd
        "ibm-valid-P10-ibm10v01.xml", // ibm10v01.xml Tests empty AttValue with double quotes as the delimiters
        "ibm-valid-P10-ibm10v02.xml", // ibm10v02.xml Tests empty AttValue with single quotes as the delimiters
        "ibm-valid-P10-ibm10v03.xml", // ibm10v03.xml Test AttValue with double quotes as the delimiters and single quote inside
        "ibm-valid-P10-ibm10v04.xml", // ibm10v04.xml Test AttValue with single quotes as the delimiters and double quote inside
        "ibm-valid-P10-ibm10v05.xml", // ibm10v05.xml Test AttValue with a GE reference and double quotes as the delimiters
        "ibm-valid-P10-ibm10v06.xml", // ibm10v06.xml Test AttValue with a GE reference and single quotes as the delimiters
        "ibm-valid-P10-ibm10v07.xml", // ibm10v07.xml testing AttValue with mixed references and text content in double quotes
        "ibm-valid-P10-ibm10v08.xml", // ibm10v08.xml testing AttValue with mixed references and text content in single quotes
        "ibm-valid-P28-ibm28v02.xml", // ibm28v02.xml Tests doctypedecl with external subset and combinations of different markup     declarations and PEReferences
        "ibm-valid-P29-ibm29v01.xml", // ibm29v01.xml Tests markupdecl with combinations of elementdecl, AttlistDecl,EntityDecl,      NotationDecl, PI and comment
        "ibm-valid-P29-ibm29v02.xml", // ibm29v02.xml Tests WFC: PE in internal subset as a positive test
        "ibm-valid-P32-ibm32v02.xml", // ibm32v02.xml Tests VC: Standalone Document Declaration with external entity reference     and standalone is no
        "ibm-valid-P43-ibm43v01.xml", // ibm43v01.xml Tests content with all possible constructs: element, CharData, Reference,      CDSect, Comment
        "ibm-valid-P67-ibm67v01.xml", // ibm67v01.xml Tests Reference could be EntityRef or CharRef.
        "ibm-valid-P78-ibm78v01.xml", // ibm78v01.xml Tests ExtParsedEnt, also TextDecl in P77 and EncodingDecl in P80
    ]);
}

#[test] fn ibm_xml_11() {
    run_suite("ibm/xml-1.1/ibm_valid.xml", &[
        "ibm-1-1-valid-P02-ibm02v04.xml", // ibm02v04.xml This test case contains embeded whitespace characters                   some form the range 1 - 1F.
        "ibm-1-1-valid-P03-ibm03v01.xml", // ibm03v01.xml The two character sequence #x0D #x85 in an external entity must be normalized to a          single newline.
        "ibm-1-1-valid-P03-ibm03v02.xml", // ibm03v02.xml The single character sequence #x85 in an external entity must be normalized to a          single newline.
        "ibm-1-1-valid-P03-ibm03v03.xml", // ibm03v03.xml The two character sequence #x0D #x85 in an external entity must be normalized to a          single newline.
        "ibm-1-1-valid-P03-ibm03v04.xml", // ibm03v04.xml The single character sequence #x85 in an external entity must be normalized to a          single newline.
        "ibm-1-1-valid-P03-ibm03v05.xml", // ibm03v05.xml The two character sequence #x0D #x85 in a document entity must be normalized to a          single newline.
        "ibm-1-1-valid-P03-ibm03v06.xml", // ibm03v06.xml The single character sequence #x85 in a document entity must be normalized to a          single newline.
        "ibm-1-1-valid-P03-ibm03v07.xml", // ibm03v07.xml The single character sequence #x2028 in a document entity must be normalized to a          single newline.
        "ibm-1-1-valid-P04-ibm04v01.xml", // ibm04v01.xml This test case covers legal NameStartChars character ranges plus discrete legal          characters for production 04.
        "ibm-1-1-valid-P05-ibm05v01.xml", // ibm05v01.xml This test case covers legal Element Names as per production 5.
        "ibm-1-1-valid-P05-ibm05v03.xml", // ibm05v03.xml This test case covers legal Attribute (Names) as per production 5.
        "ibm-1-1-valid-P77-ibm77v04.xml", // ibm77v04.xml The VersionNum of the document entity is 1.1 whereas the VersionNum of the external          entity is 1.0.  The character #xD6 which is a valid XML 1.1 but an invalid XML 1.0          character is present in both documents.
        "ibm-1-1-valid-P77-ibm77v05.xml", // ibm77v05.xml The VersionNum of the document entity is 1.1 whereas the VersionNum of the external          entity is 1.0.  The character #x1FFF which is a valid XML 1.1 but an invalid XML 1.0          character is present in both documents.
        "ibm-1-1-valid-P77-ibm77v06.xml", // ibm77v06.xml The VersionNum of the document entity is 1.1 whereas the VersionNum of the external          entity is 1.0.  The character #xF901 which is a valid XML 1.1 but an invalid XML 1.0          character is present in both documents.
        "ibm-1-1-valid-P77-ibm77v10.xml", // ibm77v10.xml The VersionNum of the document and external entity is 1.1 and both contain the          valid XML1.1 but invalid XML1.0 character #xF6.
        "ibm-1-1-valid-P77-ibm77v11.xml", // ibm77v11.xml The VersionNum of the document and external entity is 1.1 and both contain the          valid XML1.1 but invalid XML1.0 character #x1FFF.
        "ibm-1-1-valid-P77-ibm77v12.xml", // ibm77v12.xml The VersionNum of the document and external entity is 1.1 and both contain the          valid XML1.1 but invalid XML1.0 character #xF901.
        "ibm-1-1-valid-P77-ibm77v16.xml", // ibm77v16.xml The VersionNum of the document entity is 1.1 but the external entity does not          contain a textDecl and both contain the valid XML1.1 but invalid XML1.0 character          #x2FF.
        "ibm-1-1-valid-P77-ibm77v17.xml", // ibm77v17.xml The VersionNum of the document entity is 1.1 but the external entity does not          contain a textDecl and both contain the valid XML1.1 but invalid XML1.0 character          #x1FFF.
        "ibm-1-1-valid-P77-ibm77v18.xml", // ibm77v18.xml The VersionNum of the document entity is 1.1 but the external entity does not          contain a textDecl and both contain the valid XML1.1 but invalid XML1.0 character          #xF901.
        "ibm-1-1-valid-P77-ibm77v22.xml", // ibm77v22.xml The VersionNum of the document and the external entity is 1.1.  The entity contains          a reference to the character #x7F.
        "ibm-1-1-valid-P77-ibm77v23.xml", // ibm77v23.xml The VersionNum of the document and the external entity is 1.1.  The entity contains          a reference to the character #x80.
        "ibm-1-1-valid-P77-ibm77v24.xml", // ibm77v24.xml The VersionNum of the document and the external entity is 1.1.  The entity contains          a reference to the character #x9F.
        "ibm-1-1-valid-P77-ibm77v28.xml", // ibm77v28.xml The VersionNum of the document is 1.1 and the textDecl is missing in the external          entity.  The replacement text of an entity declared in the external DTD contains a          reference to the character #x7F, #x80, #x9F.
        "ibm-1-1-valid-P77-ibm77v29.xml", // ibm77v29.xml The VersionNum of the document is 1.1 and the textDecl is missing in the external          entity.  The replacement text of an entity declared in the external DTD contains a          reference to the character #x85, #x8F.
        "ibm-1-1-valid-P77-ibm77v30.xml", // ibm77v30.xml The VersionNum of the document is 1.1 and the textDecl is missing in the external          entity.
    ]);
}

#[test] fn oasis() {
    run_suite("oasis/oasis.xml", &[
        "o-p43pass1", // Valid use of character data, comments, processing instructions and CDATA sections within the start and end tag.
        "o-p68pass1", // Valid entity references.  Also ensures that a charref to           '&' isn't interpreted as an entity reference open delimiter
        "o-p04pass1", // names with all valid ASCII characters, and one from each               other class in NameChar
        "o-p05pass1", // various valid Name constructions
        "o-p01fail1", // S cannot occur before the prolog
        "o-p01fail2", // comments cannot occur before the prolog
        "o-p01fail3", // only one document element
        "o-p09fail1", // EntityValue excludes '%'
        "o-p09fail2", // EntityValue excludes '&'
        "o-p09fail3", // incomplete character reference
        "o-p09fail4", // quote types must match
        "o-p09fail5", // quote types must match
        "o-p11fail1", // quote types must match
        "o-p11fail2", // cannot contain delimiting quotes
        "o-p12fail1", // '"' excluded
        "o-p12fail2", // '\' excluded
        "o-p12fail3", // entity references excluded
        "o-p12fail6", // built-in entity refs excluded
        "o-p12fail7", // The public ID has a tab character, which is disallowed
        "o-p14fail3", // "]]>" excluded
        "o-p18fail3", // CDSect's can't nest
        "o-p22fail1", // prolog must start with XML decl
        "o-p22fail2", // prolog must start with XML decl
        "o-p23fail1", // "xml" must be lower-case
        "o-p27fail1", // References aren't allowed in Misc,     even if they would resolve to valid Misc.
        "o-p29fail1", // A processor must not pass unknown declaration types.
        "o-p30fail1", // An XML declaration is not the same as a TextDecl
        "o-p31fail1", // external subset excludes doctypedecl
        "o-p32fail3", // initial S is required
        "o-p40fail1", // S is required between attributes
        "o-p44fail4", // Whitespace required between attributes.
        "o-p45fail1", // ELEMENT must be upper case.
        "o-p45fail2", // S before contentspec is required.
        "o-p45fail3", // only one content spec
        "o-p45fail4", // no comments in declarations (contrast with SGML)
        "o-p46fail1", // no parens on declared content
        "o-p46fail2", // no inclusions (contrast with SGML)
        "o-p46fail3", // no exclusions (contrast with SGML)
        "o-p46fail4", // no space before occurrence
        "o-p46fail5", // single group
        "o-p46fail6", // can't be both declared and modeled
        "o-p47fail1", // Invalid operator '|' must match previous operator ','
        "o-p47fail2", // Illegal character '-' in Element-content model
        "o-p47fail3", // Optional character must follow a name or list
        "o-p47fail4", // Illegal space before optional character
        "o-p48fail1", // Illegal space before optional character
        "o-p48fail2", // Illegal space before optional character
        "o-p51fail1", // occurrence on #PCDATA group must be *
        "o-p51fail2", // occurrence on #PCDATA group must be *
        "o-p51fail3", // #PCDATA must come first
        "o-p51fail4", // occurrence on #PCDATA group must be *
        "o-p51fail5", // only '|' connectors
        "o-p51fail6", // Only '|' connectors and occurrence on #PCDATA group must be *
        "o-p51fail7", // no nested groups
        "o-p52fail1", // A name is required
        "o-p52fail2", // A name is required
        "o-p53fail1", // S is required before default
        "o-p53fail2", // S is required before type
        "o-p53fail3", // type is required
        "o-p53fail4", // default is required
        "o-p53fail5", // name is requried
        "o-p54fail1", // don't pass unknown attribute types
        "o-p55fail1", // must be upper case
        "o-p56fail1", // no IDS type
        "o-p56fail2", // no NUMBER type
        "o-p56fail3", // no NAME type
        "o-p56fail4", // no ENTITYS type - types must be upper case
        "o-p56fail5", // types must be upper case
        "o-p57fail1", // no keyword for NMTOKEN enumeration
        "o-p58fail1", // at least one value required
        "o-p58fail2", // separator must be '|'
        "o-p58fail3", // notations are NAMEs, not NMTOKENs -- note:     Leaving the invalid           notation undeclared would cause a validating parser to fail without           checking the name syntax, so the notation is declared with an           invalid name.  A parser that reports error positions should report           an error at the AttlistDecl on line 6, before reaching the notation           declaration.
        "o-p58fail4", // NOTATION must be upper case
        "o-p58fail5", // S after keyword is required
        "o-p58fail6", // parentheses are require
        "o-p58fail7", // values are unquoted
        "o-p58fail8", // values are unquoted
        "o-p59fail1", // at least one required
        "o-p59fail2", // separator must be ","
        "o-p59fail3", // values are unquoted
        "o-p60fail1", // keywords must be upper case
        "o-p60fail2", // S is required after #FIXED
        "o-p60fail3", // only #FIXED has both keyword and value
        "o-p60fail4", // #FIXED required value
        "o-p60fail5", // only one default type
        "o-p61fail1", // no other types, including TEMP, which is valid in SGML
        "o-p62fail1", // INCLUDE must be upper case
        "o-p62fail2", // no spaces in terminating delimiter
        "o-p63fail1", // IGNORE must be upper case
        "o-p63fail2", // delimiters must be balanced
        "o-p64fail1", // section delimiters must balance
        "o-p64fail2", // section delimiters must balance
        "o-p66fail5", // no references to non-characters
        "o-p69fail1", // terminating ';' is required
        "o-p69fail2", // no S after '%'
        "o-p69fail3", // no S before ';'
        "o-p70fail1", // This is neither
        "o-p71fail1", // S is required before EntityDef
        "o-p71fail2", // Entity name is a Name, not an NMToken
        "o-p71fail3", // no S after "<!"
        "o-p71fail4", // S is required after "<!ENTITY"
        "o-p72fail1", // S is required after "<!ENTITY"
        "o-p72fail2", // S is required after '%'
        "o-p72fail3", // S is required after name
        "o-p72fail4", // Entity name is a name, not an NMToken
        "o-p73fail1", // No typed replacement text
        "o-p73fail2", // Only one replacement value
        "o-p73fail3", // No NDataDecl on replacement text
        "o-p73fail4", // Value is required
        "o-p73fail5", // No NDataDecl without value
        "o-p74fail1", // no NDataDecls on parameter entities
        "o-p74fail2", // value is required
        "o-p74fail3", // only one value
        "o-p75fail1", // S required after "PUBLIC"
        "o-p75fail2", // S required after "SYSTEM"
        "o-p75fail3", // S required between literals
        "o-p75fail4", // "SYSTEM" implies only one literal
        "o-p75fail5", // only one keyword
        "o-p75fail6", // "PUBLIC" requires two literals (contrast with SGML)
        "o-p76fail1", // S is required before "NDATA"
        "o-p76fail2", // "NDATA" is upper-case
        "o-p76fail3", // notation name is required
    ]);
}

#[test] fn sun_valid() {
    run_suite("sun/sun-valid.xml", &[
        "ext01", // Tests use of external parsed entities with and without content.
        "ext02", // Tests use of external parsed entities with different    encodings than the base document.
        "not-sa02", // A non-standalone document is valid if declared as such.
        "not-sa03", // A non-standalone document is valid if declared as such.
        "not-sa04", // A non-standalone document is valid if declared as such.
        "sa02", // A document may be marked 'standalone' if any     attributes that need normalization are  defined within the internal DTD subset.
        "sa03", // A document may be marked 'standalone' if any     the defined entities need expanding are internal,     and no attributes need defaulting or normalization.     On output, requires notations to be correctly reported.
        "sa04", // Like sa03 but relies on attribute     defaulting defined in the internal subset.     On output, requires notations to be correctly reported.
        "v-pe00", // Tests construction of internal entity replacement text, using     an example in the XML specification.
        "v-pe03", // Tests construction of internal entity replacement text, using     an example in the XML specification.
        "v-pe02", // Tests construction of internal entity replacement text, using     a complex example in the XML specification.
    ]);
}

#[test] fn sun_ill_formed() {
    run_suite("sun/sun-not-wf.xml", &[
        "attlist01", // SGML's NUTOKEN is not allowed.
        "attlist02", // SGML's NUTOKENS attribute type is not allowed.
        "attlist03", // Comma doesn't separate enumerations, unlike in SGML.
        "attlist04", // SGML's NUMBER attribute type is not allowed.
        "attlist05", // SGML's NUMBERS attribute type is not allowed.
        "attlist06", // SGML's NAME attribute type is not allowed.
        "attlist07", // SGML's NAMES attribute type is not allowed.
        "attlist08", // SGML's #CURRENT is not allowed.
        "attlist09", // SGML's #CONREF is not allowed.
        "attlist10", // Whitespace required between attributes
        "attlist11", // Whitespace required between attributes
        "cond01", // Only INCLUDE and IGNORE are conditional section keywords
        "cond02", // Must have keyword in conditional sections
        "content01", // No whitespace before "?" in content model
        "content02", // No whitespace before "*" in content model
        "content03", // No whitespace before "+" in content model
        "decl01", // External entities may not have standalone decls.
        "nwf-dtd00", // Comma mandatory in content model
        "nwf-dtd01", // Can't mix comma and vertical bar in content models
        "dtd02", // PE name immediately after "%"
        "dtd03", // PE name immediately followed by ";"
        "dtd04", // PUBLIC literal must be quoted
        "dtd05", // SYSTEM identifier must be quoted
        "dtd07", // Text declarations (which optionally begin any external entity)     are required to have "encoding=...".
        "encoding01", // Illegal character " " in encoding name
        "encoding02", // Illegal character "/" in encoding name
        "encoding03", // Illegal character reference in encoding name
        "encoding04", // Illegal character ":" in encoding name
        "encoding05", // Illegal character "@" in encoding name
        "encoding06", // Illegal character "+" in encoding name
        "pubid01", // Illegal entity ref in public ID
        "pubid02", // Illegal characters in public ID
        "pubid03", // Illegal characters in public ID
        "pubid04", // Illegal characters in public ID
        "pubid05", // SGML-ism:  public ID without system ID
        "sgml02", // XML declaration must be at the very beginning of a document;   it"s not a processing instruction
        "sgml04", // ATTLIST declarations apply to only one element, unlike SGML
        "sgml05", // ELEMENT declarations apply to only one element, unlike SGML
        "sgml06", // ATTLIST declarations are never global, unlike in SGML
        "sgml07", // SGML Tag minimization specifications are not allowed
        "sgml08", // SGML Tag minimization specifications are not allowed
        "sgml09", // SGML Content model exception specifications are not allowed
        "sgml10", // SGML Content model exception specifications are not allowed
        "sgml11", // CDATA is not a valid content model spec
        "sgml12", // RCDATA is not a valid content model spec
        "sgml13", // SGML Unordered content models not allowed
    ]);
}

#[ignore]
#[test] fn japanese() {
    run_suite("japanese/japanese.xml", &[
        "pr-xml-little-endian.xml"  // needs DTD
    ]);
}

#[test] fn xmltest() {
    run_suite("xmltest/xmltest.xml", &[
        "not-wf-sa-003", // Processing Instruction target name is required.
        "not-wf-sa-025", // Text may not contain a literal ']]>' sequence.
        "not-wf-sa-026", // Text may not contain a literal ']]>' sequence.
        "not-wf-sa-029", // Text may not contain a literal ']]>' sequence.
        "not-wf-sa-030", // A form feed is not a legal XML character.
        "not-wf-sa-031", // A form feed is not a legal XML character.
        "not-wf-sa-032", // A form feed is not a legal XML character.
        "not-wf-sa-033", // An ESC (octal 033) is not a legal XML character.
        "not-wf-sa-037", // Character references may not appear after the root element.
        "not-wf-sa-040", // Provides two document elements.
        "not-wf-sa-041", // Provides two document elements.
        "not-wf-sa-044", // Provides two document elements.
        "not-wf-sa-048", // Provides a CDATA section after the root element.
        "not-wf-sa-051", // CDATA is invalid at top level of document.
        "not-wf-sa-052", // Invalid character reference.
        "not-wf-sa-054", // PUBLIC requires two literals.
        "not-wf-sa-056", // Invalid Document Type Definition format - misplaced comment.
        "not-wf-sa-057", // This isn't SGML; comments can't exist in declarations.
        "not-wf-sa-058", // Invalid character , in ATTLIST enumeration
        "not-wf-sa-059", // String literal must be in quotes.
        "not-wf-sa-060", // Invalid type NAME defined in ATTLIST.
        "not-wf-sa-061", // External entity declarations require whitespace between public     and system IDs.
        "not-wf-sa-062", // Entity declarations need space after the entity name.
        "not-wf-sa-063", // Conditional sections may only appear in the external     DTD subset.
        "not-wf-sa-064", // Space is required between attribute type and default values     in <!ATTLIST...> declarations.
        "not-wf-sa-065", // Space is required between attribute name and type     in <!ATTLIST...> declarations.
        "not-wf-sa-066", // Required whitespace is missing.
        "not-wf-sa-067", // Space is required between attribute type and default values     in <!ATTLIST...> declarations.
        "not-wf-sa-068", // Space is required between NOTATION keyword and list of     enumerated choices in <!ATTLIST...> declarations.
        "not-wf-sa-069", // Space is required before an NDATA entity annotation.
        "not-wf-sa-078", // Undefined ENTITY foo.
        "not-wf-sa-079", // ENTITY can't reference itself directly or indirectly.
        "not-wf-sa-080", // ENTITY can't reference itself directly or indirectly.
        "not-wf-sa-082", // This tests the No External Entity References WFC,     since the entity is referred to within an attribute.
        "not-wf-sa-084", // Tests the Parsed Entity WFC by referring to an     unparsed entity.  (This precedes the error of not declaring     that entity's notation, which may be detected any time before     the DTD parsing is completed.)
        "not-wf-sa-085", // Public IDs may not contain "[".
        "not-wf-sa-086", // Public IDs may not contain "[".
        "not-wf-sa-087", // Public IDs may not contain "[".
        "not-wf-sa-089", // Parameter entities "are" always parsed; NDATA annotations     are not permitted.
        "not-wf-sa-091", // Parameter entities "are" always parsed; NDATA annotations     are not permitted.
        "not-wf-sa-096", // Space is required before the standalone declaration.
        "not-wf-sa-101", // Space is not permitted in an encoding name.
        "not-wf-sa-105", // Invalid placement of CDATA section.
        "not-wf-sa-106", // Invalid placement of entity declaration.
        "not-wf-sa-107", // Invalid document type declaration.  CDATA alone is invalid.
        "not-wf-sa-113", // Parameter entity values must use valid reference syntax;     this reference is malformed.
        "not-wf-sa-114", // General entity values must use valid reference syntax;     this reference is malformed.
        "not-wf-sa-121", // A name of an ENTITY was started with an invalid character.
        "not-wf-sa-122", // Invalid syntax mixed connectors are used.
        "not-wf-sa-123", // Invalid syntax mismatched parenthesis.
        "not-wf-sa-124", // Invalid format of Mixed-content declaration.
        "not-wf-sa-125", // Invalid syntax extra set of parenthesis not necessary.
        "not-wf-sa-126", // Invalid syntax Mixed-content must be defined as zero or more.
        "not-wf-sa-127", // Invalid syntax Mixed-content must be defined as zero or more.
        "not-wf-sa-128", // Invalid CDATA syntax.
        "not-wf-sa-129", // Invalid syntax for Element Type Declaration.
        "not-wf-sa-130", // Invalid syntax for Element Type Declaration.
        "not-wf-sa-131", // Invalid syntax for Element Type Declaration.
        "not-wf-sa-132", // Invalid syntax mixed connectors used.
        "not-wf-sa-133", // Illegal whitespace before optional character causes syntax error.
        "not-wf-sa-134", // Illegal whitespace before optional character causes syntax error.
        "not-wf-sa-135", // Invalid character used as connector.
        "not-wf-sa-136", // Tag omission is invalid in XML.
        "not-wf-sa-137", // Space is required before a content model.
        "not-wf-sa-138", // Invalid syntax for content particle.
        "not-wf-sa-139", // The element-content model should not be empty.
        "not-wf-sa-143", // Character #x001F is not legal anywhere in an XML document.
        "not-wf-sa-144", // Character #xFFFF is not legal anywhere in an XML document.
        "not-wf-sa-147", // XML Declaration may not be preceded by whitespace.
        "not-wf-sa-148", // XML Declaration may not be preceded by comments or whitespace.
        "not-wf-sa-149", // XML Declaration may not be within a DTD.
        "not-wf-sa-154", // '<?XML ...?>' is neither an XML declaration     nor a legal processing instruction target name.
        "not-wf-sa-155", // '<?xmL ...?>' is neither an XML declaration     nor a legal processing instruction target name.
        "not-wf-sa-158", // SGML-ism:  "#NOTATION gif" can't have attributes.
        "not-wf-sa-160", // Violates the PEs in Internal Subset WFC     by using a PE reference within a declaration.
        "not-wf-sa-161", // Violates the PEs in Internal Subset WFC     by using a PE reference within a declaration.
        "not-wf-sa-162", // Violates the PEs in Internal Subset WFC     by using a PE reference within a declaration.
        "not-wf-sa-164", // Invalid placement of Parameter entity reference.
        "not-wf-sa-165", // Parameter entity declarations must have a space before     the '%'.
        "not-wf-sa-166", // Character FFFF is not legal anywhere in an XML document.
        "not-wf-sa-167", // Character FFFE is not legal anywhere in an XML document.
        "not-wf-sa-171", // Character FFFF is not legal anywhere in an XML document.
        "not-wf-sa-172", // Character FFFF is not legal anywhere in an XML document.
        "not-wf-sa-173", // Character FFFF is not legal anywhere in an XML document.
        "not-wf-sa-174", // Character FFFF is not legal anywhere in an XML document.
        "not-wf-sa-175", // Character FFFF is not legal anywhere in an XML document.
        "not-wf-sa-177", // Character FFFF is not legal anywhere in an XML document.
        "not-wf-sa-179", // Invalid syntax matching double quote is missing.
        "not-wf-sa-180", // The Entity Declared WFC requires entities to be declared     before they are used in an attribute list declaration.
        "not-wf-sa-183", // Mixed content declarations may not include content particles.
        "not-wf-sa-184", // In mixed content models, element names must not be     parenthesized.
        "not-wf-sa-186", // Whitespace is required between attribute/value pairs.
        "not-wf-not-sa-001", // Conditional sections must be properly terminated ("]>" used     instead of "]]>").
        "not-wf-not-sa-002", // Processing instruction target names may not be "XML"      in any combination of cases.
        "not-wf-not-sa-003", // Conditional sections must be properly terminated ("]]>" omitted).
        "not-wf-not-sa-004", // Conditional sections must be properly terminated ("]]>" omitted).
        "not-wf-not-sa-005", // Tests the Entity Declared VC by referring to an     undefined parameter entity within an external entity.
        "not-wf-not-sa-006", // Conditional sections need a '[' after the INCLUDE or IGNORE.
        "not-wf-not-sa-007", // A <!DOCTYPE ...> declaration may not begin any external     entity; it's only found once, in the document entity.
        "not-wf-not-sa-008", // In DTDs, the '%' character must be part of a parameter     entity reference.
        "not-wf-not-sa-009", // This test violates WFC:PE Between Declarations in Production 28a.       The last character of a markup declaration is not contained in the same      parameter-entity text replacement.
        "valid-sa-012", // Uses a legal XML 1.0 name consisting of a single colon     character (disallowed by the latest XML Namespaces draft).
        "valid-sa-023", // Test demonstrates that Entity References are valid element content.
        "valid-sa-024", // Test demonstrates that Entity References are valid element content and also demonstrates a valid Entity Declaration.
        "valid-sa-049", // Test demonstrates that characters outside of normal ascii range can be used as element content.
        "valid-sa-050", // Test demonstrates that characters outside of normal ascii range can be used as element content.
        "valid-sa-051", // The document is encoded in UTF-16 and uses some name     characters well outside of the normal ASCII range.
        "valid-sa-053", // Tests inclusion of a well-formed internal entity, which     holds an element required by the content model.
        "valid-sa-066", // Expands a CDATA attribute with a character reference.
        "valid-sa-068", // Tests definition of an internal entity holding a carriage return character     reference, which must not be normalized before reporting to the application.  Line      break normalization only occurs when parsing external parsed entities.
        "valid-sa-085", // Parameter and General entities use different namespaces,     so there can be an entity of each type with a given name.
        "valid-sa-086", // Tests whether entities may be declared more than once,     with the first declaration being the binding one.
        "valid-sa-087", // Tests whether character references in internal entities are     expanded early enough, by relying on correct handling to     make the entity be well formed.
        "valid-sa-088", // Tests whether entity references in internal entities are     expanded late enough, by relying on correct handling to     make the expanded text be valid.  (If it's expanded too     early, the entity will parse as an element that's not     valid in that context.)
        "valid-sa-089", // Tests entity expansion of three legal character references,     which each expand to a Unicode surrogate pair.
        "valid-sa-108", // This tests normalization of end-of-line characters (CRLF)     within entities to LF, primarily as an output test.
        "valid-sa-110", // Basically an output test, this requires that a CDATA     attribute with a CRLF be normalized to one space.
        "valid-sa-114", // Test demonstrates that all text within a valid CDATA section is considered text and not recognized as markup.
        "valid-sa-115", // Test demonstrates that an entity reference is processed by recursively processing the replacement text of the entity.
        "valid-sa-117", // Test demonstrates that entity expansion is done while processing entity declarations.
        "valid-sa-118", // Test demonstrates that entity expansion is done while processing entity declarations.
        "valid-not-sa-031", // Expands a general entity which contains a CDATA section with     what looks like a markup declaration (but is just text since     it's in a CDATA section).
        "valid-ext-sa-001", // A combination of carriage return line feed in an external entity must     be normalized to a single newline.
        "valid-ext-sa-002", // A carriage return (also CRLF) in an external entity must     be normalized to a single newline.
        "valid-ext-sa-003", // Test demonstrates that the content of an element can be empty. In this case the external entity is an empty file.
        "valid-ext-sa-004", // A carriage return (also CRLF) in an external entity must     be normalized to a single newline.
        "valid-ext-sa-005", // Test demonstrates the use of optional character and content particles within an element content.  The test also show the use of external entity.
        "valid-ext-sa-006", // Test demonstrates the use of optional character and content particles within mixed element content.  The test also shows the use of an external entity and that a carriage control line feed in an external entity must be normalized to a single newline.
        "valid-ext-sa-007", // Test demonstrates the use of external entity and how replacement  text is retrieved and processed.
        "valid-ext-sa-008", // Test demonstrates the use of external  entity and how replacement text is retrieved and processed.  Also tests the use of an  EncodingDecl of UTF-16.
        "valid-ext-sa-009", // A carriage return (also CRLF) in an external entity must     be normalized to a single newline.
        "valid-ext-sa-011", // Test demonstrates the use of a public identifier with and external entity.   The test also show that a carriage control line feed combination in an external  entity must be normalized to a single newline.
        "valid-ext-sa-012", // Test demonstrates both internal and external entities and that processing of entity references may be required to produce the correct replacement text.
        "valid-ext-sa-013", // Test demonstrates that whitespace is handled by adding a single whitespace to the normalized value in the attribute list.
        "valid-ext-sa-014", // Test demonstrates use of characters outside of normal ASCII range.
    ]);
}

