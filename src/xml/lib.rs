#[crate_id = "xml#0.1"];
#[crate_type = "lib"];
#[warn(missing_doc)];
#[forbid(non_camel_case_types)];

//! This crate currently provides almost XML 1.1-compliant pull parser.

#[feature(macro_rules, struct_variant)];

pub mod common;
pub mod events;
pub mod pull;
