//! Every case of the shared vendor corpus (`tests/corpus/vendor.toml`) through `Variant::parse_vendor`.
#![cfg(feature = "corpus")]

use rvariant::Variant;
use rvariant::corpus::vendor_cases;

#[test]
fn vendor_corpus() {
    let cases = vendor_cases().expect("corpus parses");
    assert!(cases.len() >= 300, "{} cases", cases.len());
    let mut failed = Vec::new();
    for case in &cases {
        let result = case
            .ty()
            .and_then(|ty| case.check(&Variant::parse_vendor(&case.text, &ty)));
        if let Err(e) = result {
            failed.push(format!(
                "{} {:?} ({}): {e}",
                case.origin(),
                case.text,
                case.kind
            ));
        }
    }
    assert!(
        failed.is_empty(),
        "{} of {} cases fail:\n{}",
        failed.len(),
        cases.len(),
        failed.join("\n")
    );
}
