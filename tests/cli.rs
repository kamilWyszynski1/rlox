use assert_cmd::Command;
use predicates::prelude::predicate;

#[test]
fn test_control_flow() -> anyhow::Result<()> {
    let mut cmd = Command::cargo_bin("rlox")?;

    cmd.arg("-f").arg("examples/control_flow_if.lox");
    let result = r#"not equal
hi
yes
"#;
    cmd.assert().success().stdout(predicate::str::is_match(result)?);
    Ok(())
}

#[test]
fn test_scope() -> anyhow::Result<()> {
    let mut cmd = Command::cargo_bin("rlox")?;

    cmd.arg("-f").arg("examples/scope.lox");
    let result = r#"inner2 a
outer b
global c
inner2 a
outer b
global c
outer a
outer b
global c
global a
global b
global c
"#;
    cmd.assert().success().stdout(predicate::str::is_match(result)?);
    Ok(())
}