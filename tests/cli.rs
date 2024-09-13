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
    cmd.assert()
        .success()
        .stdout(predicate::str::is_match(result)?);
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
    cmd.assert()
        .success()
        .stdout(predicate::str::is_match(result)?);
    Ok(())
}

#[test]
fn test_while_loop() -> anyhow::Result<()> {
    let mut cmd = Command::cargo_bin("rlox")?;

    cmd.arg("-f").arg("examples/while.lox");
    let result = r#"9\n8\n7\n6\n5\n4\n3\n2\n1\n0"#;
    cmd.assert()
        .success()
        .stdout(predicate::str::is_match(result)?);
    Ok(())
}

#[test]
fn test_for_loop() -> anyhow::Result<()> {
    let mut cmd = Command::cargo_bin("rlox")?;

    cmd.arg("-f").arg("examples/for_loop.lox");
    let result = r#"0\n1\n1\n2\n3\n5\n8\n13\n21\n34\n55\n89\n144\n233\n377\n610\n987\n1597\n2584\n4181\n6765"#;
    cmd.assert()
        .success()
        .stdout(predicate::str::is_match(result)?);
    Ok(())
}

#[test]
fn test_loops_break() -> anyhow::Result<()> {
    let mut cmd = Command::cargo_bin("rlox")?;

    cmd.arg("-f").arg("examples/loops_break.lox");
    let result = r#"9\n8\n7\n6\n5\n4\n3\n2\n1\n0\n9\n8\n7\n6\n5\n4\n3\n2\n1\n0\n9\n8\n7\n6\n5\n4"#;
    cmd.assert()
        .success()
        .stdout(predicate::str::is_match(result)?);
    Ok(())
}

#[test]
fn test_function_return() -> anyhow::Result<()> {
    let mut cmd = Command::cargo_bin("rlox")?;

    cmd.arg("-f").arg("examples/function.lox");
    let result = r#"1\n2\n3\n1\n2\n3\n2\n3\n"#;
    cmd.assert()
        .success()
        .stdout(predicate::str::is_match(result)?);

    let mut cmd = Command::cargo_bin("rlox")?;

    cmd.arg("-f").arg("examples/function2.lox");
    let result =
        r#"0\n1\n1\n2\n3\n5\n8\n13\n21\n34\n55\n89\n144\n233\n377\n610\n987\n1597\n2584\n4181"#;
    cmd.assert()
        .success()
        .stdout(predicate::str::is_match(result)?);
    Ok(())
}

#[test]
fn test_function_closure() -> anyhow::Result<()> {
    let mut cmd = Command::cargo_bin("rlox")?;

    cmd.arg("-f").arg("examples/function_local_function.lox");
    let result = r#"1\n2\n3"#;
    cmd.assert()
        .success()
        .stdout(predicate::str::is_match(result)?);
    Ok(())
}
