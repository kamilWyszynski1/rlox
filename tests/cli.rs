use assert_cmd::Command;
use predicates::prelude::predicate;

macro_rules! test_set {
    ($name:ident, $path:expr, $result:expr) => {
        #[test]
        fn $name() -> Result<(), Box<dyn std::error::Error>> {
            let mut cmd = Command::cargo_bin("rlox")?;
            cmd.arg("-f").arg($path).arg("--verbose").arg("false");
            cmd.assert()
                .success()
                .stdout(predicate::str::is_match(format!("{}$", $result))?);
            Ok(())
        }
    };
}

macro_rules! test_set_error {
    ($name:ident, $path:expr, $result:expr) => {
        #[test]
        fn $name() -> Result<(), Box<dyn std::error::Error>> {
            let mut cmd = Command::cargo_bin("rlox")?;
            cmd.arg("-f").arg($path).arg("--verbose").arg("false");
            cmd.assert()
                .failure()
                .stderr(predicate::str::is_match($result)?);
            Ok(())
        }
    };
}

test_set!(
    test_control_flow,
    "examples/control_flow_if.lox",
    r#"not equal
hi
yes
"#
);

test_set!(
    test_scope,
    "examples/scope.lox",
    r#"inner2 a
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
"#
);

test_set_error!(
    var_already_exists,
    "examples/var_already_exists.lox",
    r#"Error: Already a variable with this name in this scope."#
);

test_set_error!(
    invalid_top_level_return,
    "examples/invalid_return.lox",
    "Error: Can't return from top-level code."
);

test_set_error!(
    cannot_read_var_in_init,
    "examples/local_variable_in_init.lox",
    "Error: Can't read local variable in its own initializer."
);

test_set_error!(
    variable_not_used,
    "examples/variable_not_used.lox",
    "Error: Variable b is never used"
);

test_set!(
    simple_class_prints,
    "examples/class.lox",
    r#"Class DevonshireCream
Class DevonshireCream instance
1
2
100
100
Crunch crunch crunch!
The German chocolate cake is delicious!
Class Thing instance
"#
);

test_set!(
    class_static_method,
    "examples/class_static_method.lox",
    r#"9
"#
);

test_set_error!(
    invalid_this_usage,
    "examples/invalid_this_usage.lox",
    "Error: Can't use 'this' outside of a class."
);

test_set!(
    class_init,
    "examples/class_init.lox",
    r#"Class Foo instance
Class Foo instance
Class Foo instance
lol
"#
);

test_set_error!(
    class_invalid_return_from_init,
    "examples/class_invalid_return_from_init.lox",
    "Error: Undefined property 'secondField'."
);

test_set_error!(
    class_invalid_this_in_static_method,
    "examples/class_invalid_this_in_static_method.lox",
    r#"Error: Can't access 'this' keyword in static method.
"#
);

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

#[test]
fn test_function_static_scope() -> anyhow::Result<()> {
    let mut cmd = Command::cargo_bin("rlox")?;

    cmd.arg("-f").arg("examples/function_static_scope.lox");
    let result = r#"global\nglobal"#;
    cmd.assert()
        .success()
        .stdout(predicate::str::is_match(result)?);
    Ok(())
}
