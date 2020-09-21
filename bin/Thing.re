open Ast.Builder.Default;

let code =
  bind_function(
    ~identifier="add_one",
    ~parameter="x",
    ~value=
      apply(
        ~function_=apply(~function_=identifier("+"), ~argument=number(1)),
        ~argument=identifier("x"),
      ),
    ~return=
      apply(
        ~function_=identifier("print"),
        ~argument=
          apply(
            ~function_=identifier("number_to_string"),
            ~argument=
              apply(~function_=identifier("add_one"), ~argument=number(3)),
          ),
      ),
  );

let infer_with_errors = (env, code) =>
  try(Typecheck.infer(env, code)) {
  | Typecheck.Failed_to_type({expected, received}) =>
    Printf.printf(
      "expected: %s\nreceived: %s\n",
      Typecheck.show_typed_expr(expected),
      Typecheck.show_typed_expr(received),
    );
    exit(1);
  };
let typed_code = infer_with_errors(Typecheck.Environment.initial, code);
Typecheck.show_typed_ast(typed_code) |> print_endline;
Interpreter.eval(Interpreter.Environment.initial, typed_code);
