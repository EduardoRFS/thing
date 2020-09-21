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
              apply(~function_=identifier("add_one"), ~argument=number(4)),
          ),
      ),
  );

Interpreter.eval(Interpreter.Environment.initial, code);
