let code =
  Ast.Binding({
    identifier: "add_one",
    parameter: Some("x"),
    value:
      Apply({
        function_:
          Apply({
            function_: Identifier("+"),
            argument: Literal(Number(1)),
          }),
        argument: Identifier("x"),
      }),
    return:
      Apply({
        function_: Identifier("print"),
        argument:
          Apply({
            function_: Identifier("number_to_string"),
            argument:
              Apply({
                function_: Identifier("add_one"),
                argument: Literal(Number(4)),
              }),
          }),
      }),
  });

Interpreter.eval(Interpreter.Environment.initial, code);
