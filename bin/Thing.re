module Identifier = {
  type t = string;
  let compare = String.compare;
};
module Ast = {
  type literal =
    | String(string)
    | Number(int);

  type expr =
    | Identifier(Identifier.t)
    | Literal(literal)
    | Binding({
        identifier: Identifier.t,
        parameter: option(Identifier.t),
        value: expr,
        return: expr,
      })
    | Apply({
        function_: expr,
        argument: expr,
      });
  type t = expr;
};
module Value = {
  type t =
    | Void
    | String(string)
    | Number(int)
    | Function(t => t);
};

module Environment = {
  include Map.Make(Identifier);

  type nonrec t = t(Value.t);

  open Value;
  let initial =
    [
      (
        "print",
        Function(
          fun
          | String(string) => {
              print_endline(string);
              Void;
            }
          | _ => failwith("should fail on type check"),
        ),
      ),
      (
        "number_to_string",
        Function(
          fun
          | Number(n) => {
              String(string_of_int(n));
            }
          | _ => failwith("should fail on type check"),
        ),
      ),
      (
        "+",
        Function(
          fun
          | Number(a) =>
            Function(
              (
                fun
                | Number(b) => Number(a + b)
                | _ => failwith("should fail on type check")
              ),
            )
          | _ => failwith("should fail on type check"),
        ),
      ),
    ]
    |> List.to_seq
    |> of_seq;
};

module Evaluate = {
  open Ast;
  open Value;

  let eval_literal =
    fun
    | Ast.String(string) => String(string)
    | Number(n) => Number(n);

  let rec eval = (env: Environment.t, code: Ast.t): Value.t =>
    switch (code) {
    // TODO: find_opt
    | Identifier(id) => env |> Environment.find(id)
    | Literal(literal) => eval_literal(literal)
    | Binding({identifier, parameter: None, value, return}) =>
      let value = eval(env, value);
      let env = env |> Environment.add(identifier, value);
      eval(env, return);
    | Binding({identifier, parameter: Some(parameter), value, return}) =>
      let value =
        Function(
          argument => {
            let env = env |> Environment.add(parameter, argument);
            eval(env, value);
          },
        );
      let env = env |> Environment.add(identifier, value);
      eval(env, return);
    | Apply({function_, argument}) =>
      let function_ =
        switch (eval(env, function_)) {
        | Function(function_) => function_
        | _ => failwith("should fail at typecheck")
        };
      let argument = eval(env, argument);
      function_(argument);
    };
};

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

Evaluate.eval(Environment.initial, code);
