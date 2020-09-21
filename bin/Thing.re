module Identifier = {
  type t = string;
  let compare = String.compare;
};
module Ast = {
  type literal =
    | String(string);

  type expr =
    | Identifier(Identifier.t)
    | Literal(literal)
    | Binding({
        identifier: Identifier.t,
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
    ]
    |> List.to_seq
    |> of_seq;
};

module Evaluate = {
  open Ast;
  open Value;

  let eval_literal =
    fun
    | Ast.String(string) => String(string);

  let rec eval = (env: Environment.t, code: Ast.t): Value.t =>
    switch (code) {
    // TODO: find_opt
    | Identifier(id) => env |> Environment.find(id)
    | Literal(literal) => eval_literal(literal)
    | Binding({identifier, value, return}) =>
      let value = eval(env, value);
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
    identifier: "message",
    value: Literal(String("Hello")),
    return:
      Apply({
        function_: Identifier("print"),
        argument: Identifier("message"),
      }),
  });

Evaluate.eval(Environment.initial, code);
