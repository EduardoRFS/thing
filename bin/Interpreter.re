open Ast;

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

  let rec eval = (env: Environment.t, {Ast.desc: code, _}): Value.t =>
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

include Evaluate;
