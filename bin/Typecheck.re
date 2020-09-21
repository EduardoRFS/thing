open Ast;
open Ast.Builder;

module Type_variable: {
  [@deriving show]
  type t;
  let compare: (t, t) => int;
  let next: unit => t;
} = {
  [@deriving show]
  type t = int;
  let compare = Int.compare;
  let count = ref(0);
  let next = () => {
    count := count^ + 1;
    count^;
  };
};

[@deriving show]
type typed_expr =
  | Void
  | String
  | Number
  | Function(typed_expr, typed_expr)
  | Variable(Type_variable.t);

exception
  Failed_to_type({
    expected: typed_expr,
    received: typed_expr,
  });

[@deriving show]
type typed_ast = Ast.t(typed_expr);

module Substitution = {
  include Map.Make(Type_variable);

  type nonrec t = t(typed_expr);

  let rec apply = (typed_expr, s) =>
    switch (typed_expr) {
    | Void => Void
    | String => String
    | Number => Number
    | Function(parameter, return) =>
      Function(apply(parameter, s), apply(return, s))
    | Variable(v) =>
      switch (s |> find_opt(v)) {
      | Some(type_) => type_
      | None => Variable(v)
      }
    };
  let apply_typed_code = (code, s) =>
    map_data((_desc, type_) => s |> apply(type_), code);

  let merge = (left: t, right: t) =>
    merge(
      (_key, left, right) =>
        switch (left, right) {
        | (_, Some(v))
        | (Some(v), _) => Some(v)
        | (None, None) => None
        },
      left,
      right,
    );
};
module Environment = {
  include Map.Make(Identifier);

  type nonrec t = t(typed_expr);

  let initial =
    [
      ("print", Function(String, Void)),
      ("number_to_string", Function(Number, String)),
      ("+", Function(Number, Function(Number, Number))),
    ]
    |> List.to_seq
    |> of_seq;
};

let rec unify = (l, r) => {
  switch (l, r) {
  | (Variable(v), t)
  | (t, Variable(v)) => Substitution.singleton(v, t)
  | (Function(l_p, l_r), Function(r_p, r_r)) =>
    Substitution.merge(unify(l_p, r_p), unify(l_r, r_r))
  | (l, r) when l == r => Substitution.empty
  | _ => raise(Failed_to_type({received: l, expected: r}))
  };
};

let rec infer = (env, {desc: code, _}) =>
  switch (code) {
  | Identifier(id) =>
    Identifier(id)
    |> expr((env |> Environment.find(id), Substitution.empty))
  | Literal(String(s)) =>
    Literal(String(s)) |> expr((String, Substitution.empty))
  | Literal(Number(n)) =>
    Literal(Number(n)) |> expr((Number, Substitution.empty))
  | Binding({identifier, parameter: None, value, return}) =>
    let value = infer(env, value);
    let (value_type, s1) = value.data;
    let env = env |> Environment.add(identifier, value_type);

    let return = infer(env, return);
    let (return_type, s2) = return.data;

    let desc = Binding({identifier, parameter: None, value, return});
    let data = (return_type, Substitution.merge(s1, s2));
    expr(data, desc);
  | Binding({identifier, parameter: Some(parameter), value, return}) =>
    let value = {
      let parameter_type = Variable(Type_variable.next());
      let env = env |> Environment.add(parameter, parameter_type);
      let value = infer(env, value);
      let (value_type, s1) = value.data;

      let value_type =
        Function(s1 |> Substitution.apply(parameter_type), value_type);
      expr((value_type, s1), value.desc);
    };
    let (value_type, s1) = value.data;
    let env = env |> Environment.add(identifier, value_type);

    let return = infer(env, return);
    let (return_type, s2) = return.data;

    let desc =
      Binding({identifier, parameter: Some(parameter), value, return});
    let data = (return_type, Substitution.merge(s1, s2));
    expr(data, desc);
  | Apply({function_, argument}) =>
    let function_ = infer(env, function_);
    let (function_type, s1) = function_.data;
    let argument = infer(env, argument);
    let (argument_type, s2) = argument.data;

    let return_type = Variable(Type_variable.next());
    let s3 = unify(Function(argument_type, return_type), function_type);

    let substitutions = Substitution.merge(s1, Substitution.merge(s2, s3));

    let data = (
      substitutions |> Substitution.apply(return_type),
      substitutions,
    );
    let desc = Apply({function_, argument});
    expr(data, desc);
  };
let infer = (env: Environment.t, code) => {
  let typed_code = infer(env, code);
  let (_, substitutions) = typed_code.data;
  let typed_code = typed_code |> map_data((_desc, (type_, _)) => type_);
  substitutions |> Substitution.apply_typed_code(typed_code);
};
