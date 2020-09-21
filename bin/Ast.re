module Identifier = {
  type t = string;
  let compare = String.compare;
};
module Types = {
  type t =
    | String
    | Number
    | Function;
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
      })
    | Typed(Types.t, expr);
  type t = expr;
};
include Ast;
