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
include Ast;
