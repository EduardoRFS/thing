module Identifier = {
  type t = string;
  let compare = String.compare;
};
module Types = {
  type t =
    | Void
    | String
    | Number
    | Function(t, t);
};

module Ast = {
  type literal =
    | String(string)
    | Number(int);

  type expr_desc('data) =
    | Identifier(Identifier.t)
    | Literal(literal)
    | Binding({
        identifier: Identifier.t,
        parameter: option(Identifier.t),
        value: expr('data),
        return: expr('data),
      })
    | Apply({
        function_: expr('data),
        argument: expr('data),
      })
  and expr('data) = {
    desc: expr_desc('data),
    data: 'data,
  };

  module Builder = {
    let expr = (~data, desc) => {desc, data};
    let identifier = (~data, id) => expr(~data, Identifier(id));
    let string = (~data, string) => expr(~data, Literal(String(string)));
    let number = (~data, number) => expr(~data, Literal(Number(number)));
    let binding = (~data, ~identifier, ~value, ~return) =>
      expr(~data, Binding({identifier, parameter: None, value, return}));
    let bind_function = (~data, ~identifier, ~parameter, ~value, ~return) =>
      expr(
        ~data,
        Binding({identifier, parameter: Some(parameter), value, return}),
      );
    let apply = (~data, ~function_, ~argument) =>
      expr(~data, Apply({function_, argument}));

    module Default = {
      let expr = expr(~data=());
      let identifier = identifier(~data=());
      let string = string(~data=());
      let number = number(~data=());
      let binding = binding(~data=());
      let bind_function = bind_function(~data=());
      let apply = apply(~data=());
    };
  };

  type t('data) = expr('data);
};
include Ast;
