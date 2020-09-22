module ASM = {
  type kind =
    | Symbol
    | Label;
  type address = {
    name: string,
    kind,
  };
  type instr =
    | Int32(int32)
    | String(string)
    | Load(address)
    | Store(address)
    | Push
    | Pop
    | Jmp;

  type t = {
    address,
    body: list(instr),
  };
};
