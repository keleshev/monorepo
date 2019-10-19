open Printf

module Token = struct
  type t =
    | Number of int
    | Addition
    | Exponent
(*
    | Left_paren
    | Right_paren
*)
end

module Syntax = struct
  type t =
    | Number of int
    | Addition of t * t
    | Exponent of t * t
end

let rec parse_term left tokens = match left, tokens with
  | None, Token.Number n :: rest ->
      parse_term (Some (Syntax.Number n)) rest
  | Some left, Token.Addition :: rest ->
      begin match parse_term None rest with
      | None -> None
      | Some (right, rest) ->
          parse_term (Some (Syntax.Addition (left, right))) rest
      end
  | Some left, Token.Exponent :: rest ->
      begin match parse_term None rest with
      | None -> None
      | Some (right, rest) ->
          parse_term (Some (Syntax.Exponent (left, right))) rest
      end
  | Some left, other -> Some (left, other)
  | _, _ -> None


let parse tokens =
  match parse_term None tokens with
  | Some (result, []) -> Some result
  | _ -> None

let rec format = function
  | Syntax.Number n -> [Token.Number n]
  | Syntax.Addition (left, right) ->
      format left @ [Token.Addition] @ format right
  | Syntax.Exponent (left, right) ->
      format left @ [Token.Exponent] @ format right

(*
theorem format_of_parse_of_x_is_x x =
  match parse x with
  | None -> true
  | Some syntax -> format syntax = x
*)


(*
let exponent_is_right_associative =
  assert (format "1 ^ 2 ^ 3" = "(1 ^ (2 ^ 3))")

let addition_is_left_associative =
  assert (format "1 + 2 + 3" = "((1 + 2) + 3)")

let exponent_has_higher_precedence_than_addition =
  assert (format "1 + 2 ^ 3 + 4 ^ 5 + 6" = "(((1 + (2 ^ 3)) + (4 ^ 5)) + 6)")
*)
