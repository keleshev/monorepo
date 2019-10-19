open Printf

module Token = struct
  type t =
    | Number of int
    | Addition
    | Exponent
    | Left_paren
    | Right_paren
end

module Syntax = struct
  type t =
    | Number of int
    | Addition of t * t
    | Exponent of t * t

  let rec debug = function
    | Number n -> sprintf "%d" n
    | Addition (left, right) ->
        sprintf "(%s + %s)" (debug left) (debug right)
    | Exponent (left, right) ->
        sprintf "(%s ^ %s)" (debug left) (debug right)
end

let string_to_list source =
  source |> String.to_seq |> List.of_seq

let rec tokenize: char list -> Token.t list = function
  | [] -> []
  | ' ' :: rest -> tokenize rest
  | '+' :: rest -> Addition :: tokenize rest
  | '^' :: rest -> Exponent :: tokenize rest
  | '(' :: rest -> Left_paren :: tokenize rest
  | ')' :: rest -> Right_paren :: tokenize rest
  | '0'..'9' as char :: rest ->
      Number (Char.code char - 48) :: tokenize rest
  | _ -> raise (Failure "tokenizer error")

let rec parse_term ~precedence = function
  | Token.Number n :: rest ->
      parse_infix ~precedence ~left:(Syntax.Number n) rest
  | Token.Left_paren :: rest ->
      begin match parse_term ~precedence rest with
      | term, (Token.Right_paren :: _ as rest) ->
          parse_infix ~precedence ~left:term rest
      | _ -> raise (Failure "parser: unmatched paren")
      end
  | _ -> raise (Failure "parser error")

and parse_infix ~precedence ~left = function
  | Token.Addition :: rest when precedence < 10 -> (* (<) for left associativity *)
      let right, rest = parse_term ~precedence:10 rest in
      parse_infix ~precedence ~left:(Syntax.Addition (left, right)) rest
  | Token.Exponent :: rest when precedence <= 20 -> (* (<=) for right associativity *)
      let right, rest = parse_term ~precedence:20 rest in
      parse_infix ~precedence ~left:(Syntax.Exponent (left, right)) rest
  | other -> left, other

let parse source =
  let chars = string_to_list source in
  let tokens = tokenize chars in
  match parse_term ~precedence:0 tokens with
  | result, [] -> result
  | _ -> raise (Failure "could not parse to completion")

let format source = Syntax.debug (parse source)

let exponent_is_right_associative =
  assert (format "1 ^ 2 ^ 3" = "(1 ^ (2 ^ 3))")

let addition_is_left_associative =
  assert (format "1 + 2 + 3" = "((1 + 2) + 3)")

let exponent_has_higher_precedence_than_addition =
  assert (format "1 + 2 ^ 3 + 4 ^ 5 + 6" = "(((1 + (2 ^ 3)) + (4 ^ 5)) + 6)")
