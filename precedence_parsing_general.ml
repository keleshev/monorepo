open Printf

module Operator = struct
  type t = Addition | Exponent

  let precedence = function Addition -> 10 | Exponent -> 20

  let associativity = function Addition -> `Left | Exponent -> `Right

  let to_string = function Addition -> "+" | Exponent -> "^"
end

module Token = struct
  type t =
    | Number of int
    | Operator of Operator.t
    | Left_paren
    | Right_paren
end

module Syntax = struct
  type t =
    | Number of int
    | Infix of t * Operator.t * t

  let rec debug = function
    | Number n -> sprintf "%d" n
    | Infix (left, operator, right) ->
        sprintf "(%s %s %s)"
          (debug left) (Operator.to_string operator) (debug right)
end

let string_to_list source =
  source |> String.to_seq |> List.of_seq

let rec tokenize: char list -> Token.t list = function
  | [] -> []
  | ' ' :: rest -> tokenize rest
  | '+' :: rest -> Operator Operator.Addition :: tokenize rest
  | '^' :: rest -> Operator Operator.Exponent :: tokenize rest
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
  | Token.Operator o :: rest
    when precedence <= (let p = Operator.precedence o in
                        if Operator.associativity o = `Left then p - 1 else p)  ->
      let right, rest = parse_term ~precedence:(Operator.precedence o) rest in
      parse_infix ~precedence ~left:(Syntax.Infix (left, o, right)) rest
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
