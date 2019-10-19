open Printf

module type GENERIC = sig
  type t

  val constructor : string -> int -> t list -> t
  val string : string -> t
  val int : int -> t
end

module Json_string = struct
  type t = string

  let constructor name _ = function
    | [] -> sprintf "%S" name
    | payload ->
        sprintf "{%S: [%s]}" name (String.concat ", " payload)

  let string = sprintf "%S"
  let int = sprintf "%d"
end

module Show = struct
  type t = string

  let constructor name _ = function
    | [] -> name
    | payload ->
        sprintf "%s (%s)" name (String.concat ", " payload)

  let string = sprintf "%S"
  let int = sprintf "%d"
end



type t =
  | Bar
  | Baz of string * t
[@@deriving Generic]



module Generic (X: GENERIC) = struct
  let rec run = function
    | Bar -> X.constructor "Bar" 0 []
    | Baz (_1, _2) -> X.constructor "Baz" 1 [X.string _1; run _2]
end




module Test = struct
  let (=>) left right = if left = right then printf "." else printf "F" in
  let module M = Generic (Json_string) in

  M.run Bar => {|"Bar"|};
  M.run (Baz ("hai", Bar)) => {|{"Baz": ["hai", "Bar"]}|};


  let module M = Generic (Show) in
  M.run Bar => "Bar";
  M.run (Baz ("hai", Bar)) => {|Baz ("hai", Bar)|};
end
