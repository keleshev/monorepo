let sprintf = Format.sprintf
let fprintf = Format.fprintf

module JSON = struct
  type t = [
    | `Assoc of (string * t) list
    | `Bool of bool
    | `Float of float
    | `Int of int
    | `List of t list
    | `Null
    | `String of string
  ]

  let escape = sprintf "%S"  (* TODO *)

  let comma_separated to_string list =
    String.concat ", " (List.map to_string list)

  let rec to_string_naive = function
    | `Null     -> sprintf "null"
    | `Bool b   -> sprintf "%b" b
    | `Int i    -> sprintf "%i" i
    | `Float f  -> sprintf "%e" f     (* %g? *)
    | `String s -> sprintf "%s" (escape s)
    | `List l   -> sprintf "[%s]" (comma_separated to_string_naive l)
    | `Assoc l  -> sprintf "{%s}" (comma_separated pair_to_string l)

  and pair_to_string (key, value) =
    sprintf "%s: %s" (escape key) (to_string_naive value)




  let comma_separated pp =
    Format.pp_print_list pp ~pp_sep:(fun ppf () -> fprintf ppf ", ")

  let rec pp ppf = function
    | `Null     -> fprintf ppf "null"
    | `Bool b   -> fprintf ppf "%b" b
    | `Int i    -> fprintf ppf "%d" i
    | `Float f  -> fprintf ppf "%e" f
    | `String s -> fprintf ppf "%s" (escape s)
    | `List l   -> fprintf ppf "[%a]" (comma_separated pp) l
    | `Assoc l  -> fprintf ppf "{%a}" (comma_separated pp_pair) l

  and pp_pair ppf (key, value) =
    fprintf ppf "%s: %a" key pp value

end



let data: JSON.t = `Assoc [
  "difficulty", `Int 3;
  "hash", `String "0x000c8a76cc";
  "nonce", `Int 3785;
  "predecessor", `String "0x0004ea327a7";
  "transactions", `List [
    `Assoc [
      "inputs", `List [`Assoc ["amount", `Int 30; "id", `Int 74]];
      "outputs", `List [
        `Assoc ["amount", `Int 15; "id", `Int 75];
        `Assoc ["amount", `Int 15; "id", `Int 76];
      ];
    ];
  ];
]


let () = print_endline (JSON.to_string_naive data)
