let rec compare_lengths l1 l2 =
  match l1, l2 with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | _ :: l1, _ :: l2 -> compare_lengths l1 l2

let rec compare_length_with l n =
  match l with
  | [] ->
    if n = 0 then 0 else
      if n > 0 then -1 else 1
  | _ :: l ->
    if n <= 0 then 1 else
      compare_length_with l (n - 1)



module Comparable = struct
  module type S = sig
    type t

    val compare: t -> t -> int

    val (> ): t -> t -> bool
    val (< ): t -> t -> bool
    val (>=): t -> t -> bool
    val (<=): t -> t -> bool
    val (= ): t -> t -> bool
    val (<>): t -> t -> bool
  end

  module Make (T: sig
    type t

    val compare: t -> t -> int
  end): S with type t = T.t = struct
    type t = T.t
    let compare = T.compare (* or include T? *)

    let (>)  a b = compare a b >  0
    let (<)  a b = compare a b <  0
    let (>=) a b = compare a b >= 0
    let (<=) a b = compare a b <= 0
    let (=)  a b = compare a b =  0
    let (<>) a b = compare a b <> 0
  end
end


module type LENGTH = sig
  type t

  val of_list: 'a list -> t
  val of_int: int -> t

  val to_int: t -> int

  include Comparable.S with type t := t
end

module Example_using_GADT = struct

  module List = struct
    type 'a t = 'a list

    module Length: LENGTH = struct

      type t =
        | Of_list: 'a list -> t
        | Of_int: int -> t

      let of_list list = Of_list list
      let of_int int = Of_int int

      let to_int = function
        | Of_list l -> List.length l
        | Of_int n -> n

      let compare t1 t2 =
        match t1, t2 with
        | Of_list l1, Of_list l2 ->
            compare_lengths l1 l2
        | Of_list l, Of_int n ->
            compare_length_with l n
        | Of_int n, Of_list l ->
            compare_length_with l n * -1
        | Of_int n, Of_int m ->
            compare n m

      include (Comparable.Make (struct
        type nonrec t = t
        let compare = compare
      end): Comparable.S with type t := t)
    end

    let length = Length.of_list
  end
end

module Type_equal = struct
  type ('a, 'b) t = T: ('a, 'a) t

  let conv (type a b) (T : (a, b) t) (a : a) : b = a
end


module type INLINE_SUM = functor
  (Immediate: sig type t end) (Reference: sig type _ t end) ->
sig
  type t

  val of_immediate : Immediate.t -> t
  val of_reference : _ Reference.t -> t

  val is_immediate: t -> bool

  val to_immediate_exn: t -> Immediate.t
  val to_reference_exn: t -> _ Reference.t

  type case =
    | Immediate: (t, Immediate.t) Type_equal.t -> case
    | Reference: (t, 'a Reference.t) Type_equal.t -> case

  val switch : t -> case

  val case :
    t ->
    immediate:(Immediate.t -> 'a) ->
    reference:(_ Reference.t -> 'a) ->
    'a
end

module Unsafe_inline_sum : INLINE_SUM = functor
  (Immediate: sig type t end) (Reference: sig type 'a t end) ->
struct
  type t = Immediate.t (* Had to pick one *)
  type 'a _t = 'a Reference.t (* TODO Unused type *)

  let of_immediate t = t
  let of_reference = Obj.magic

  let is_immediate t = Obj.is_int (Obj.repr t)

  let to_immediate_exn t =
    if is_immediate t then t else raise (Failure "not immediate")

  let to_reference_exn t =
    if is_immediate t then raise (Failure "not reference") else Obj.magic t

  type case =
    | Immediate: (t, Immediate.t) Type_equal.t -> case
    | Reference: (t, 'a Reference.t) Type_equal.t -> case

  let switch t =
    if is_immediate t then
      Immediate Type_equal.T
    else
      Reference (Obj.magic Type_equal.T)

  let case t ~immediate ~reference =
    if is_immediate t then
      immediate t
    else
      reference (Obj.magic t)
end


module Example_using_inline_sum = struct

  module List = struct
    type 'a t = 'a list

    module Length = struct
      module Int_or_list = Unsafe_inline_sum (struct type t = int end) (
        struct type 'a t = 'a list end)

      type t = Int_or_list.t

      let of_list = Int_or_list.of_reference
      let of_int = Int_or_list.of_immediate

      let case t ~list ~int =
        Int_or_list.case t ~reference:list ~immediate:int

(*       let id x = x *)
(*       let to_int t = case t ~int:id ~list:List.length *)

      let to_int t = match Int_or_list.switch t with
        | Immediate wit -> let i = Type_equal.conv wit t in i
        | Reference wit -> let l = Type_equal.conv wit t in List.length l
(*
      let compare_m t1 t2 =
        match _is_int t1, _is_int t2 with
        | false, false ->
            compare_lengths (Obj.magic t1) (Obj.magic t2)
        | false, true ->
            compare_length_with (Obj.magic t1) t2
        | true, false ->
            compare_length_with (Obj.magic t2) t1 * -1
        | true, true ->
            compare t1 t2
*)

(*
      let compare t1 t2 =
        match Int_or_list.(switch t1, switch t2) with
        | Reference w1, Reference w2 ->
            let l1 = Type_equal.conv w1 t1 in
            let l2 = Type_equal.conv w2 t2 in
            compare_lengths l1 l2
        | Reference w1, Immediate w2 ->
            let l1 = Type_equal.conv w1 t1 in
            let n2 = Type_equal.conv w2 t2 in
            compare_length_with l1 n2
        | Immediate w1, Reference w2 ->
            let n1 = Type_equal.conv w1 t1 in
            let l2 = Type_equal.conv w2 t2 in
            compare_length_with l2 n1 * -1
        | Immediate w1, Immediate w2 ->
            let n1 = Type_equal.conv w1 t1 in
            let n2 = Type_equal.conv w2 t2 in
            compare n1 n2
*)

      let compare t1 t2 =
        case t1
          ~list:(fun l ->
            case t2
              ~list:(fun l2 -> compare_lengths l l2)
              ~int:(fun n -> compare_length_with l n))
          ~int:(fun n ->
            case t2
              ~list:(fun l -> compare_length_with l n * -1)
              ~int:(fun m -> compare n m))

      module IL = Int_or_list

      let compare_m t1 t2 =
        match (IL.is_immediate t1, IL.is_immediate t2) with
        | false, false ->
            compare_lengths (IL.to_reference_exn t1) (IL.to_reference_exn t2)
        | false, true ->
            compare_length_with (IL.to_reference_exn t1) (IL.to_immediate_exn t2)
        | true, false ->
            compare_length_with (IL.to_reference_exn t2) (IL.to_immediate_exn t1) * -1
        | true, true ->
            Stdlib.compare (IL.to_immediate_exn t1) (IL.to_immediate_exn t2)

      let (>)  a b = compare a b >  0
      let (<)  a b = compare a b <  0
      let (>=) a b = compare a b >= 0
      let (<=) a b = compare a b <= 0
      let (=)  a b = compare a b =  0
      let (<>) a b = compare a b <> 0
    end

    let length = Length.of_list
  end
end

module Example_using_inline_sum_2 = struct

  module List = struct
    type 'a t = 'a list

    module Length = struct
      type t = int (* | 'a list *)

      let of_list = Obj.magic
      let of_int x = x


      let _is_int t = Obj.is_int (Obj.repr t)

      let to_int t =
        if _is_int t then
          t
        else
          List.length (Obj.magic t)

(*
      let compare t1 t2 =
        match t1, t2 with
        | Of_list l1, Of_list l2 ->
            compare_lengths l1 l2
        | Of_list l, Of_int n ->
            compare_length_with l n
        | Of_int n, Of_list l ->
            compare_length_with l n * -1
        | Of_int n, Of_int m ->
            compare n m

*)
      let compare_m t1 t2 =
        match _is_int t1, _is_int t2 with
        | false, false ->
            compare_lengths (Obj.magic t1) (Obj.magic t2)
        | false, true ->
            compare_length_with (Obj.magic t1) t2
        | true, false ->
            compare_length_with (Obj.magic t2) t1 * -1
        | true, true ->
            compare t1 t2

(*
      let compare_m t1 t2 =
        match _is_int t1, _is_int t2 with
        | true, true ->
            let n = t1 in
            let m = t2 in
            compare n m
        | true, false ->
            let n = t1 in
            let l = Obj.magic t2 in
            compare_length_with l n * -1
        | false, true ->
            let l = Obj.magic t1 in
            let n = t2 in
            compare_length_with l n
        | false, false ->
            let l = Obj.magic t1 in
            let l2 = Obj.magic t2 in
            compare_lengths l l2
*)

      let compare t1 t2 =
        if _is_int t1 then
          let n = t1 in
          if _is_int t2 then
            let m = t2 in
            compare n m
          else
            let l = Obj.magic t2 in
            compare_length_with l n * -1
        else
          let l = Obj.magic t1 in
          if _is_int t2 then
            let n = t2 in
            compare_length_with l n
          else
            let l2 = Obj.magic t2 in
            compare_lengths l l2

      let (>)  a b = compare a b >  0
      let (<)  a b = compare a b <  0
      let (>=) a b = compare a b >= 0
      let (<=) a b = compare a b <= 0
      let (=)  a b = compare a b =  0
      let (<>) a b = compare a b <> 0
    end

    let length = Length.of_list
  end
end
