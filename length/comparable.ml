let compare = 1

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


module Foo_bar: sig
  type t = Foo | Bar

  include Comparable.S with type t := t
end = struct
  type t = Foo | Bar

  let compare t1 t2 =
    match t1, t2 with
    | Foo, Foo -> 0
    | Foo, Bar -> +1
    | Bar, Foo -> -1
    | Bar, Bar -> 0

  module C = Comparable.Make (struct
    type nonrec t = t
    let compare = compare
  end)

  include (C: Comparable.S with type t := t)
end
