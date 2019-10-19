(* open Printf *)

(* let bell () = printf "\007%!" *)

(* let () = bell () *)

(* let _ = Gc.create_alarm bell *)

let random_list () = Stdlib.List.init (Random.int 1_000) (fun x -> x)

let n = 10_000
let dataset = Stdlib.Array.init n (fun _ -> random_list ())

let reference = Stdlib.List.init 100 (fun x -> x)


open Core
open Core_bench.Std

let main () =
  Random.self_init ();
  Command.run (Bench.make_command [
    Bench.Test.create ~name:"stdlib" (fun () ->
      Array.iter dataset ~f:(fun list ->
        Stdlib.List.compare_lengths reference list |> ignore
      );
    );
    Bench.Test.create ~name:"gadt" (fun () ->
      Array.iter dataset ~f:(fun list ->
        Length.Example_using_GADT.List.Length.(
          compare (of_list reference) (of_list list) |> ignore
        )
      );
    );
    Bench.Test.create ~name:"inline" (fun () ->
      Array.iter dataset ~f:(fun list ->
        Length.Example_using_inline_sum.List.Length.(
          compare (of_list reference) (of_list list) |> ignore
        )
      );
    );
    Bench.Test.create ~name:"inline m" (fun () ->
      Array.iter dataset ~f:(fun list ->
        Length.Example_using_inline_sum.List.Length.(
          compare_m (of_list reference) (of_list list) |> ignore
        )
      );
    );
    Bench.Test.create ~name:"inline 2" (fun () ->
      Array.iter dataset ~f:(fun list ->
        Length.Example_using_inline_sum_2.List.Length.(
          compare (of_list reference) (of_list list) |> ignore
        )
      );
    );
    Bench.Test.create ~name:"inline 2 m" (fun () ->
      Array.iter dataset ~f:(fun list ->
        Length.Example_using_inline_sum_2.List.Length.(
          compare_m (of_list reference) (of_list list) |> ignore
        )
      );
    );
  ])

let () = main ()

(*
module Bench = struct let time name thunk =
    let t = Sys.time () in
    for i = 1 to n do
      thunk (i - 1) |> ignore
    done;
    printf "%s: %gs\n" name (Sys.time () -. t)

  let () =
    Gc.print_stat stdout;
    time "stdlib" (fun i ->
      Stdlib.(
        List.compare_lengths reference dataset.(i) |> ignore));
    Gc.print_stat stdout;
    time "GADT" (fun i ->
      Example_using_GADT.List.Length.(
        compare (of_list reference) (of_list dataset.(i))) |> ignore);
    Gc.print_stat stdout;
    time "inline" (fun i ->
      Example_using_inline_sum.List.Length.(
        compare (of_list reference) (of_list dataset.(i))) |> ignore);
    Gc.print_stat stdout;



end

(*
List.length nom_ev > List.length quote_ev

List.Length.(of_list nom_ev > of_list quote_ev)

List.compare_lengths nom_ev quote_ev > 0
*)
*)
