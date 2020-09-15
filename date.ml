

module Date = struct
  type t = (int * int) * int

  let (-) x y = x, y

  let year ((y, _), _) = y
  let month ((_, m), _) = m
  let day ((_, _), d) = d
end

let () =
  let date = Date.(2019-12-11) in
  assert (Date.year date = 2019);
  assert (Date.month date = 12);
  assert (Date.day date = 11);



