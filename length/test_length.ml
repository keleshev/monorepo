
module Test (Length: Length.LENGTH) = struct
  let zero  = [] in
  let one   = [1] in
  let two   = [1; 2] in
  let three = [1; 2; 3] in
  let rec infinite = 1 :: infinite in

  assert Length.(of_list two > of_list zero);
  assert Length.(of_list two > of_list one);
  assert Length.(of_list two = of_list two);
  assert Length.(of_list two < of_list three);
  assert Length.(of_list two < of_list infinite);
end

module Test_1 = Test (Length.Example_using_GADT.List.Length)
module Test_2 = Test (Length.Example_using_inline_sum.List.Length)
