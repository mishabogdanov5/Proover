let fac n =
  let rec helper acc = function 0 -> acc | n -> helper (n * acc) (n - 1) in
  helper 1 n

let%expect_test "fac-test-1" =
  print_int (fac 5);
  [%expect {|120|}]

let%expect_test "fac-test-2" =
  print_int (fac 7);
  [%expect {|5040|}]
