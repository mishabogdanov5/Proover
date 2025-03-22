let rec fac n = if n < 2 then 1 else n * fac (n - 1)

let%expect_test "fac-test-1" =
  print_int (fac 5);
  [%expect {|120|}]

let%expect_test "fac-test-2" =
  print_int (fac 1);
  [%expect {|1|}]

let%expect_test "fac-test-1" =
  print_int (fac 7);
  [%expect {|504|}]
