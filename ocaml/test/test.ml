open OUnit2
open Lib

let test_square _ =
  assert_equal 25.0 (square 5.0)

let suite =
  "Unit tests" >::: [
    "test_square" >:: test_square;
  ]

let () =
  run_test_tt_main suite
  