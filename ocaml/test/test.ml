open OUnit2
open Lib

let print s = s

let test_square _ =
  assert_equal 25.0 (square 5.0)

let test_read_string _ =
  let str = read_string ['"'; 'H'; 'e'; 'l'; 'l'; 'o'; '"'] in
  assert_equal "Hello" str

let test_read_escaped_char _ =
  let str = read_string ['"'; '\\'; '"'; '"'] in

  print_endline str;

  assert_equal "\"" str

let suite =
  "Unit tests" >::: [
    "test_square" >:: test_square;
    "test_read_string" >:: test_read_string;
    "test_read_escaped_char" >:: test_read_escaped_char;
  ]

let () =
  run_test_tt_main suite
  