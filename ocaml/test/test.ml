open OUnit2
open Lib

let test_skip_whitespace _ =
  let bruh = [ ' '; '\n'; 'A'; 'B' ] in
  assert_equal [ 'A'; 'B' ] (skip_whitespace bruh)

let test_read_string _ =
  let node = parse "\"Hello\"" in
  assert_equal (String "Hello") node

let test_read_escaped_char _ =
  let node = parse "\"\\\"\"" in
  assert_equal (String "\"") node

let test_read_array _ =
  let node = parse "[\"yee\", \"boi\"]" in
  assert_equal (Array [ String "yee"; String "boi" ]) node

let test_read_array_with_whitespace _ =
  let node = parse "[           \"yee\",   \"boi\" \n \n \t]" in
  assert_equal (Array [ String "yee"; String "boi" ]) node

let test_read_bool _ =
  let a = parse "true" in
  let b = parse "false" in
  assert_equal (Boolean true) a;
  assert_equal (Boolean false) b

let test_read_null _ =
  let a = parse "null" in
  assert_equal Null a

let test_read_object_with_string_value _ =
  let tbl = Hashtbl.create 16 in
  Hashtbl.add tbl "yee" (String "boi");
  let a = parse "{ \"yee\" : \"boi\" }" in
  assert_equal (Object tbl) a

let test_read_object_with_nested_stuff _ =
  let tbl = Hashtbl.create 16 in
  Hashtbl.add tbl "yee" (Array [ String "boi" ]);
  let a = parse "{ \"yee\" : [\"boi\"] }" in
  assert_equal (Object tbl) a

let test_read_number _ =
  let a = parse "123.5" in
  assert_equal (Number 123.5) a

let test_big_thing _ =
  let yee =
    {big_thing|
  {
    "firstName": "John",
    "lastName": "Smith",
    "isAlive": true,
    "age": 27,
    "address": {
      "streetAddress": "21 2nd Street",
      "city": "New York",
      "state": "NY",
      "postalCode": "10021-3100"
    },
    "phoneNumbers": [
      {
        "type": "home",
        "number": "212 555-1234"
      },
      {
        "type": "office",
        "number": "646 555-4567"
      }
    ],
    "children": [],
    "spouse": null
}
  |big_thing}
  in
  let res = parse yee in
  assert_equal (Some (String "646 555-4567"))
    (child_at_path res "phoneNumbers.1.number");
  assert_equal (Some (String "NY")) (child_at_path res "address.state");
  assert_equal (Some Null) (child_at_path res "spouse")

let suite =
  "Unit tests"
  >::: [
         "test_skip_whitespace" >:: test_skip_whitespace;
         "test_read_string" >:: test_read_string;
         "test_read_escaped_char" >:: test_read_escaped_char;
         "test_read_array" >:: test_read_array;
         "test_read_array_with_whitespace" >:: test_read_array_with_whitespace;
         "test_read_bool" >:: test_read_bool;
         "test_read_null" >:: test_read_null;
         "test_read_object_with_single_key"
         >:: test_read_object_with_string_value;
         "test_read_object_with_nested_stuff"
         >:: test_read_object_with_nested_stuff;
         "test_read_number" >:: test_read_number;
         "test_big_thing" >:: test_big_thing;
       ]

let () = run_test_tt_main suite
