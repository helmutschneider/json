open Lib

let () =
  let stuff = "{ \"hello\": \"world\" }" in
  let _ = parse stuff in
  print_endline "Hello world!"
