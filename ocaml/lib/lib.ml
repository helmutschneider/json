type node =
  | String of string
  | Number of float
  | Boolean of bool
  | Array of node list
  | Object of (string, node) Hashtbl.t
  | Null

type parse_output = {
  node : node;
  remainder : char list;
}

type parse_fn = char list -> parse_output

exception ParseError of string

let rec skip_whitespace : char list -> char list =
 fun chars ->
  match chars with
  | ' ' :: tail -> skip_whitespace tail
  | '\n' :: tail -> skip_whitespace tail
  | '\r' :: tail -> skip_whitespace tail
  | '\t' :: tail -> skip_whitespace tail
  | _ -> chars

let read_string : parse_fn =
 fun chars ->
  let rec aux carry remainder =
    match remainder with
    (* escaped characters *)
    | '\\' :: x :: tail -> aux (List.append carry [ x ]) tail
    (* start of string *)
    | '"' :: tail when carry = [] -> aux carry tail
    (* end of string *)
    | '"' :: tail when carry != [] -> (carry, tail)
    (* regular characters *)
    | x :: tail -> aux (List.append carry [ x ]) tail
    | _ -> raise (ParseError "Supa bad!!")
  in

  let carry, remainder = aux [] chars in
  let str = List.to_seq carry |> String.of_seq in

  { node = String str; remainder }

let is_number_like ch =
  ch = '0' || ch = '1' || ch = '2' || ch = '3' || ch = '4' || ch = '5'
  || ch = '6' || ch = '7' || ch = '8' || ch = '9' || ch = '.'

let read_number : parse_fn =
 fun chars ->
  let rec aux carry remainder =
    match remainder with
    | head :: tail when is_number_like head ->
        aux (List.append carry [ head ]) tail
    | _ -> (carry, remainder)
  in
  let number_chars, rem = aux [] chars in
  let number_str = List.to_seq number_chars |> String.of_seq in

  { node = Number (float_of_string number_str); remainder = rem }

let rec read_node : parse_fn =
 fun chars ->
  let chars = skip_whitespace chars in
  match chars with
  | '"' :: _ -> read_string chars
  | '[' :: _ -> read_array chars
  | '{' :: _ -> read_object chars
  | 't' :: 'r' :: 'u' :: 'e' :: tail ->
      { node = Boolean true; remainder = tail }
  | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: tail ->
      { node = Boolean false; remainder = tail }
  | 'n' :: 'u' :: 'l' :: 'l' :: tail -> { node = Null; remainder = tail }
  | head :: _ when is_number_like head -> read_number chars
  | _ -> raise (ParseError "Very bad!")

and read_array : parse_fn =
 fun chars ->
  let rec aux carry remainder =
    let rem = skip_whitespace remainder in
    match rem with
    | '[' :: tail -> aux carry tail
    | ']' :: tail -> (carry, tail)
    | ',' :: tail -> aux carry tail
    | _ ->
        let res = read_node rem in
        let nodes = List.append carry [ res.node ] in
        aux nodes res.remainder
  in
  let nodes, remainder = aux [] chars in
  { node = Array nodes; remainder }

and read_object : parse_fn =
 fun chars ->
  let tbl = Hashtbl.create 16 in
  let rec aux remainder =
    let rem = skip_whitespace remainder in
    match rem with
    | '{' :: tail -> aux tail
    | '}' :: tail -> tail
    | ',' :: tail -> aux tail
    | '"' :: _ -> (
        let key_node = read_string rem in
        let key_str =
          match key_node.node with
          | String x -> x
          | _ -> raise (ParseError "Invalid key type.")
        in
        match skip_whitespace key_node.remainder with
        | ':' :: tail ->
            let value_node = read_node tail in
            Hashtbl.add tbl key_str value_node.node;
            aux value_node.remainder
        | _ -> rem)
    | _ ->
        let str = String.of_seq (List.to_seq rem) in
        print_endline str;
        raise (ParseError "Bad bad!")
  in

  let rem = aux chars in

  { node = Object tbl; remainder = rem }

let child_at_path : node -> string -> node option =
 fun node path ->
  let rec aux : node -> string list -> node option =
   fun parent parts ->
    match parts with
    | [] -> Some parent
    | head :: tail -> (
        match parent with
        | Array items ->
            let index = int_of_string head in
            let child = List.nth items index in
            aux child tail
        | Object entries ->
            let key = head in
            let child = Hashtbl.find entries key in
            aux child tail
        | _ -> None)
  in
  let parts = String.split_on_char '.' path in
  aux node parts

let parse : string -> node =
 fun str ->
  let chars = String.to_seq str |> List.of_seq in
  let out = read_node chars in
  out.node
