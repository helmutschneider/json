type node =
  | String of string
  | Number of float
  | Boolean of bool
  | Null

type parser = {
  buffer: char list;
  index: int;
}

exception ParserError of string

let read_string str =
  let rec read_from chunk carry =
    match chunk with
    | ['"'] -> carry
    | '"'::tail -> read_from tail carry
    | '\\'::x::tail -> read_from tail (List.append carry [x])
    | head::tail -> read_from tail (List.append carry [head])
    | _ -> raise (ParserError("Bruh!"))
  in
  read_from str [] |>
    List.to_seq |>
    String.of_seq

let square : (float -> float)
  = fun x -> x *. x
