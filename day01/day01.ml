type rotation = Left of int | Right of int

let parse_line l =
  let dir = String.sub l 0 1 in
  let clicks = String.sub l 1 (String.length l - 1) |> int_of_string in
  if dir = "R" then Right clicks else Left clicks

let parse lines = lines |> String.split_on_char '\n' |> List.map parse_line
let div_mod x y = (x / y, x mod y)

let part1 input =
  let to_left pointer clicks =
    abs (100 + ((pointer - clicks) mod 100)) mod 100
  in
  let to_right pointer clicks = (pointer + clicks) mod 100 in

  let rotate_to rotation pointer =
    match rotation with
    | Left clicks -> to_left pointer clicks
    | Right clicks -> to_right pointer clicks
  in

  let rec rotate_all rotations pointer =
    match rotations with
    | [] -> [ -1 ]
    | h :: l ->
        let new_pointer = rotate_to h pointer in
        new_pointer :: rotate_all l new_pointer
  in

  let get_password output =
    List.filter (fun i -> i = 0) output |> List.length
  in
  let res = rotate_all input 50 in
  get_password res

let part2 input pointer =
  let to_left pointer clicks =
    let new_pointer = abs (100 + ((pointer - clicks) mod 100)) mod 100 in
    let on_zero =
      if pointer != 0 then (clicks + new_pointer) / 100 else clicks / 100
    in
    (on_zero, new_pointer)
  in

  let to_right pointer clicks =
    let new_pointer = (pointer + clicks) mod 100 in
    let on_zeros = clicks / 100 in
    let on_zero =
      if pointer + clicks <= 99 then 0
      else if new_pointer < pointer && new_pointer != 0 then on_zeros + 1
      else on_zeros
    in
    (on_zero, new_pointer)
  in

  let rotate_to rotation pointer =
    match rotation with
    | Left clicks -> to_left pointer clicks
    | Right clicks -> to_right pointer clicks
  in

  let rec rotate_all rotations pointer =
    match rotations with
    | [] -> []
    | h :: l ->
        let on_zero, new_pointer = rotate_to h pointer in
        (on_zero, new_pointer) :: rotate_all l new_pointer
  in
  let get_password output =
    output
    |> List.map (fun (x, y) -> if y != 0 then x else x + 1)
    |> List.fold_left (fun x y -> x + y) 0
  in
  let output = rotate_all input pointer in
  get_password output

let () =
  let input = Sys.argv.(1) in
  let lines = parse input in
  let pwd1 = part1 lines in
  let pwd2 = part2 lines 50 in
  Printf.printf "Part 1 = %i\nPart 2 = %i\n" pwd1 pwd2;
