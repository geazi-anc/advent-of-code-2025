let ( --> ) x y = Seq.ints x |> fun s -> Seq.take (y - x + 1) s |> List.of_seq

let list_of_int x =
  x |> string_of_int |> String.fold_left (fun a b -> a @ [ String.make 1 b ]) []

let split_by n l =
  let x = List.length l / n in
  let rec loop l2 =
    match l2 with [] -> [] | l3 -> List.take x l3 :: (List.drop x l3 |> loop)
  in
  loop l

let parse input =
  input |> String.split_on_char ','
  |> List.map @@ fun id ->
     let ids = String.split_on_char '-' id in
     (List.nth ids 0 |> int_of_string, List.nth ids 1 |> int_of_string)

let part1 input =
  let is_invalid id =
    let l = list_of_int id in
    let n = List.length l / 2 in
    let x = List.take n l in
    let y = List.rev l |> List.take n |> List.rev in
    x = y
  in
  input
  |> List.map (fun (l, r) -> l --> r)
  |> List.flatten
  |> (List.filter @@ fun e -> (e |> string_of_int |> String.length) mod 2 == 0)
  |> List.filter is_invalid

let part2 input =
  let is_invalid id =
    let str_id = list_of_int id in
    let len = List.length str_id in
    let rec loop len index =
      if index = 1 then false
      else if len mod index != 0 then loop len (index - 1)
      else if split_by index str_id |> List.sort_uniq compare |> List.length = 1
      then true
      else loop len (index - 1)
    in
    loop len len
  in
  input
  |> (List.map @@ fun (x, y) -> x --> y)
  |> List.flatten |> List.filter is_invalid

let () =
  let input = Sys.argv.(1) in
  let ids = parse input in
  let invalid_ids1 = part1 ids in
  let invalid_ids2 = part2 ids in
  let sum l = List.fold_left (fun x y -> x + y) 0 l in
  let res1 = sum invalid_ids1 in
  let res2 = sum invalid_ids2 in
  Printf.printf "Part1 = %i\n" res1;
  Printf.printf "Part2 = %i\n" res2
