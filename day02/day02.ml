let ( --> ) x y = Seq.ints x |> fun s -> Seq.take (y - x + 1) s |> List.of_seq

let list_of_int x =
  x |> string_of_int |> String.fold_left (fun a b -> a @ [ String.make 1 b ]) []

let sum l = List.fold_left (fun x y -> x + y) 0 l

let split_by n l =
  let x = List.length l / n in
  let rec loop l2 =
    match l2 with [] -> [] | l3 -> List.take x l3 :: (List.drop x l3 |> loop)
  in
  loop l

let divisibles_by n =
  let rec loop acc =
    if acc = n then [ acc ]
    else if n mod acc = 0 && acc != 1 then [ acc ] @ loop (acc + 1)
    else loop (acc + 1)
  in
  loop 1

let parse input =
  input |> String.split_on_char ','
  |> List.map @@ fun id ->
     let ids = String.split_on_char '-' id in
     (List.nth ids 0 |> int_of_string, List.nth ids 1 |> int_of_string)

let part1 input =
  let filter range =
    range
    |> List.filter @@ fun e -> (e |> string_of_int |> String.length) mod 2 == 0
  in
  let id_is_invalid id =
    let l = list_of_int id in
    let n = List.length l / 2 in
    let x = List.take n l in
    let y = List.rev l |> List.take n |> List.rev in
    x = y
  in
  input
  |> List.map (fun (l, r) -> l --> r |> filter |> List.filter id_is_invalid)
  |> List.flatten

let part2 input =
  let is_invalid input =
    let l_id = list_of_int input in
    let divs = List.length l_id |> divisibles_by in
    let ids = divs |> List.map @@ fun x -> split_by x l_id in
    ids
    |> (List.filter @@ fun x -> List.sort_uniq compare x |> List.length = 1)
    |> List.flatten |> List.length >= 1
    && input > 10
  in
  input
  |> (List.map @@ fun (x, y) -> x --> y |> List.filter @@ is_invalid)
  |> List.flatten

let () =
  let input = Sys.argv.(1) in
  let ids = parse input in
  let invalid_ids1 = part1 ids in
  let invalid_ids2 = part2 ids in
  let res1 = sum invalid_ids1 in
  let res2 = sum invalid_ids2 in
  Printf.printf "Part1 = %i\n" res1;
  Printf.printf "Part2 = %i\n" res2
