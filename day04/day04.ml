let parse input =
  input |> String.split_on_char '\n' |> Array.of_list
  |> Array.map @@ fun line -> line |> String.to_seq |> Array.of_seq

let adjacents_of x y mat =
  let num_rows = Array.length mat in
  let candidates =
    [
      (x - 1, y - 1);
      (x - 1, y);
      (x - 1, y + 1);
      (x, y - 1);
      (x, y + 1);
      (x + 1, y - 1);
      (x + 1, y);
      (x + 1, y + 1);
    ]
  in
  let is_valid (r, c) =
    r >= 0 && r < num_rows && c >= 0 && c < Array.length mat.(r)
  in
  candidates |> List.filter is_valid |> List.map (fun (r, c) -> mat.(r).(c))

let forklift_can_access x y mat =
  let open List in
  adjacents_of x y mat
  |> (filter @@ fun elem -> elem = '@' || elem = 'x')
  |> length < 4

let mark_rolls_to_be_removed mat =
  let _ =
    mat
    |> Array.iteri @@ fun x row ->
       row
       |> Array.mapi_inplace @@ fun y roll ->
          if roll = '@' && forklift_can_access x y mat then 'x' else roll
  in
  let open List in
  mat |> Array.to_list
  |> (map @@ fun x -> Array.to_list x)
  |> flatten
  |> (filter @@ fun e -> e = 'x')
  |> length

let remove_rolls mat =
  let open Array in
  mat
  |> iter @@ fun row ->
     row |> map_inplace @@ fun elem -> if elem = 'x' then '.' else elem

let part1 mat = mark_rolls_to_be_removed mat

let part2 mat =
  let rec loop () =
    let rolls_to_be_removed = mark_rolls_to_be_removed mat in
    if rolls_to_be_removed = 0 then []
    else
      let _ = remove_rolls mat in
      [ rolls_to_be_removed ] @ loop ()
  in
  loop () |> List.fold_left (fun x y -> x + y) 0

let () =
  let input = Sys.argv.(1) in
  let mat = parse input in
  let res1 = mat |> Array.copy |> part1 in
  let res2 = mat |> Array.copy |> part2 in
  Printf.printf "Part1 = %i\n" res1;
  Printf.printf "Part2 = %i\n" res2
