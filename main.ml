open struct
  [@@@warning "-32"]

  let ( <> ) = `Shadowed
  let ( = ) = `Shadowed
  let ( < ) : int -> int -> bool = ( < )
  let ( <= ) : int -> int -> bool = ( <= )

  module List = ListLabels
  module Array = ArrayLabels
  module Bytes = BytesLabels
  module String = StringLabels
end

let parallelism = 8
let include_3_letter_words = false
let include_4_letter_words = true
let omit_solutions_with_duplicate_words = true

module String_set = Hashtbl.Make (String)
module Int_map = Map.Make

let set_row_to_word ~grid ~row ~word =
  for column = 0 to 4 do
    Bytes.set grid.(column) row word.[column]
  done
;;

let rec iter_solutions_non_parallel ~grid ~first_words ~all_words ~prefixes ~row ~f =
  if Int.equal row 5
  then f grid
  else
    Array.iter first_words ~f:(fun word ->
      for column = 0 to 4 do
        Bytes.set grid.(column) row word.[column]
      done;
      if
        Array.for_all grid ~f:(fun column ->
          let prefix = Bytes.sub_string column ~pos:0 ~len:(row + 1) in
          String_set.mem prefixes prefix)
      then
        iter_solutions_non_parallel
          ~grid
          ~first_words:all_words
          ~all_words
          ~prefixes
          ~row:(row + 1)
          ~f)
;;

let iter_solutions_parallel ~num_chunks ~grid ~words ~prefixes ~row ~f =
  let chunks = Array.init num_chunks ~f:(fun _ -> []) in
  Array.iteri words ~f:(fun i word ->
    let i = i mod num_chunks in
    chunks.(i) <- word :: chunks.(i));
  let domains =
    Array.map chunks ~f:(fun chunk ->
      Domain.spawn (fun () ->
        let grid = Array.map grid ~f:Bytes.copy in
        iter_solutions_non_parallel
          ~grid
          ~first_words:(Array.of_list (List.rev chunk))
          ~all_words:words
          ~prefixes
          ~row
          ~f))
  in
  Array.iter domains ~f:Domain.join
;;

let rec list_has_adjacent_duplicates xs =
  match xs with
  | [] | [ _ ] -> false
  | x :: y :: rest -> String.equal x y || list_has_adjacent_duplicates (y :: rest)
;;

let () =
  let argc = Array.length Sys.argv in
  if argc < 2
  then (
    print_endline "Generate the letters of a crossword puzzle using the given words file";
    print_endline "usage: self.exe WORDSFILE")
  else (
    let wordsfile = Sys.argv.(1) in
    let words =
      In_channel.with_open_text wordsfile In_channel.input_lines
      |> List.concat_map ~f:(fun word ->
        let s = Printf.sprintf in
        match String.length word with
        | 3 ->
          if include_3_letter_words
          then [ s "!!%s" word; s "!%s!" word; s "%s!!" word ]
          else []
        | 4 -> if include_4_letter_words then [ s "!%s" word; s "%s!" word ] else []
        | 5 -> [ word ]
        | _ -> [])
      |> Array.of_list
    in
    let prefixes = String_set.create 300000 in
    for prefix_length = 0 to 5 do
      Array.iter words ~f:(fun word ->
        String_set.add prefixes (String.sub word ~pos:0 ~len:prefix_length) ())
    done;
    let grid = Array.init 5 ~f:(fun _ -> Bytes.create 5) in
    for arg = 2 to argc - 1 do
      let word = Sys.argv.(arg) in
      if String.length word > 5
      then Printf.eprintf "skipping '%s' because it is longer than 5 characters%!" word
      else set_row_to_word ~grid ~row:(arg - 2) ~word
    done;
    let mutex = Mutex.create () in
    iter_solutions_parallel
      ~num_chunks:parallelism
      ~grid
      ~words
      ~prefixes
      ~row:(argc - 2)
      ~f:(fun grid ->
        let horizontal_words =
          List.init ~len:5 ~f:(fun row ->
            String.init 5 ~f:(fun column -> Bytes.get grid.(column) row))
        in
        let vertical_words =
          List.init ~len:5 ~f:(fun column -> String.of_bytes grid.(column))
        in
        if
          (not omit_solutions_with_duplicate_words)
          || not
               (list_has_adjacent_duplicates
                  (List.sort ~cmp:String.compare (horizontal_words @ vertical_words)))
        then
          Mutex.protect mutex (fun () ->
            Printf.printf
              "%s;%s\n%!"
              (String.concat horizontal_words ~sep:",")
              (String.concat vertical_words ~sep:","))))
;;
