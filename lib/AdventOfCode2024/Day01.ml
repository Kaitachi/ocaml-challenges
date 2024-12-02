#load "str/str.cma";;

(* NOTE: Kudos to Jeffrey Scofield from StackOverflow for the below snippet! *)
(* https://stackoverflow.com/a/53840784 *)
let read_whole_file filename =
    (* open_in_bin works correctly on Unix and Windows *)
    let ch = open_in_bin filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s
;;

let part_01 =
       read_whole_file "../../assets/AdventOfCode2024/Day01.in"
    |> String.split_on_char '\n'
    |> List.filter_map (fun contents ->
            let line = Str.split (Str.regexp "[ ]+") contents in
            match line with
                | [] -> None
                | hd :: tl -> Some (int_of_string hd, int_of_string (List.hd tl))
    )
    |> List.split
    |> (fun lists ->
            let a, b = lists in
            let a' = List.sort compare a in
            let b' = List.sort compare b in
            List.combine a' b'
    )
    |> List.map (fun pair ->
            let a, b = pair in
            abs (a - b)
    )
    |> List.fold_left (+) 0

;;

let thingy lst x =
    List.map (fun el ->
        if el == x then 1
        else 0
    ) lst

let part_02 = 
       read_whole_file "../../assets/AdventOfCode2024/Day01.in"
    |> String.split_on_char '\n'
    |> List.filter_map (fun contents ->
            let line = Str.split (Str.regexp "[ ]+") contents in
            match line with
                | [] -> None
                | hd :: tl -> Some (int_of_string hd, int_of_string (List.hd tl))
    )
    |> List.split
    |> (fun lists ->
            let a, b = lists in
            let freq = List.map (thingy b) a
                |> List.map (List.fold_left (+) 0) in
            List.combine a freq
    )
    |> List.map (fun pair ->
            let a, freq = pair in
            a * freq
    )
    |> List.fold_left (+) 0

;;

let () = part_02
    |> Printf.printf "%d\n"

