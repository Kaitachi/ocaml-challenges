#load "str/str.cma";;
#require "str";;

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
       read_whole_file "../../assets/AdventOfCode2024/Day03.in"
    |> (fun text -> (* Let's find all operations first... *)
            let index = ref 0 in
            let matches = ref [] in
            (* let re = Str.regexp {|mul(\([0-9]+\),\([0-9]+\))|} in *)
            let re = Str.regexp {|mul([0-9]+,[0-9]+)|} in
            while (!index >= 0) do
                let i =
                    try
                        Str.search_forward re text !index
                    with Not_found -> -1 in
                if i >= 0 then begin
                    matches := !matches @ [Str.matched_group 0 text];
                    index := Str.match_end ();
                end else
                    index := i
            done;
            print_endline "done";
            !matches
    )
    |> List.map (fun op ->
            let re = Str.regexp {|\([0-9]+\),\([0-9]+\)|} in

            let _ = Str.search_forward re op 0 in
            let x = int_of_string (Str.matched_group 1 op) in
            let y = int_of_string (Str.matched_group 2 op) in
            x * y
    )
    (* |> List.iter (Printf.printf "%d\n") *)
    |> List.fold_left (+) 0

;;

let part_02 =
       read_whole_file "../../assets/AdventOfCode2024/Day03.in"
    |> (fun text -> (* Let's find all operations first... *)
            let index = ref 0 in
            let matches = ref [] in
            let re = Str.regexp {|mul([0-9]+,[0-9]+)\|do()\|don't()|} in
            while (!index >= 0) do
                let i =
                    try
                        Str.search_forward re text !index
                    with Not_found -> -1 in
                if i >= 0 then begin
                    matches := !matches @ [Str.matched_group 0 text];
                    index := Str.match_end ();
                end else
                    index := i
            done;
            print_endline "done";
            !matches
    )
    |> (fun lst ->
            let valid = ref [] in
            let should_do = ref true in

               lst
            |> List.iter (fun item ->
                    Printf.printf "> %s;\t should_do? %b\n" item !should_do;
                    match item with
                    | "do()" -> should_do := true
                    | "don't()" -> should_do := false
                    | instruction -> if !should_do then valid := !valid @ [instruction]
            );
            !valid
    )
    |> List.map (fun op ->
            let re = Str.regexp {|\([0-9]+\),\([0-9]+\)|} in
    
            let _ = Str.search_forward re op 0 in
            let x = int_of_string (Str.matched_group 1 op) in
            let y = int_of_string (Str.matched_group 2 op) in
            x * y
    )
    |> List.fold_left (+) 0

;;


let () = part_02
    |> Printf.printf "%d\n"

