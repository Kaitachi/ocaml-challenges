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

let is_stable_report report =
    match report with
    | [] -> false
    | [_] -> false
    | a::b::_ ->
            let delta = compare a b in (* get sign of difference *)

            match delta with
            | 0  -> (* First two elements are equal! *)
                    (fun () -> false) ()

            | 1  -> (* First two elements are in descending order *)
                    (fun () ->
                        let prev = ref (List.hd report) in
                        let is_safe = ref true in
                        List.iter (fun i ->
                            let diff = i - !prev in
                            Printf.printf "...> %d - %d = %d\n" !prev i (diff);
                            if diff < -3 || 0 <= diff then is_safe := false;
                            prev := i;
                        ) (List.tl report);
                        Printf.printf "...>> So, %b!\n\n" !is_safe;
                        !is_safe
                    ) ()

            | -1 -> (* First two elements are in ascending order *)
                    (fun () ->
                        let prev = ref (List.hd report) in
                        let is_safe = ref true in
                        List.iter (fun i ->
                            let diff = i - !prev in
                            Printf.printf "...> %d - %d = %d\n" !prev i (diff);
                            if diff <= 0 || 3 < diff then is_safe := false;
                            prev := i;
                        ) (List.tl report);
                        Printf.printf "...>> So, %b!\n\n" !is_safe;
                        !is_safe
                    ) ()

let part_01 = 
       (* Read input file *)
       read_whole_file "../../assets/AdventOfCode2024/Day02.in"
    |> String.split_on_char '\n'
    |> List.filter (fun line -> line <> "")
    |> List.map (Str.split (Str.regexp "[ ]+"))
    |> List.map (List.map int_of_string)

       (* Check each report - is it stable/acceptable? *)
    |> List.filter is_stable_report
    |> List.length

;;


let part_02 =
       (* Read input file *)
       read_whole_file "../../assets/AdventOfCode2024/Day02.in"
    |> String.split_on_char '\n'
    |> List.filter (fun line -> line <> "")
    |> List.map (Str.split (Str.regexp "[ ]+"))
    |> List.map (List.map int_of_string)

       (* Check each report - is it stable/acceptable? *)
    |> List.filter (fun report ->
        if not (is_stable_report report) then
               report
            |> List.mapi (fun i item ->
                let sub_list = ref [] in
                List.iteri (fun j elmt ->
                    if i <> j then sub_list := !sub_list @ [elmt]
                ) report;
                is_stable_report !sub_list
            )
            |> List.exists (fun x -> x)
        else true
    )
    |> List.length

