open Lib

let rec split_tachyon (current : int array) (level : char array list) =
    match level with
    | [] -> current
    | l :: tail ->
      CCArray.iteri
        (fun i c ->
           if c = '^' && current.(i) > 0 then (
             current.(i - 1) <- current.(i - 1) + current.(i);
             current.(i + 1) <- current.(i + 1) + current.(i);
             current.(i) <- 0
           ) )
        l;
      split_tachyon current tail
;;

let parse_line (s : string) = CCString.to_array s

let () =
    let lines = File.read_list_of_line parse_line Sys.argv.(1) in
    let (first, tail) = CCList.hd_tl lines in
    let first =
        CCArray.map
          (fun c ->
             if c = 'S' then
               1
             else
               0 )
          first
    in

    let res = split_tachyon first tail |> CCArray.fold ( + ) 0 in
    print_int res;
    ()
;;
