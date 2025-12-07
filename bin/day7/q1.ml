open Lib

let rec split_tachyon
          (current : char array)
          (level : char array list)
          (acc : int)
  =
    match level with
    | [] -> acc
    | l :: tail ->
      let split_count =
          CCArray.foldi
            (fun acc i c ->
               if c = '^' && current.(i) = '|' then (
                 current.(i) <- '.';
                 current.(i + 1) <- '|';
                 current.(i - 1) <- '|';
                 acc + 1
               ) else
                 acc )
            0
            l
      in
      split_tachyon current tail (acc + split_count)
;;

let parse_line (s : string) = CCString.to_array s

let () =
    let lines = File.read_list_of_line parse_line Sys.argv.(1) in
    let (first, tail) = CCList.hd_tl lines in
    let first =
        CCArray.map
          (fun c ->
             if c = 'S' then
               '|'
             else
               c )
          first
    in

    let res = split_tachyon first tail 0 in
    print_int res;
    ()
;;
