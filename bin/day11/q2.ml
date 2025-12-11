open Lib

module StringHashtbl = CCHashtbl.Make (CCString)

let parse_line (s : string) =
    match CCString.split_on_char ':' s with
    | [ node; s ] ->
      let connections = s |> CCString.trim |> CCString.split_on_char ' ' in
      (node, connections)
    | _ -> failwith "invalid case"
;;

let devices = File.read_list_of_line parse_line Sys.argv.(1)

let devices_map = StringHashtbl.of_list devices

let bool_to_string b =
    if b then
      "1"
    else
      "0"
;;

let solve devices_map =
    let cached = StringHashtbl.create 50000 in
    let rec dfs node dac fft =
        let dac = dac || node = "dac" in
        let fft = fft || node = "fft" in

        let key = node ^ "|" ^ bool_to_string dac ^ "|" ^ bool_to_string fft in

        match StringHashtbl.find_opt cached key with
        | Some n -> n
        | None ->
          ( match node with
            | "out" ->
              if dac && fft then
                1
              else
                0
            | node -> begin
              let connections =
                  StringHashtbl.get_or devices_map node ~default:[]
              in

              let paths =
                  connections
                  |> CCList.fold_left
                       (fun acc connection -> acc + dfs connection dac fft)
                       0
              in

              StringHashtbl.add cached key paths;
              paths
            end )
    in

    dfs "svr" false false
;;

let res = solve devices_map

let () = print_int res
