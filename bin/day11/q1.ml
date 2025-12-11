open Lib

module StringHashtbl = CCHashtbl.Make (CCString)

module StringSet = CCSet.Make (CCString)

let parse_line (s : string) =
    match CCString.split_on_char ':' s with
    | [ node; s ] ->
      let connections = s |> CCString.trim |> CCString.split_on_char ' ' in
      (node, connections)
    | _ -> failwith "invalid case"
;;

let devices = File.read_list_of_line parse_line Sys.argv.(1)

let devices_map = StringHashtbl.of_list devices

let solve devices_map =
    let count = ref 0 in
    let visited = ref StringSet.empty in
    let rec dfs node =
        match node with
        | "out" -> count := !count + 1
        | node when StringSet.mem node !visited -> ()
        | node -> begin
          visited := StringSet.add node !visited;

          let connections = StringHashtbl.get_or devices_map node ~default:[] in

          connections |> CCList.iter (fun connection -> dfs connection);

          visited := StringSet.remove node !visited
        end
    in

    dfs "you";

    !count
;;

let res = solve devices_map

let () = print_int res
