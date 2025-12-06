let read_list_of_line parse_line filename =
    let input_channel = open_in filename in
    let rec loop acc =
        match CCIO.read_line input_channel with
        | None ->
          close_in input_channel;
          acc
        | Some line ->
          let item = line |> CCString.trim |> parse_line in
          loop (item :: acc)
    in
    CCList.rev (loop [])
;;

let read_file filename =
    let input_channel = open_in filename in
    CCIO.read_all input_channel
;;
