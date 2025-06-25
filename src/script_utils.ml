let rec find ?(recursive = false)
    ?(filter : [ `Files | `Dirs | `Sat of string -> bool ] option) dir =
  assert (Sys.is_directory dir);
  let handle = Unix.opendir dir in
  let rec loop acc =
    match Unix.readdir handle with
    | exception End_of_file -> acc
    | d when String.starts_with ~prefix:"." d -> loop acc
    | d ->
        let f =
          match filter with
          | None -> fun _ -> true
          | Some `Files -> Sys.is_regular_file
          | Some `Dirs -> Sys.is_directory
          | Some (`Sat f) -> f
        in
        let full_path = Filename.concat dir d in
        let acc =
          if recursive && Sys.is_directory full_path then
            find ~recursive ?filter full_path @ acc
          else acc
        in
        if f full_path then loop (full_path :: acc) else loop acc
  in
  let result = loop [] in
  Unix.closedir handle;
  result

let copy_rec category files target =
  if not (Sys.file_exists target) then Unix.mkdir target 0o755;
  List.map files ~f:(fun source ->
      let basename = Filename.basename source in
      let target = Filename.concat target basename in
      let command = Format.ksprintf Sys.command "cp -r %s %s" source target in
      if command <> 0 then
        Format.eprintf "ERROR: could not copy files form '%s' to '%s'@." source
          target;
      (category, source))

let copy src dst =
  let data = In_channel.with_open_text src In_channel.input_all in
  Out_channel.with_open_text dst (fun oc -> Out_channel.output_string oc data)
