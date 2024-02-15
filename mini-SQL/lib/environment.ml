(** Copyright 2023-2024, Zaytsev Dmitriy *)

(** SPDX-License-Identifier: CC0-1.0 *)

open Types
open Utils

(*
   File.csv structure:
   +--------------------------------+
   numeric, real, (types ...)
   column1, column2, (column names ...)
   value, (values ...)
   +--------------------------------+
*)

module Env (M : Utils.MONAD_FAIL) = struct
  open M

  (* Just check & open *)
  let open_file file =
    if Filename.extension file = ".csv" && Sys.file_exists file
    then return (open_in file)
    else fail (IncorrectData file)
  ;;

  let get_csv_channel channel = return (Csv.of_channel ?separator:(Some ',') channel)
  let row (rows : Csv.t) i = return (List.nth rows i)
  let rows_after (rows : Csv.t) i = return (List.filteri (fun id _ -> id > i) rows)

  let column_type_of = function
    | "string" -> return String_Column
    | "numeric" -> return Numeric_Column
    | "real" -> return Real_Column
    | "bool" -> return Boolean_Column
    | x -> fail (UnknownType x)
  ;;

  let to_column_types_list strs = M.all (List.map column_type_of strs)

  let to_columns_list ns ts =
    let init_column n t = { column_name = n; column_type = t } in
    return (List.map2 init_column ns ts)
  ;;

  let to_table ~filename ~columns = { table_name = filename; table_header = columns }

  (* loads table from file.csv *)
  let load_table file =
    let csv_rows csv_channel = return (Csv.input_all csv_channel) in
    open_file file
    >>= fun file_channel ->
    get_csv_channel file_channel
    >>= fun csv_channel ->
    csv_rows csv_channel
    >>= fun data ->
    row data 0
    >>= fun row1 ->
    row data 1
    >>= fun row2 ->
    rows_after data 2
    >>= fun data_rows ->
    to_column_types_list row1
    >>= fun ctypes ->
    to_columns_list row2 ctypes
    >>= fun cols ->
    return
      { Table.data = Sheet.init ctypes data_rows
      ; Table.meta =
          to_table
            ~filename:(Filename.basename (Filename.remove_extension file))
            ~columns:(Array.of_list cols)
      }
  ;;

  let load_database folder =
    if Sys.is_directory folder
    then (
      let files = ref (Sys.readdir folder) in
      if Array.length files.contents > 0
      then (
        let rec helper acc list =
          match list with
          | [] -> acc
          | hd :: tl -> helper (acc @ [ load_table (folder ^ "/" ^ hd) ]) tl
        in
        M.all (helper [] (Array.to_list files.contents))
        >>= fun tables -> return { Database.tables; name = Filename.basename folder })
      else fail (IncorrectData folder))
    else fail (IncorrectData folder)
  ;;
end