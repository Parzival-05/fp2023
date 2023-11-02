(** Copyright 2021-2023, Julia Kononova *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  match Csharp_lib.Parser.parse Csharp_lib.Parser.e_method s with
  | Result.Ok x -> Format.printf "%a\n%!" Csharp_lib.Ast.pp_methods x
  | Error _ -> Format.printf "Some error with parsing"
;;