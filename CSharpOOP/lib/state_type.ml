(** Copyright 2023-2024, Julia Kononova *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

module Name = struct
  type t = name

  let compare = compare
end

module MapName = Map.Make (Name)

module TypeCheck = struct
  type context = objects
  type available_members = member MapName.t
  type ast = (context * available_members) MapName.t

  type obj_content =
    | VType of var_type
    | Method of methods
    | Field of field
    | Constructor of constructor

  type local_env = obj_content MapName.t
  type cur_class_name = name
  type st_type_check = ast * local_env * cur_class_name * meth_type option
end
