(** Copyright 2023-2024, Julia Kononova *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

module Name = struct
  type t = name

  let compare = compare
end

module MapName = Map.Make (Name)

type adr = Adr of int

module Adr = struct
  type t = adr

  let compare = compare
end

module MapAdr = Map.Make (Adr)

module TypeCheck = struct
  type obj_content =
    | VType of var_type
    | Method of methods
    | Field of field
    | Constructor of constructor

  type context =
    | TC_Class of obj_class
    | TC_Interface of interface * obj_content MapName.t

  type global_env = context MapName.t
  type local_env = obj_content MapName.t
  type cur_class_name = name
  type class_with_main_method = name

  type st_type_check =
    global_env
    * local_env
    * cur_class_name option
    * meth_type option
    * class_with_main_method option
end

(* module St_Interpreter = struct
   type mems = Field of field
   type members = mems MapName.t
   type memory = members MapAdr.t
   type loc_adr = adr
   type local_env = field MapAdr.t
   type st_interpreter = global_env * local_env * loc_adr * memory * adr
   end *)
