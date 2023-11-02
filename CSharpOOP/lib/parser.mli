(** Copyright 2021-2023, Julia Kononova *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

val parse : 'a Angstrom.t -> string -> ('a, string) result
val e_method : methods Angstrom.t
