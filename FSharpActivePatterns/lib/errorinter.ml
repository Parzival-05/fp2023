(** Copyright 2023-2024, Vitaliy Dyachkov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Typedtree

type error =
  | Division_by_zero
  | UnboundValue of string
  | UnboundConstructor of string
  | FunctionCompare
  | MatchFailure
  | EmptyProgram
  | TypeError
  | Unreachable
  | NotImplemented
  | Occurs_check
  | No_variable of string
  | Unification_failed of typ * typ
  | Empty_pattern
  | Empty_input
  | NotReachable

let pp_error fmt : error -> _ = function
  | Division_by_zero -> Format.fprintf fmt "Exception: Division_by_zero."
  | UnboundValue s -> Format.fprintf fmt "Error: Unbound value %s" s
  | UnboundConstructor s -> Format.fprintf fmt "Error: Unbound constructor %s" s
  | FunctionCompare ->
    Format.fprintf fmt "Exception: Invalid_argument \"compare: functional value\""
  | MatchFailure ->
    Format.fprintf fmt "Exception: this pattern-matching is not exhaustive."
  | EmptyProgram -> Format.fprintf fmt "Error: the program was not provided or was empty"
  | TypeError -> Format.fprintf fmt "Error: type mismatch, a different type was expected"
  | Unreachable ->
    Format.fprintf fmt "Error: Unreachable error... Something went wrong..."
  | NotImplemented -> Format.fprintf fmt "This feature has not yet been implemented"
  | Occurs_check -> Format.fprintf fmt "Typechecker error: occurs check failed"
  | No_variable s -> Format.fprintf fmt "Typechecker error: undefined variable '%s'" s
  | Unification_failed (l, r) ->
    Format.fprintf
      fmt
      "Typechecker error: unification failed on %a and %a"
      Pprinttypedtree.pp_typ_binder
      l
      Pprinttypedtree.pp_typ_binder
      r
  | Empty_pattern -> Format.fprintf fmt "Typechecker error: empty pattern"
  | Empty_input -> Format.fprintf fmt "Typechecker error: empty pattern"
  | NotReachable -> Format.fprintf fmt "This not reacheable"
;;