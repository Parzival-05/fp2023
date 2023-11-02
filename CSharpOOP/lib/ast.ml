(** Copyright 2021-2023, Julia Kononova *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type value =
  | VInt of int
  | VChar of char
  | VBool of bool
  | VString of string
  | NULL (** Null value*)
[@@deriving show { with_path = false }, eq]

type ref_type =
  | TString
  | TClass
  | TInterface
[@@deriving show { with_path = false }, eq]

type base_type =
  | TInt
  | TChar
  | TBool
[@@deriving show { with_path = false }, eq]

type not_nullable_type =
  | TBase of base_type
  | TRef of ref_type
[@@deriving show { with_path = false }, eq]

type types =
  | TNot_nullable_type of not_nullable_type
  | TNullable_type of base_type
  | TVoid (** type [void] for methods *)
[@@deriving show { with_path = false }, eq]

type name = Name of string [@@deriving show { with_path = false }, eq]

type access_modifier =
  | Public
  | Protected
  | Private
[@@deriving show { with_path = false }, eq]

type field_modifier =
  | New
  | FAccess of access_modifier
[@@deriving show { with_path = false }, eq]

type class_modifier = CAccess of access_modifier
[@@deriving show { with_path = false }, eq]

type method_modifier =
  | MAccess of access_modifier
  | Static
  | Override
[@@deriving show { with_path = false }, eq]

type bin_op =
  | Plus (** + *)
  | Asterisk (** * *)
  | Dash (** - *)
  | Slash (** / *)
  | Mod (** % *)
  | Equal (** == *)
  | NotEqual (** != *)
  | Less (** < *)
  | LessOrEqual (** <= *)
  | More (** > *)
  | MoreOrEqual (** >= *)
  | And (** && *)
  | Or (** || *)
  | Assign (** = *)
[@@deriving show { with_path = false }, eq]

type un_op =
  | Not (** ! *)
  | Minus (** - *)
[@@deriving show { with_path = false }, eq]

type expr =
  | Bin_op of bin_op * expr * expr
  | Un_op of un_op * expr
  | Var_Declaration of types * name (** Variable declaration [int a] *)
  | Access_By_Point of expr * expr (**  Access by point to class member [a.b.c]*)
  | Exp_Assign of expr * expr (** Declare and assign a value to a variable [int a = 1]*)
  | Exp_Val of value
  | Exp_Name of name
  | Exp_Args of expr list
  | Method_invoke of expr * expr (** Calling a method [Foo.a(i, 9)] *)
  | Exp_Body of expr list (** Method body*)
  | Exp_Class of class_modifier option * name * member list option
  | If_Else of expr * expr * expr option
  | Return of expr option
[@@deriving show { with_path = false }, eq]

and field = Field of (*field_modifier list option **) types * name * expr option

and methods =
  | Main
  | Method of method_modifier list option * types * name * expr * expr

and member =
  | CField of field
  | CMethod of methods
