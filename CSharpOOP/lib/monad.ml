(** Copyright 2023-2024, Julia Kononova *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type error =
  | Occurs_check
  | No_variable of string
  | Mismatch
  | Other of string

let pp_error ppf : error -> _ = function
  | Occurs_check -> Format.fprintf ppf "Occurs check failed"
  | No_variable x -> Format.fprintf ppf "Undefined variable '%s'" x
  | Mismatch -> Format.fprintf ppf "Mismatch"
  | Other x -> Format.fprintf ppf "%s" x
;;

module Monad_SE = struct
  type ('st, 'a) t = 'st -> 'st * ('a, error) Result.t

  let return : 'a -> ('st, 'a) t = fun x st -> st, Result.Ok x
  let fail : 'a -> ('st, 'b) t = fun er st -> st, Result.Error er

  let ( >>= ) : ('st, 'a) t -> ('a -> ('st, 'b) t) -> ('st, 'b) t =
    fun x f st ->
    let st, x = x st in
    match x with
    | Result.Ok x -> f x st
    | Result.Error er -> fail er st
  ;;

  let ( >>| ) : ('st, 'a) t -> ('a -> 'b) -> ('st, 'b) t =
    fun x f st ->
    match x st with
    | st, Result.Ok x -> return (f x) st
    | st, Result.Error er -> fail er st
  ;;

  let ( <|> ) : ('st, 'a) t -> ('st, 'a) t -> ('st, 'a) t =
    fun x1 x2 st ->
    let st, x = x1 st in
    match x with
    | Result.Ok x -> return x st
    | Result.Error _ -> x2 st
  ;;

  let ( *> ) : ('st, 'a) t -> ('st, 'b) t -> ('st, 'b) t = fun x1 x2 -> x1 >>= fun _ -> x2

  let ( <* ) : ('st, 'a) t -> ('st, 'b) t -> ('st, 'a) t =
    fun x1 x2 -> x1 >>= fun x -> x2 >>| fun _ -> x
  ;;

  let lift2 : ('a -> 'b -> 'c) -> ('st, 'a) t -> ('st, 'b) t -> ('st, 'c) t =
    fun f a b -> a >>= fun r_a -> b >>= fun r_b -> return @@ f r_a r_b
  ;;

  let lift3
    : ('a -> 'b -> 'c -> 'd) -> ('st, 'a) t -> ('st, 'b) t -> ('st, 'c) t -> ('st, 'd) t
    =
    fun f a b c -> lift2 f a b >>= fun f -> c >>| f
  ;;

  let lift4
    :  ('a -> 'b -> 'c -> 'd -> 'e) -> ('st, 'a) t -> ('st, 'b) t -> ('st, 'c) t
    -> ('st, 'd) t -> ('st, 'e) t
    =
    fun f a b c d -> lift3 f a b c >>= fun f -> d >>| f
  ;;

  let read : ('st, 'st) t = fun st -> return st st
  let write : 'st -> ('st, unit) t = fun new_st _ -> new_st, Result.Ok ()
  let run : ('st, 'a) t -> 'st -> 'st * ('a, error) Result.t = fun f st -> f st

  (* TODO: let iter : *)
end

module Monad_TypeCheck = struct
  open Ast
  open State_type.TypeCheck
  open State_type
  include Monad_SE

  type 'a t = (TypeCheck.st_type_check, 'a) Monad_SE.t

  let return_s_n = function
    | Some x -> return x
    | None -> fail Occurs_check
  ;;

  let read_local_el name =
    read
    >>= function
    | _, l, _, _ -> MapName.find_opt name l |> return_s_n
  ;;

    let read_global_el name =
    read
    >>= function
    | g, _, _, _ -> MapName.find_opt name g |> return_s_n
  ;; 
  let get_cur_class_name : cur_class_name t =
    read
    >>= function
    | _, _, n, _ -> return n
  ;;
end
