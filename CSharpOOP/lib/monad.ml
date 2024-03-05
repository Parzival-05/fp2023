(** Copyright 2023-2024, Julia Kononova *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type error =
  | Occurs_check
  | No_variable of string
  | Mismatch
  | Other of string
  | Impossible_result

let pp_error ppf : error -> _ = function
  | Occurs_check -> Format.fprintf ppf "Occurs check failed"
  | No_variable x -> Format.fprintf ppf "Undefined variable '%s'" x
  | Mismatch -> Format.fprintf ppf "Mismatch"
  | Impossible_result -> Format.fprintf ppf "Impossible result"
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

  let map : ('a -> ('st, 'b) t) -> 'a list -> ('st, 'b list) t =
    fun f list ->
    let f acc el = acc >>= fun acc -> f el >>= fun el -> return (el :: acc) in
    List.fold_left f (return []) list >>| List.rev
  ;;

  let iter : ('a -> ('st, unit) t) -> 'a list -> ('st, unit) t =
    fun f list ->
    let f acc el = acc *> f el *> return () in
    List.fold_left f (return ()) list
  ;;
end

module Monad_TypeCheck = struct
  open Ast
  open State_type.TypeCheck
  open State_type
  include Monad_SE

  type 'a t = (TypeCheck.st_type_check, 'a) Monad_SE.t

  let return_with_fail = function
    | Some x -> return x
    | None -> fail Occurs_check
  ;;

  let read_local : 'a MapName.t t =
    read
    >>= function
    | _, l, _, _, _ -> return l
  ;;

  let read_local_el name f = read_local >>= fun l -> MapName.find_opt name l |> f
  let read_local_el_opt name = read_local_el name return
  let read_local_el name = read_local_el name return_with_fail

  let read_global : 'a MapName.t t =
    read
    >>= function
    | g, _, _, _, _ -> return g
  ;;

  let read_global_el name f = read_global >>= fun g -> MapName.find_opt name g |> f
  let read_global_el_opt name = read_global_el name return
  let read_global_el name = read_global_el name return_with_fail

  let get_cur_class_name : cur_class_name t =
    read
    >>= function
    | _, _, Some n, _, _ -> return n
    | _ -> fail Impossible_result
  ;;

  let read_meth_type : meth_type option t =
    read
    >>= function
    | _, _, _, m_t, _ -> return m_t
  ;;

  let read_main_class : class_with_main_method option t =
    read
    >>= function
    | _, _, _, _, main -> return main
  ;;

  let write_local n_l =
    read
    >>= function
    | g, _, n, m, main -> write (g, n_l, n, m, main)
  ;;

  let write_local_el el_name el_ctx =
    read_local >>= fun l -> write_local (MapName.add el_name el_ctx l)
  ;;

  let write_meth_type_opt m_t =
    read
    >>= function
    | g, l, n, _, main -> write (g, l, n, m_t, main)
  ;;

  let write_meth_type m_t = write_meth_type_opt (Some m_t)

  let write_main_class main =
    read
    >>= function
    | g, l, n, m_t, _ -> write (g, l, n, m_t, main)
  ;;

  let write_global n_g =
    read
    >>= function
    | _, l, n, m, main -> write (n_g, l, n, m, main)
  ;;

  let write_global_el el_name el_ctx =
    read_global >>= fun g -> write_global (MapName.add el_name el_ctx g)
  ;;

  let write_cur_class_name n =
    read
    >>= function
    | g, l, _, t, main -> write (g, l, Some n, t, main)
  ;;
end

(*
   module Monad_Interpreter = struct
   open Ast
   open State_type.St_Interpreter
   open State_type
   include Monad_SE

   type 'a t = (St_Interpreter.st_interpreter, 'a) Monad_SE.t

   end *)
