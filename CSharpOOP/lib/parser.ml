(** Copyright 2021-2023, Julia Kononova *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Angstrom
open Ast

let is_token = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false
;;

let is_first_name_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false
;;

let is_keyword = function
  | "abstract"
  | "base"
  | "bool"
  | "break"
  | "char"
  | "class"
  | "else"
  | "false"
  | "if"
  | "int"
  | "null"
  | "private"
  | "protected"
  | "public"
  | "return"
  | "string"
  | "true"
  | "void" -> true
  | _ -> false
;;

let space = take_while Char.is_whitespace
let p_space p = space *> p
let read_str p = space *> string p
let parens p = space *> char '(' *> p <* space <* char ')'
let parens2 p = space *> char '{' *> p <* space <* char '}'
let p_str = char '\"' *> take_till (Char.equal '\"') <* char '\"'
let p_token = take_while1 is_token
let read_word = p_space p_token

(* VALUES PARSE *)
let check test str =
  try test str |> fun _ -> true with
  | Failure _ -> false
  | Invalid_argument _ -> false
;;

let p_integer =
  let r_integer i = return (VInt i) in
  p_token
  >>= fun num ->
  match check int_of_string num with
  | true -> int_of_string num |> r_integer
  | false -> fail "This is not a number"
;;

let p_char = char '\'' *> any_char <* char '\'' >>| fun c -> VChar c
let p_string = p_str >>= fun s -> return (VString s)

let p_bool =
  let r_bool i = return (VBool i) in
  p_token
  >>= fun b ->
  match check bool_of_string b with
  | true -> bool_of_string b |> r_bool
  | false -> fail "This is not a bool"
;;

let p_null =
  read_word
  >>= function
  | "null" -> return NULL
  | _ -> fail "This is not a null"
;;

let p_val =
  space *> choice [ p_bool; p_char; p_integer; p_string; p_null ]
  >>= fun v -> return (Exp_Val v)
;;

let p_name =
  let r_name i = return (Name i) in
  space *> p_token
  >>= fun n ->
  match is_keyword n with
  | true -> fail "This is not a name"
  | false -> if is_first_name_char n.[0] then r_name n else fail "This is not a name"
;;

(* TYPES PARSE *)
let p_q = space *> char '?' *> return true <|> return false

let base_converter = function
  | "bool" -> return TBool
  | "int" -> return TInt
  | "char" -> return TChar
  | _ -> fail "This is not a base type"
;;

let p_base_t t =
  p_q
  >>= function
  | true -> return (TNullable_type t)
  | false -> return (TNot_nullable_type (TBase t))
;;

let t_base_q = read_word >>= fun str -> base_converter str >>= fun t -> p_base_t t

let t_string =
  read_word
  >>= function
  | "string" -> return (TNot_nullable_type (TRef TString))
  | _ -> fail "This is not a string type"
;;

let t_void =
  read_word
  >>= function
  | "void" -> return TVoid
  | _ -> fail "This is not a void"
;;

let t_val = choice [ t_base_q; t_string ]
let t_method = choice [ t_val; t_void ]

(* EXPR PARSE *)
let return_bin_op str binop =
  read_str str *> return (fun expr1 expr2 -> Bin_op (binop, expr1, expr2))
;;

let return_un_op str unop = read_str str *> return (fun expr -> Un_op (unop, expr))
let add = return_bin_op "+" Plus
let sub = return_bin_op "-" Dash
let mul = return_bin_op "*" Asterisk
let div = return_bin_op "/" Slash
let bmod = return_bin_op "%" Mod
let eq = return_bin_op "==" Equal
let not_eq = return_bin_op "!=" NotEqual
let less = return_bin_op "<" Less
let less_or_eq = return_bin_op "<=" LessOrEqual
let more = return_bin_op ">" More
let more_or_eq = return_bin_op ">=" MoreOrEqual
let log_and = return_bin_op "&&" And
let log_or = return_bin_op "||" Or
let assign = return_bin_op "=" Assign
let minus = return_un_op "-" Minus
let u_not = return_un_op "!" Not

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let chainr1 e op =
  fix (fun go -> e >>= fun x -> op >>= (fun f -> go >>| fun g -> f x g) <|> return x)
;;

let p_list e str =
  let rec args lst e str =
    e >>= fun a -> read_str str *> args (lst @ [ a ]) e str <|> return (lst @ [ a ])
  in
  args [] e str
;;

let p_list1 e str = p_list e str <|> return []
let e_name = p_name >>| fun n -> Exp_Name n
let p_dot = read_str "." *> return (fun expr1 expr2 -> Access_By_Point (expr1, expr2))
let e_access_by_point = chainr1 e_name p_dot
let e_args e = parens @@ p_list1 e "," >>| fun e_l -> Exp_Args e_l
let method_invoke e = lift2 (fun n a -> Method_invoke (n, a)) e_access_by_point (e_args e)

let e_op =
  fix (fun expr ->
    let expr = parens expr <|> p_val <|> method_invoke expr <|> e_name in
    let expr = chainl1 expr (mul <|> div <|> bmod) in
    let expr = chainl1 expr (add <|> sub) in
    let expr = chainl1 expr (less_or_eq <|> more_or_eq <|> less <|> more) in
    let expr = chainl1 expr (eq <|> not_eq) in
    let expr = chainl1 expr log_and in
    let expr = chainl1 expr log_or in
    chainr1 expr assign)
;;

let e_op_assign = lift3 (fun f a x -> a f x) e_name assign e_op
let e_var_declaration = lift2 (fun t n -> Var_Declaration (t, n)) t_val p_name
let skip_eq_run_op = read_str "=" *> e_op

let e_assign =
  lift2 (fun vd op -> Exp_Assign (vd, op)) e_var_declaration skip_eq_run_op
  <|> e_var_declaration
;;

let p_return =
  read_word
  >>= function
  | "return" -> e_op >>= (fun e -> return (Return (Some e))) <|> return (Return None)
  | _ -> fail "This is not a return"
;;

let p_if_cond =
  read_word
  >>= function
  | "if" -> parens e_op
  | _ -> fail "This is not IF word"
;;

let p_else expr e_ie =
  read_str "else" *> (choice [ e_ie; expr ] >>= fun e -> return (Some e)) <|> return None
;;

let e_if_else expr =
  fix (fun if_else ->
    lift3 (fun i t e -> If_Else (i, t, e)) p_if_cond expr (p_else expr if_else))
;;

let semicolon = fix (fun sem -> read_str ";" *> sem <|> return ())
let semicolon1 = read_str ";" *> semicolon

let p_body =
  let read_body expr =
    parens2 (many expr >>| (fun e -> Exp_Body e) <|> return (Exp_Body []))
  in
  let e_choice b =
    choice
      [ e_op_assign <* semicolon1
      ; e_assign <* semicolon1
      ; p_return <* semicolon1
      ; method_invoke e_op <* semicolon1
      ; e_if_else b <* semicolon
      ]
  in
  fix (fun body -> read_body @@ e_choice body)
;;

let p_access_modifier = function
  | "public" -> return Public
  | "protected" -> return Protected
  | "private" -> return Private
  | _ -> fail "Is not an access modifier"
;;

let p_m_modifier =
  read_word
  >>= fun str ->
  match str with
  | "static" -> return Static
  | "override" -> return Override
  | _ -> p_access_modifier str >>= fun m -> return (MAccess m)
;;

let p_m_modifier_list = many p_m_modifier >>= fun l -> return (Some l) <|> return None

let p_m_name_args =
  lift2
    (fun n l -> n, l)
    p_name
    (parens @@ p_list1 e_var_declaration "," >>| fun m -> Exp_Args m)
;;

let e_method =
  lift4
    (fun p i (d, o) r -> Method (p, i, d, o, r))
    p_m_modifier_list
    t_method
    p_m_name_args
    p_body
;;

let parse p str = Angstrom.parse_string p ~consume:Angstrom.Consume.Prefix str

let parse_to_some p str =
  match parse p str with
  | Ok x -> Some x
  | Error _ -> None
;;

let show_wrap form = function
  | Some x -> Format.printf "%a@\n" form x
  | _ -> Format.print_string "Some error during parsing\n"
;;

let print_pars ps pp str = show_wrap pp (parse_to_some ps str)

(* ---------------------------TESTS--------------------------- *)
let test eq p s r =
  match parse_to_some p s with
  | Some x -> eq x r
  | None -> false
;;

(* number tests *)
let%test "s" = test equal_value p_integer "10" (VInt 10)
let%test "s" = test equal_value p_integer "1_2_3" (VInt 123)
let%test "s" = not (test equal_value p_integer "10sss" (VInt 9))
let%test "s" = not (test equal_value p_integer "ghk" (VInt 9))

(* bool tests *)
let%test "s" = test equal_value p_bool "true" (VBool true)
let%test "s" = test equal_value p_bool "false" (VBool false)
let%test "s" = not (test equal_value p_bool "true1" (VBool true))

(* string tests *)
let%test "s" = test equal_value p_string "\"hsjksda\"" (VString "hsjksda")
let%test "s" = not (test equal_value p_string "4\"hsjksda\"" (VString "4hsjksda"))

(* char tests *)
let%test "s" = test equal_value p_char "\'a\'" (VChar 'a')
let%test "s" = not (test equal_value p_char "\'ss\'" (VChar 's'))
let%test "s" = not (test equal_value p_char "dd" (VChar 'd'))

(* name tests *)
let%test "s" = test equal_name p_name "Sring" (Name "Sring")
let%test "s" = test equal_name p_name "_Sring" (Name "_Sring")
let%test "s" = test equal_name p_name "sring" (Name "sring")
let%test "s" = test equal_name p_name "s" (Name "s")
let%test "s" = not (test equal_name p_name "1Sring" (Name "1Sring"))
let%test "s" = not (test equal_name p_name "" (Name ""))
let%test "s" = not (test equal_name p_name "if" (Name "if"))
let%test "s" = test equal_name p_name "if1" (Name "if1")

(* type tests *)
let%test "s" = test equal_types t_val "string" (TNot_nullable_type (TRef TString))
let%test "s" = test equal_types t_val "   int     ?" (TNullable_type TInt)
let%test "s" = test equal_types t_val "   int   y" (TNot_nullable_type (TBase TInt))
let%test "s" = not (test equal_types t_val "int11" (TNot_nullable_type (TBase TInt)))
let%test "s" = not (test equal_types t_val "int?11" (TNot_nullable_type (TBase TInt)))
let%test "s" = not (test equal_types t_val "     ybool        ?" (TNullable_type TBool))

let%test "s" =
  test
    equal_expr
    e_var_declaration
    "   int  ?  name "
    (Var_Declaration (TNullable_type TInt, Name "name"))
;;

let%test "s" =
  test equal_expr e_op "1+2" (Bin_op (Plus, Exp_Val (VInt 1), Exp_Val (VInt 2)))
;;

let%test "s" =
  test
    equal_expr
    e_op
    "1+2*3"
    (Bin_op (Plus, Exp_Val (VInt 1), Bin_op (Asterisk, Exp_Val (VInt 2), Exp_Val (VInt 3))))
;;

let%test "s" =
  test
    equal_expr
    e_op
    "( 1     +28)*   num   "
    (Bin_op
       ( Asterisk
       , Bin_op (Plus, Exp_Val (VInt 1), Exp_Val (VInt 28))
       , Exp_Name (Name "num") ))
;;

let%test "s" =
  test
    equal_expr
    e_op
    "1>=2 &&        3"
    (Bin_op
       (And, Bin_op (MoreOrEqual, Exp_Val (VInt 1), Exp_Val (VInt 2)), Exp_Val (VInt 3)))
;;

let%test "s" =
  test
    equal_expr
    e_op
    "  num1 = 1+2*3 / 9"
    (Bin_op
       ( Assign
       , Exp_Name (Name "num1")
       , Bin_op
           ( Plus
           , Exp_Val (VInt 1)
           , Bin_op
               ( Slash
               , Bin_op (Asterisk, Exp_Val (VInt 2), Exp_Val (VInt 3))
               , Exp_Val (VInt 9) ) ) ))
;;

let%test "s" =
  test
    equal_expr
    e_assign
    "   int  ?  name = (25 + 9) *  7"
    (Exp_Assign
       ( Var_Declaration (TNullable_type TInt, Name "name")
       , Bin_op
           (Asterisk, Bin_op (Plus, Exp_Val (VInt 25), Exp_Val (VInt 9)), Exp_Val (VInt 7))
       ))
;;

let%test "s" =
  test
    equal_expr
    e_access_by_point
    "a.b.n.m"
    (Access_By_Point
       ( Exp_Name (Name "a")
       , Access_By_Point
           ( Exp_Name (Name "b")
           , Access_By_Point (Exp_Name (Name "n"), Exp_Name (Name "m")) ) ))
;;

let%test "s" =
  test
    equal_expr
    e_op
    "  a = b = n= m"
    (Bin_op
       ( Assign
       , Exp_Name (Name "a")
       , Bin_op
           ( Assign
           , Exp_Name (Name "b")
           , Bin_op (Assign, Exp_Name (Name "n"), Exp_Name (Name "m")) ) ))
;;

let%test "t" =
  test
    equal_expr
    p_body
    "{if (true) \n\
    \      { a(); \n\
    \        if(false) \n\
    \          {\n\
    \            e    = b; \n\
    \            return;\n\
    \          } else \n\
    \            {\n\
    \              int  ?   exmp = 243 + 1;\n\
    \            }\n\
    \          }; ; ;     ; \n\
    \          a(1+2 , cl)  ; ;   ; \n\
    \          if (1+ run()) \n\
    \                {\n\
    \                  first(1);\n\
    \                } else if (true) {} \n\
    \          return 1+1; ; ;\n\
    \  }"
    (Exp_Body
       [ If_Else
           ( Exp_Val (VBool true)
           , Exp_Body
               [ Method_invoke (Exp_Name (Name "a"), Exp_Args [])
               ; If_Else
                   ( Exp_Val (VBool false)
                   , Exp_Body
                       [ Bin_op (Assign, Exp_Name (Name "e"), Exp_Name (Name "b"))
                       ; Return None
                       ]
                   , Some
                       (Exp_Body
                          [ Exp_Assign
                              ( Var_Declaration (TNullable_type TInt, Name "exmp")
                              , Bin_op (Plus, Exp_Val (VInt 243), Exp_Val (VInt 1)) )
                          ]) )
               ]
           , None )
       ; Method_invoke
           ( Exp_Name (Name "a")
           , Exp_Args
               [ Bin_op (Plus, Exp_Val (VInt 1), Exp_Val (VInt 2)); Exp_Name (Name "cl") ]
           )
       ; If_Else
           ( Bin_op
               (Plus, Exp_Val (VInt 1), Method_invoke (Exp_Name (Name "run"), Exp_Args []))
           , Exp_Body
               [ Method_invoke (Exp_Name (Name "first"), Exp_Args [ Exp_Val (VInt 1) ]) ]
           , Some (If_Else (Exp_Val (VBool true), Exp_Body [], None)) )
       ; Return (Some (Bin_op (Plus, Exp_Val (VInt 1), Exp_Val (VInt 1))))
       ])
;;

let%test "s" = test equal_method_modifier p_m_modifier " private" (MAccess Private)
let test_fac = print_pars e_method pp_methods

let%expect_test _ =
  test_fac
    "static int Fac(int num)\n\
    \    {\n\
    \        if (num == 1)\n\
    \        {\n\
    \            return 1;\n\
    \        }\n\
    \        else \n\
    \        {\n\
    \            return num * Fac(num - 1);\n\
    \        }\n\
    \    }";
  [%expect
    {|
      (Method ((Some [Static]), (TNot_nullable_type (TBase TInt)), (Name "Fac"),
         (Exp_Args
            [(Var_Declaration ((TNot_nullable_type (TBase TInt)), (Name "num")))]),
         (Exp_Body
            [(If_Else (
                (Bin_op (Equal, (Exp_Name (Name "num")), (Exp_Val (VInt 1)))),
                (Exp_Body [(Return (Some (Exp_Val (VInt 1))))]),
                (Some (Exp_Body
                         [(Return
                             (Some (Bin_op (Asterisk, (Exp_Name (Name "num")),
                                      (Method_invoke ((Exp_Name (Name "Fac")),
                                         (Exp_Args
                                            [(Bin_op (Dash,
                                                (Exp_Name (Name "num")),
                                                (Exp_Val (VInt 1))))
                                              ])
                                         ))
                                      ))))
                           ]))
                ))
              ])
         )) |}]
;;
