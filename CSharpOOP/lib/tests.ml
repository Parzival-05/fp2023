(** Copyright 2023-2024, Julia Kononova *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Csharp_lib.Parser
open Csharp_lib.Ast

(* PARSERS *)
let show_wrap form = function
  | Some x -> Format.printf "%a@\n" form x
  | _ -> Format.print_string "Some error\n"
;;

let print_pars ps pp str = show_wrap pp (parse_to_some ps str)

(* TESTS *)

(* statements tests*)
let test_decl = print_pars s_declaration pp_statement

let%expect_test _ =
  test_decl "   int    name = -(25 - 9) *  !7";
  [%expect
    {|
      (Decl ((Var_Declaration ((TVar TInt), (Name "name"))),
         (Some (Bin_op (Asterisk,
                  (Un_op (Minus,
                     (Bin_op (Dash, (Exp_Const (VInt 25)), (Exp_Const (VInt 9)))))),
                  (Un_op (Not, (Exp_Const (VInt 7)))))))
         )) |}]
;;

let test_body = print_pars p_body pp_statement

let%expect_test _ =
  test_body
    "{if (true) \n\
    \      { a(); \n\
    \        if(false) \n\
    \          {\n\
    \            e    = b; \n\
    \            return;\n\
    \          } else \n\
    \            {\n\
    \              int    exmp = 243 + 1;\n\
    \            }\n\
    \          }; ; ;     ; \n\
    \          a(1+2 , cl)  ; ;   ; \n\
    \          if (1+ run()) \n\
    \                {\n\
    \                  first(1);\n\
    \                } else if (true) {} \n\
    \          return 1+1; ; ;\n\
    \  }";
  [%expect
    {|
    (Body
       [(If ((Exp_Const (VBool true)),
           (Body
              [(Expr (Method_invoke ((Exp_Name (Name "a")), (Args []))));
                (If ((Exp_Const (VBool false)),
                   (Body
                      [(Expr
                          (Bin_op (Assign, (Exp_Name (Name "e")),
                             (Exp_Name (Name "b")))));
                        (Return None)]),
                   (Some (Body
                            [(Decl (
                                (Var_Declaration ((TVar TInt), (Name "exmp"))),
                                (Some (Bin_op (Plus, (Exp_Const (VInt 243)),
                                         (Exp_Const (VInt 1)))))
                                ))
                              ]))
                   ))
                ]),
           None));
         (Expr
            (Method_invoke ((Exp_Name (Name "a")),
               (Args
                  [(Bin_op (Plus, (Exp_Const (VInt 1)), (Exp_Const (VInt 2))));
                    (Exp_Name (Name "cl"))])
               )));
         (If (
            (Bin_op (Plus, (Exp_Const (VInt 1)),
               (Method_invoke ((Exp_Name (Name "run")), (Args []))))),
            (Body
               [(Expr
                   (Method_invoke ((Exp_Name (Name "first")),
                      (Args [(Exp_Const (VInt 1))]))))
                 ]),
            (Some (If ((Exp_Const (VBool true)), (Body []), None)))));
         (Return
            (Some (Bin_op (Plus, (Exp_Const (VInt 1)), (Exp_Const (VInt 1))))))
         ]) |}]
;;

let test_fac = print_pars c_method pp_member

let%expect_test _ =
  test_fac
    "int Fac(int num)\n\
    \    {\n\
    \        if (num == 1)\n\
    \        {\n\
    \            return 1;\n\
    \        }\n\
    \        else \n\
    \        {\n\
    \            return num * Fac(num - 1);\n\
    \        };\n\
    \        }";
  [%expect
    {|
      (CMethod (
         { m_acc_modifier = None; m_poly_modifier = None; m_type = (TRetrun TInt);
           m_name = (Name "Fac");
           m_params = (Params [(Var_Declaration ((TVar TInt), (Name "num")))]) },
         (Body
            [(If ((Bin_op (Equal, (Exp_Name (Name "num")), (Exp_Const (VInt 1)))),
                (Body [(Return (Some (Exp_Const (VInt 1))))]),
                (Some (Body
                         [(Return
                             (Some (Bin_op (Asterisk, (Exp_Name (Name "num")),
                                      (Method_invoke ((Exp_Name (Name "Fac")),
                                         (Args
                                            [(Bin_op (Dash,
                                                (Exp_Name (Name "num")),
                                                (Exp_Const (VInt 1))))
                                              ])
                                         ))
                                      ))))
                           ]))
                ))
              ])
         )) |}]
;;

(* class tests *)
let test_class = print_pars p_class pp_objects

let%expect_test _ =
  test_class
    "class Test {\n\
    \  int Fac(int num) {\n\
    \    if (num == 1)\n\
    \    {\n\
    \        return 1;\n\
    \    }\n\
    \    else\n\
    \    {\n\
    \        return num * Fac(num - 1);\n\
    \    }\n\
    \  }\n\
     }";
  [%expect
    {|
  (Class
     { cl_modifier = None; cl_name = (Name "Test"); cl_parent = None;
       cl_body =
       [(CMethod (
           { m_acc_modifier = None; m_poly_modifier = None;
             m_type = (TRetrun TInt); m_name = (Name "Fac");
             m_params =
             (Params [(Var_Declaration ((TVar TInt), (Name "num")))]) },
           (Body
              [(If (
                  (Bin_op (Equal, (Exp_Name (Name "num")), (Exp_Const (VInt 1))
                     )),
                  (Body [(Return (Some (Exp_Const (VInt 1))))]),
                  (Some (Body
                           [(Return
                               (Some (Bin_op (Asterisk,
                                        (Exp_Name (Name "num")),
                                        (Method_invoke (
                                           (Exp_Name (Name "Fac")),
                                           (Args
                                              [(Bin_op (Dash,
                                                  (Exp_Name (Name "num")),
                                                  (Exp_Const (VInt 1))))
                                                ])
                                           ))
                                        ))))
                             ]))
                  ))
                ])
           ))
         ]
       }) |}]
;;

let%expect_test _ =
  test_class
    "class Test {\n\
    \  Person p = new Person();\n\
    \      int Sum(int num1, int num2) {\n\
    \    int a = num1;\n\
    \    int b = num2;\n\
    \    int a = a + b;\n\
    \    int b = a+6;\n\
    \    Person p = new Person();\n\
    \        return a;\n\
    \     }\n\
    \    }";
  [%expect
    {|
    (Class
       { cl_modifier = None; cl_name = (Name "Test"); cl_parent = None;
         cl_body =
         [(CField (
             { f_modifier = None; f_type = (TVar (TObj (Name "Person")));
               f_name = (Name "p") },
             (Some (Un_op (New,
                      (Method_invoke ((Exp_Name (Name "Person")), (Args []))))))
             ));
           (CMethod (
              { m_acc_modifier = None; m_poly_modifier = None;
                m_type = (TRetrun TInt); m_name = (Name "Sum");
                m_params =
                (Params
                   [(Var_Declaration ((TVar TInt), (Name "num1")));
                     (Var_Declaration ((TVar TInt), (Name "num2")))])
                },
              (Body
                 [(Decl ((Var_Declaration ((TVar TInt), (Name "a"))),
                     (Some (Exp_Name (Name "num1")))));
                   (Decl ((Var_Declaration ((TVar TInt), (Name "b"))),
                      (Some (Exp_Name (Name "num2")))));
                   (Decl ((Var_Declaration ((TVar TInt), (Name "a"))),
                      (Some (Bin_op (Plus, (Exp_Name (Name "a")),
                               (Exp_Name (Name "b")))))
                      ));
                   (Decl ((Var_Declaration ((TVar TInt), (Name "b"))),
                      (Some (Bin_op (Plus, (Exp_Name (Name "a")),
                               (Exp_Const (VInt 6)))))
                      ));
                   (Decl (
                      (Var_Declaration ((TVar (TObj (Name "Person"))), (Name "p")
                         )),
                      (Some (Un_op (New,
                               (Method_invoke ((Exp_Name (Name "Person")),
                                  (Args [])))
                               )))
                      ));
                   (Return (Some (Exp_Name (Name "a"))))])
              ))
           ]
         }) |}]
;;

(* ast tests*)
let test_ast = print_pars p_ast pp_program

let%expect_test _ =
  test_ast
    "class Dog {\n\
    \    public string name = \"Undefined\";\n\
    \    public int age;\n\
    \    string breed;\n\n\
    \    public void bark() {\n\
    \      Console.WriteLine(\"Bark Bark !!\");\n\
    \    }\n\
    \    }";
  [%expect
    {|
      (Ast
         [(Class
             { cl_modifier = None; cl_name = (Name "Dog"); cl_parent = None;
               cl_body =
               [(CField (
                   { f_modifier = (Some Public); f_type = (TVar TString);
                     f_name = (Name "name") },
                   (Some (Exp_Const (VString "Undefined")))));
                 (CField (
                    { f_modifier = (Some Public); f_type = (TVar TInt);
                      f_name = (Name "age") },
                    None));
                 (CField (
                    { f_modifier = None; f_type = (TVar TString);
                      f_name = (Name "breed") },
                    None));
                 (CMethod (
                    { m_acc_modifier = (Some Public); m_poly_modifier = None;
                      m_type = TVoid; m_name = (Name "bark");
                      m_params = (Params []) },
                    (Body
                       [(Expr
                           (Method_invoke (
                              (Access_By_Point ((Exp_Name (Name "Console")),
                                 (Exp_Name (Name "WriteLine")))),
                              (Args [(Exp_Const (VString "Bark Bark !!"))]))))
                         ])
                    ))
                 ]
               })
           ]) |}]
;;

let%expect_test _ =
  test_ast
    "public interface Animal {\n\
    \  string name;\n\
    \  int age;\n\
    \  void animalSound();\n\
    \  void run();\n\
    \ }\n\
    \ class Pig: Animal {\n\
    \    public string name = \"Undefined\";\n\
    \    public int age;\n\
    \    public int Sum(int a) {\n\
    \            animalSound(a, b);\n\
    \    };\n\
    \        public Pig(int name) : base(name, age){};\n\
    \        public void animalSound() {\n\
    \      Console.WriteLine(\"The pig says: wee wee\");\n\
    \    }\n\
    \  }\n\
    \ class Dog {\n\
    \    public string name = \"Undefined\";\n\
    \    public int age;\n\
    \    string breed;\n\n\
    \    public void bark() {\n\
    \      Console.WriteLine(\"Bark Bark !!\");\n\
    \    }\n\
    \    }\n\
    \  ";
  [%expect
    {|
    (Ast
       [(Interface
           { i_modifier = (Some Public); i_name = (Name "Animal");
             i_parent = None;
             i_body =
             [(IField
                 { f_modifier = None; f_type = (TVar TString);
                   f_name = (Name "name") });
               (IField
                  { f_modifier = None; f_type = (TVar TInt);
                    f_name = (Name "age") });
               (IMethod
                  { m_acc_modifier = None; m_poly_modifier = None;
                    m_type = TVoid; m_name = (Name "animalSound");
                    m_params = (Params []) });
               (IMethod
                  { m_acc_modifier = None; m_poly_modifier = None;
                    m_type = TVoid; m_name = (Name "run"); m_params = (Params [])
                    })
               ]
             });
         (Class
            { cl_modifier = None; cl_name = (Name "Pig");
              cl_parent = (Some (Name "Animal"));
              cl_body =
              [(CField (
                  { f_modifier = (Some Public); f_type = (TVar TString);
                    f_name = (Name "name") },
                  (Some (Exp_Const (VString "Undefined")))));
                (CField (
                   { f_modifier = (Some Public); f_type = (TVar TInt);
                     f_name = (Name "age") },
                   None));
                (CMethod (
                   { m_acc_modifier = (Some Public); m_poly_modifier = None;
                     m_type = (TRetrun TInt); m_name = (Name "Sum");
                     m_params =
                     (Params [(Var_Declaration ((TVar TInt), (Name "a")))]) },
                   (Body
                      [(Expr
                          (Method_invoke ((Exp_Name (Name "animalSound")),
                             (Args [(Exp_Name (Name "a")); (Exp_Name (Name "b"))])
                             )))
                        ])
                   ));
                (CConstructor (
                   { c_modifier = (Some Public); c_name = (Name "Pig");
                     c_params =
                     (Params [(Var_Declaration ((TVar TInt), (Name "name")))]);
                     c_base =
                     (Some (Args
                              [(Exp_Name (Name "name")); (Exp_Name (Name "age"))]))
                     },
                   (Body [])));
                (CMethod (
                   { m_acc_modifier = (Some Public); m_poly_modifier = None;
                     m_type = TVoid; m_name = (Name "animalSound");
                     m_params = (Params []) },
                   (Body
                      [(Expr
                          (Method_invoke (
                             (Access_By_Point ((Exp_Name (Name "Console")),
                                (Exp_Name (Name "WriteLine")))),
                             (Args
                                [(Exp_Const (VString "The pig says: wee wee"))])
                             )))
                        ])
                   ))
                ]
              });
         (Class
            { cl_modifier = None; cl_name = (Name "Dog"); cl_parent = None;
              cl_body =
              [(CField (
                  { f_modifier = (Some Public); f_type = (TVar TString);
                    f_name = (Name "name") },
                  (Some (Exp_Const (VString "Undefined")))));
                (CField (
                   { f_modifier = (Some Public); f_type = (TVar TInt);
                     f_name = (Name "age") },
                   None));
                (CField (
                   { f_modifier = None; f_type = (TVar TString);
                     f_name = (Name "breed") },
                   None));
                (CMethod (
                   { m_acc_modifier = (Some Public); m_poly_modifier = None;
                     m_type = TVoid; m_name = (Name "bark");
                     m_params = (Params []) },
                   (Body
                      [(Expr
                          (Method_invoke (
                             (Access_By_Point ((Exp_Name (Name "Console")),
                                (Exp_Name (Name "WriteLine")))),
                             (Args [(Exp_Const (VString "Bark Bark !!"))]))))
                        ])
                   ))
                ]
              })
         ]) |}]
;;

let%expect_test _ =
  test_ast "";
  [%expect {| (Ast []) |}]
;;
