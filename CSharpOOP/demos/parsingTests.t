(** Copyright 2023-2024, Julia Kononova *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

  $ ./demoParse.exe <<-EOF
  > class Test {
  >   static int Fac(int num) 
  >   {
  >      if (num == 1)
  >      {
  >         return 1;
  >      }
  >      else
  >      {
  >         return num * Fac(num - 1);
  >      }
  >   }
  > }
  > EOF
  (Ast
     [(Class
         { cl_modifier = None; cl_name = (Name "Test"); cl_parent = None;
           cl_body =
           [(CMethod (
               { m_acc_modifier = None; m_poly_modifier = (Some Static);
                 m_type = (TRetrun TInt); m_name = (Name "Fac");
                 m_params =
                 (Params [(Var_Declaration ((TVar TInt), (Name "num")))]) },
               (Body
                  [(If (
                      (Bin_op (Equal, (Exp_Name (Name "num")),
                         (Exp_Const (VInt 1)))),
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
           })
       ])
