%{
(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module PTX = PTXBase
%}

%token EOF
%token <PTX.reg> ARCH_REG
%token <string> SYMB_REG
%token <int> NUM
%token <string> NAME
%token <int> PROC

%token SEMI COMMA DOT PIPE COLON LBRK RBRK COLON AMPERSAT BANG

%nonassoc SEMI

(* Instructons *)
%token I_ADD I_INC I_DEC I_AND I_OR I_XOR
%token I_MOV I_SETP I_LD I_ST I_CAS I_EXCH I_FENCE I_FENCE_SC

(* Scopes *)
%token <PTX.op_scope> OP_SCOPE

(* Semantics modifier *)
%token <PTX.op_sem> OP_SEM

(* Type modifier *)
%token <PTX.op_type> OP_TYPE

(* Comparison operators *)
%token <PTX.cmp_op> CMP_OP

(* Main *)
%type <int list * (PTX.pseudo) list list> main
%start  main

%%

main:
| semi_opt proc_list iol_list EOF
    { $2, $3 }

semi_opt:
| { () }
| SEMI { () }

proc_list:
| PROC SEMI {[$1]}
| PROC PIPE proc_list { $1::$3 }

iol_list :
|  instr_option_list SEMI
    {[$1]}
|  instr_option_list SEMI iol_list { $1::$3 }

instr_option_list :
  | instr_option
      { [$1] }
  | instr_option PIPE instr_option_list
      { $1::$3 }

instr_option :
| { Nop }
| NAME COLON instr_option { Label ($1, $3) }
| instr      { Instruction $1 }

instr :
| instr_guard_option instr_base
    { ($1, $2) }

instr_guard_option :
| { None }
| instr_guard { Some $1 }

instr_guad :
| AMPERSAT reg
    { PTX.Guard $2 }
| AMPERSAT BANG reg
    { PTX.Guard_Neg $3 }

instr_base :
| I_ADD DOT OP_TYPE reg COMMA operand COMMA operand
    { PTX.I_ADD ($3, $4, $6, $7) }
| I_AND DOT OP_TYPE reg COMMA operand COMMA operand
    { PTX.I_AND ($3, $4, $6, $7) }
| I_MOV DOT OP_TYPE reg COMMA operand
    { PTX.I_MOV ($3, $4, $6) }
| I_SETP DOT CMP_OP DOT OP_TYPE reg COMMA operand COMMA operand
    { PTX.I_SETP ($3, $5, $6, $8, $10) }
| I_LD DOT OP_SEM DOT OP_SCOPE DOT OP_TYPE reg COMMA LBRK reg RBRK
    { PTX.I_LD ($3, $5, $6, $8, $10) }
| I_ST DOT OP_SEM DOT OP_SCOPE DOT OP_TYPE LBRK reg RBRK COMMA reg
    { PTX.I_ST ($3, $5, $6, $8, $10) }
| I_ATOM DOT OP_SEM DOT OP_SCOPE DOT CAS DOT OP_TYPE reg COMMA LBRK reg RBRK COMMA operand COMMA operand
    { PTX.I_CAS ($3, $5, $9, $10, $13, $16, $18) }
| I_ATOM DOT OP_SEM DOT OP_SCOPE DOT ATOM_OP DOT OP_TYPE reg COMMA LBRK reg RBRK COMMA operand
    { PTX.I_ATOM ($3, $5, $7, $9, $10, $13, $16) }
| I_ATOM DOT OP_SEM DOT OP_SCOPE DOT I_AND DOT OP_TYPE reg COMMA LBRK reg RBRK COMMA operand
    { PTX.I_ATOM ($3, $5, PTX.A_AND, $9, $10, $13, $16) }
| I_ATOM DOT OP_SEM DOT OP_SCOPE DOT I_OR DOT OP_TYPE reg COMMA LBRK reg RBRK COMMA operand
    { PTX.I_ATOM ($3, $5, PTX.A_OR, $9, $10, $13, $16) }
| I_ATOM DOT OP_SEM DOT OP_SCOPE DOT I_XOR DOT OP_TYPE reg COMMA LBRK reg RBRK COMMA operand
    { PTX.I_ATOM ($3, $5, PTX.A_XOR, $9, $10, $13, $16) }
| I_ATOM DOT OP_SEM DOT OP_SCOPE DOT I_ADD DOT OP_TYPE reg COMMA LBRK reg RBRK COMMA operand
    { PTX.I_ATOM ($3, $5, PTX.A_ADD, $9, $10, $13, $16) }
| I_ATOM DOT OP_SEM DOT OP_SCOPE DOT I_EXCH DOT OP_TYPE reg COMMA LBRK reg RBRK COMMA operand
    { PTX.I_ATOM ($3, $5, PTX.A_EXCH, $9, $10, $13, $16) }
| I_ATOM DOT OP_SEM DOT OP_SCOPE DOT I_INC DOT OP_TYPE reg COMMA LBRK reg RBRK COMMA operand
    { PTX.I_ATOM ($3, $5, PTX.A_INC, $9, $10, $13, $16) }
| I_ATOM DOT OP_SEM DOT OP_SCOPE DOT I_DEC DOT OP_TYPE reg COMMA LBRK reg RBRK COMMA operand
    { PTX.I_ATOM ($3, $5, PTX.A_DEC, $9, $10, $13, $16) }
| I_FENCE DOT OP_SCOPE
    { PTX.I_FENCE (PTX.Fence $3) }
| I_FENCE_SC DOT OP_SCOPE
    { PTX.I_FENCE (PTX.FenceSC $3) }

reg:
(*| SYMB_REG { Symbolic_reg $1 }*)
| ARCH_REG { $1 }

operand:
| reg { PTX.Op_reg $1 }
| NUM { PTX.OP_imm $1 }