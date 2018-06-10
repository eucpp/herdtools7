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

(** Nvidia PTX architecture, base definitions *)

open Printf

let arch = Archs.ptx

(*************)
(* Registers *)
(*************)

type reg =
  (* numeric registers *)
  | R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 |
  (* predicate registers *)
  | P0 | P1 | P2 | P3 | P4 | P5 | P6 | P7 | P8 | P9 |
  (* program counter *)
  | PC

let regs =
  [
    R0, "r0";
    R1, "r1";
    R2, "r2";
    R3, "r3";
    R4, "r4";
    R5, "r5";
    R6, "r6";
    R7, "r7";
    R8, "r8";
    R9, "r9";
    P0, "p0";
    P1, "p1";
    P2, "p2";
    P3, "p3";
    P4, "p4";
    P5, "p5";
    P6, "p6";
    P7, "p7";
    P8, "p8";
    P9, "p9";
    PC, "PC";
  ]

let parse_list =
  List.map (fun (r, s) -> (s, r)) regs

let parse_reg s =
  try Some (List.assoc s parse_list)
  with Not_found -> None

let pp_reg r =
  try List.assoc r regs
  with Not_found -> assert false

let reg_compare = Pervasives.compare

(**********)
(* Scopes *)
(**********)

type scope = CTA | GPU | SYS

let parse_scope =
  | "cta" -> CTA
  | "gpu" -> GPU
  | "sys" -> SYS
  | _ -> None

let pp_scope =
  | CTA -> "cta"
  | GPU -> "gpu"
  | SYS -> "sys"


(************)
(* Barriers *)
(************)

type barrier_sem =
  | SC
  | ACQ_REL

let pp_barrier_sem =
  | SC -> "sc"
  | ACQ_REL -> "acq_rel"

type barrier =
  | Fence of barrier_sem * scope

let all_kinds_of_barriers =
  [
    Fence (SC, CTA); Fence (SC, GPU); Fence (SC, SYS);
    Fence (ACQ_REL, CTA); Fence (ACQ_REL, GPU); Fence (ACQ_REL, SYS);
  ]

let pp_barrier =
  | Fence (sem, scp) -> sprintf "fence.%s.%s" (pp_barrier_sem sem) (pp_scope scp)

let barrier_compare = Pervasives.compare

(****************)
(* Instructions *)
(****************)

type operand =
  | Operand_reg of reg
  | Operand_imm of int

type op_type =
  | S16
  | S32
  | S64
  | U16
  | U32
  | U64
  | B16
  | B32
  | B64
  | PRED

type cmp_op =
  | C_EQ
  | C_NEQ
  | C_LE
  | C_LT
  | C_GE
  | C_GT

type bool_op =
  | B_AND
  | B_OR
  | B_XOR

type op_sem =
  | WEAK
  | RLX
  | REL
  | ACQ

type instruction_base =
  | I_ADD of op_type * reg * operand * operand
  | I_AND of op_type * reg * operand * operand
  | P_MOV of op_type * reg * operand
  | I_SETP of cmp_op * bool_op  * op_type * reg * operand * operand
  | I_LD of op_sem * scope * op_type * reg * reg
  | I_ST of op_sem * scope * op_type * reg * reg
  | I_FENCE of barrier

