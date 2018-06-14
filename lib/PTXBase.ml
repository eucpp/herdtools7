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

let parse_scope = function
  | "cta" -> CTA
  | "gpu" -> GPU
  | "sys" -> SYS
  | _ -> None

let pp_scope = function
  | CTA -> "cta"
  | GPU -> "gpu"
  | SYS -> "sys"


(************)
(* Barriers *)
(************)

type barrier =
  | Fence of scope
  | FenceSC of scope

let all_kinds_of_barriers =
  [
    FenceSC CTA; FenceSC GPU; FenceSC SYS;
    Fence CTA; Fence GPU; Fence SYS;
  ]

let pp_barrier = function
  | Fence scp -> sprintf "fence.%s" (pp_scope scp)
  | FenceSC scp -> sprintf "fence.sc.%s" (pp_scope scp)

let barrier_compare = Pervasives.compare

(****************)
(* Instructions *)
(****************)

type operand =
  | Op_reg of reg
  | Op_imm of int

let pp_operand = function
  | Op_reg r -> pp_reg r
  | Op_imm i -> sprintf "%d" i

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

let pp_op_type = function
  | S16 -> "s16"
  | S32 -> "s32"
  | S64 -> "s64"
  | U16 -> "u16"
  | U32 -> "u32"
  | U64 -> "u64"
  | B16 -> "b16"
  | B32 -> "b32"
  | B64 -> "b64"
  | PRED -> "pred"

type cmp_op =
  | C_EQ
  | C_NE
  | C_LE
  | C_LT
  | C_GE
  | C_GT

let pp_cmp_op = function
  | C_EQ -> "eq"
  | C_NE -> "ne"
  | C_LE -> "le"
  | C_LT -> "lt"
  | C_GE -> "ge"
  | C_GT -> "gt"

type bool_op =
  | B_AND
  | B_OR
  | B_XOR

let pp_bool_op = function
  | B_AND -> "and"
  | B_OR -> "or"
  | B_XOR -> "xor"

type atom_op =
  | A_AND
  | A_OR
  | A_XOR
  | A_EXCH
  | A_ADD
  | A_INC
  | A_DEC

let pp_atom_op = function
  | A_AND -> "and"
  | A_OR -> "or"
  | A_XOR -> "xor"
  | A_EXCH -> "exch"
  | A_ADD -> "add"
  | A_INC -> "inc"
  | A_DEC -> "dec"

type op_sem =
  (*| WEAK*)
  | RLX
  | REL
  | ACQ
  | ACQ_REL

let pp_op_sem = function
  (*| WEAK -> "weak"*)
  | RLX -> "relaxed"
  | REL -> "release"
  | ACQ -> "acquire"
  | ACQ_REL -> "acq_rel"

type instruction_base =
  | I_ADD of op_type * reg * operand * operand
  | I_AND of op_type * reg * operand * operand
  | I_MOV of op_type * reg * operand
  | I_SETP of cmp_op * op_type * reg * operand * operand
  | I_LD of op_sem * scope * op_type * reg * reg
  | I_ST of op_sem * scope * op_type * reg * reg
  | I_CAS of op_sem * scope * op_type * reg * reg * operand * operand
  | I_ATOM of op_sem * scope * atom_op * op_type * reg * reg * operand
  | I_FENCE of barrier

let pp_instruction_base instr = match instr with
  | I_ADD (typ, r, a, b) ->
    sprintf "add.%s %s, %s, %s"
      (pp_op_type typ) (pp_reg r) (pp_operand a) (pp_operand b)
  | I_AND (typ, r, a, b) ->
    sprintf "and.%s %s, %s, %s"
      (pp_op_type typ) (pp_reg r) (pp_operand a) (pp_operand b)
  | P_MOV (typ, r, a) ->
    sprintf "mov.%s %s, %s"
      (pp_op_type typ) (pp_reg r) (pp_operand a)
  | I_SETP (op, typ, r, a, b) ->
    sprintf "setp.%s.%s %s, %s, %s"
      (pp_cmp_op op) (pp_op_type typ) (pp_reg r) (pp_operand a) (pp_operand b)
  | I_LD (sem, scp, typ, d, a) ->
    sprintf "ld.%s.%s.%s %s, [%s]"
      (pp_op_sem sem) (pp_scope scp) (pp_op_type typ) (pp_reg d) (pp_reg a)
  | I_ST (sem, scp, typ, a, d) ->
    sprintf "st.%s.%s.%s [%s], %s"
      (pp_op_sem sem) (pp_scope scp) (pp_op_type typ) (pp_reg a) (pp_reg d)
  | I_CAS (sem, scp, typ, d, a, b, c) ->
    spintf "cas.%s.%s.%s %s, [%s], %s, %s"
      (pp_op_sem sem) (pp_scope scp) (pp_op_type typ)
      (pp_reg d) (pp_reg a) (pp_operand b) (pp_operand c)
  | I_ATOM (op, sem, scp, typ, d, a, b) ->
      spintf "%s.%s.%s.%s %s, [%s], %s"
        (pp_atom_op op) (pp_op_sem sem) (pp_scope scp) (pp_op_type typ)
        (pp_reg d) (pp_reg a)
  | I_FENCE f ->
    pp_barrier f

type instruction_guard =
  | Guard of reg
  | Guard_Neg of reg

let pp_instruction_guard guard = match guard with
  | Guard p -> sprintf "@%s" (pp_reg p)
  | Guard_Neg p -> sprintf "@!%s" (pp_reg p)

type instruction = instruction_guard option * instruction_base

type parsedInstruction = instruction

let pp_instruction m (guard, instr) =
  match guard with
  | None -> pp_instruction_base instr
  | Some g -> sprintf "%s %s" (pp_instruction_guard g) (pp_instruction_base instr)

let dump_instruction = pp_instruction

(****************************)
(* Symbolic registers stuff *)
(****************************)

(* TODO: support symbolic registers *)

let allowed_for_symb = []

let fold_regs (f_reg, f_sreg) =
  let fold_reg reg (y_reg, y_sreg) =
    f_reg reg y_reg, y_sreg
  in
  let fold_operand op acc = match op with
  | Operand_reg reg -> fold_reg reg acc
  | Operand_imm _ -> acc
  in
  fun acc (guard, instr) ->
    let acc = match guard with
    | None -> acc
    | Some (Guard p) -> fold_reg p acc
    | Some (Guard_Neg p) -> fold_reg p acc
    in
    match instr with
    | I_ADD (_, r, a, b) ->
      fold_reg r (fold_operand a (fold_operand b acc))
    | I_AND (_, r, a, b) ->
      fold_reg r (fold_operand a (fold_operand b acc))
    | P_MOV (typ, r, a) ->
      fold_reg r (fold_operand a acc)
    | I_SETP (_, _, r, a, b) ->
      fold_reg r (fold_operand a (fold_operand b acc))
    | I_LD (_, _, _, d, a) ->
      fold_reg d (fold_reg a acc)
    | I_ST (_, _, _, a, d) ->
      fold_reg d (fold_reg a acc)
    | I_CAS (sem, scp, typ, d, a, b, c) ->
      fold_reg d (fold_reg a (fold_operand b (fold_operand c acc)))
    | I_ATOM (op, sem, scp, typ, d, a, b) ->
      fold_reg d (fold_reg a (fold_operand b acc))
    | I_FENCE _ ->
      acc

let map_regs f_reg f_sreg =
  let map_reg reg =
    f_reg reg
  in
  let map_operand op = match op with
  | Operand_reg reg -> map_reg reg
  | Operand_imm _ -> op
  in
  fun instr ->
    let guard = match guard with
    | None -> None
    | Some (Guard p) -> Some (Guard @@ map_reg p)
    | Some (Guard_Neg p) -> Some (Guard_Neg @@ map_reg p)
    in
    let instr = match instr with
    | I_ADD (typ, r, a, b) ->
      I_ADD(typ, map_reg r, map_operand a, map_operand b)
    | I_AND (typ, r, a, b) ->
      I_AND (typ, map_reg r, map_operand a, map_operand b)
    | I_MOV (typ, r, a) ->
      I_MOV (typ, map_reg r, map_operand a)
    | I_SETP (op, typ, r, a, b) ->
      I_SETP (op, typ, map_reg r, map_operand a, map_operand b)
    | I_LD (sem, scp, typ, d, a) ->
      I_LD (sem, scp, typ, map_reg d, map_reg a)
    | I_ST (sem, scp, typ, a, d) ->
      I_ST (sem, scp, typ, map_reg a, map_reg d)
    | I_CAS (sem, scp, typ, d, a, b, c) ->
      I_CAS (sem, scp, typ, map_reg d, map_reg a, map_operand b, map_operand c)
    | I_ATOM (op, sem, scp, typ, d, a, b) ->
      I_ATOM (op, sem, scp, typ, map_reg d, map_reg a, map_operand b)
    | I_FENCE _ -> instr
    in
    (guard, instr)

(* No addresses burried in PTX code *)

let fold_addrs _f acc _instr = acc

let map_addrs _f acc instr = instr

(* No normalization (yet ?) *)

let norm_ins instr = instr

(* TODO: `bra` instruction ? *)
let get_next = function
  | _ -> [Label.next]

include Pseudo.Make
  (struct
    type ins = instruction
    type pins = parsedInstruction
    type reg_arg = reg

    let parsed_tr instr = instr

    let get_naccesses = function
    | I_LD _ | I_ST _ | I_CAS _ | I_ATOM _ -> 1
    | _ -> 0

    (* TODO: `bra` instruction *)
    let fold_labels acc _f = function
    | _ -> acc

    let map_labels _f instr = instr

  end)