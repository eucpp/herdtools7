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

(** Semantics of PTX instructions *)

module Make (C:Sem.Config)(V : Value.S)
    =
  struct
    module PTX = PTXArch_herd.Make(C.PC)(V)
    module Act = MachAction.Make(PTX)
    include SemExtra.Make(C)(PTX)(Act)

    (* barrier pretty print *)

    let barriers =
      List.map
        (fun bar -> { barrier = bar; pp = PTXBase.pp_barrier bar } )
        [ Fence CTA; Fence GPU; Fence SYS;
          FenceSC CTA; FenceSC GPU; FenceSC SYS
        ]

    let isync = None

    (* semantics *)

    let (>>=) = M.(>>=)
    let (>>*=) = M.(>>*=)
    let (>>|) = M.(>>|)
    let (>>!) = M.(>>!)

    let tr_type = function
    | PTX.S16 | PTX.U16 | PTX.B16 -> MachSize.Short
    | PTX.S32 | PTX.U32 | PTX.B32 -> MachSize.Word
    | PTX.S64 | PTX.U64 | PTX.B64 -> MachSize.Quad
    (* What shoud be a MachSize for predicate register ?
       PTX documentatios says it's 1-bit.
       We will use Byte for now.
     *)
    | PTX.Pred -> MachSize.Byte

    let tr_cmp_op = function
    | C_EQ -> Op.Eq
    | C_NE -> Op.Ne
    | C_LE -> Op.Le
    | C_LT -> Op.Lt
    | C_GE -> Op.Ge
    | C_GT -> Op.Gt

    let tr_atom_op = function
    | A_AND -> Op.And
    | A_OR -> Op.Or
    | A_XOR -> Op.Xor
    (*| A_EXCH -> Op.*)
    | A_ADD -> Op.Add
    (*| A_INC -> Op.*)
    (*| A_DEC*)

    let mk_annot sem scp atom typ = (sem, scp, atom, tr_type typ)

    let mk_read sem scp atom typ loc v = Act.Access (Dir.R, loc, v, mk_annot sem scp atom typ)

    let read_reg is_data typ r ii =
      M.read_loc is_data (mk_read PTX.WEAK PTX.CTA false typ) (A.Location_reg (ii.A.proc,r)) ii

    let read_reg_ord = read_reg false
    let read_reg_data = read_reg true

    let read_op typ o ii = match o with
    | Op_reg r -> read_reg_ord typ r ii
    | Op_imm i -> M.unitT (V.intToV i)

    let read_mem sem scp typ a ii =
      M.read_loc false (mk_read sem scp false typ) (A.Location_global a) ii

    let read_mem_atom sem scp typ a ii =
      let sem = match sem with
      | PTX.RLX | PTX.ACQ | PTX.ACQ_REL -> scp
      | PTX.REL -> PTX.RLX
      | WEAK -> assert false
      in
      M.read_loc false (mk_read sem scp true typ) (A.Location_global a) ii

    let mk_write sem scp atom typ loc v = Act.Access (Dir.W, loc, v, mk_annot sem scp atom typ)

    let write_loc sem scp atom typ loc v ii =
      M.mk_singleton_es (mk_write sem scp atom typ) ii

    let write_reg typ r v ii =
      write_loc PTX.WEAK PTX.CTA false typ (A.Location_reg (ii.A.proc,r)) v ii

    let write_mem sem scp typ loc v ii =
      write_loc sem scp false typ (A.Location_global loc) v ii

    let write_mem_atom sem scp typ loc v ii =
      let sem = match sem with
      | PTX.RLX | PTX.REL | PTX.ACQ_REL -> scp
      | PTX.ACQ -> PTX.RLX
      | WEAK -> assert false
      in
      write_loc sem scp true typ (A.Location_global loc) v ii

    let create_barrier b ii =
      M.mk_singleton_es (Act.Barrier b) ii

    let do_op typ op r a b =
      (read_op typ a ii >>| read_op typ b ii) >>=
      (fun (va, vb) -> M.op op va vb) >>=
      (fun v -> write_reg typ r v ii)

    let build_semantics ii =
      let (guard, instr) = ii.A.inst in
      let instr_sem = match instr with
      | PTX.I_ADD (typ, r, a, b) ->
          do_op typ OP.Add r a b >>! B.Next
      | PTX.I_AND (_, r, a, b) ->
          do_op typ OP.And r a b >>! B.Next
      | PTX.I_MOV (typ, r, a) ->
          (read_op typ a ii) >>=
          (fun v -> write_reg typ r v ii) >>! B.Next
      | PTX.I_SETP (op, _, r, a, b) ->
          (do_op typ (tr_cmp_op op) r a b) >>! B.Next
      | PTX.I_LD (sem, scp, typ, d, a) ->
          (read_reg_ord typ a ii) >>=
          (fun addr -> read_mem sem scp typ addr ii) >>=
          (fun v -> write_reg typ d v ii) >>! B.Next
      | PTX.I_ST (sem, scp, typ, a, d) ->
          ((read_reg_ord typ d ii) >>| (read_reg_data typ a ii)) >>=
          (fun (v, addr) -> write_mem sem scp typ addr v ii) >>! B.Next
      | PTX.I_CAS (sem, scp, typ, d, a, b, c) ->
          (* TODO: check that rmw_events is allowed ? *)
          (* TODO: maybe we need M.cmpexch ? *)
          read_reg_data typ a ii >>=
          (fun addr ->
            (read_op typ b ii >>| read_mem_atom sem scp typ addr ii) >>*=
            (fun (vr, v) ->
              (M.op Op.Eq v vr) >>=
              (fun eq ->
                M.choiceT eq
                  (read_op typ c ii >>=
                   fun vw -> write_mem_atom sem scp typ addr vw ii)
                  (write_mem_atom sem scp typ addr vr ii)
              ) >>=
              (write_reg typ d vr ii)
            )
          ) >>! B.Next
      | PTX.I_ATOM (op, sem, scp, typ, d, a, b) ->
          (* TODO: check that rmw_events is allowed ? *)
          let radr = read_reg_ord typ a ii in
          let rval = read_op typ b ii in
          let rmem = fun loc -> read_mem_atom sem scp typ loc ii in
          let wmem = fun loc v -> write_mem_atom sem scp typ loc v ii in
          M.amo (tr_atom_op op) radr rval rmem wmem >>=
          (fun vr -> write_reg d vr ii) >>! B.Next
      | PTX.I_FENCE f ->
          create_barrier f ii >>! B.Next
      in
      M.addT (A.next_po_index ii.A.program_order_index)
        begin match guard with
        | None -> instr_sem
        | Some (PTX.Guard p) ->
            read_reg p ii >>*= (fun v -> M.choiceT v instr_sem (M.unitT B.Next))
        | Some (PTX.Guard_Neg p) ->
            read_reg p ii >>*= (fun v -> M.choiceT v (M.unitT B.Next) instr_sem)
        end

  end