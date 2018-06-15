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

    let mk_annot sem atom typ = (sem, atom, tr_type typ)

    let mk_read sem atom typ loc v = Act.Access (Dir.R, loc, v, mk_annot sem atom typ)

    let read_reg is_data typ r ii =
      M.read_loc is_data (mk_read PTX.WEAK false typ) (A.Location_reg (ii.A.proc,r)) ii

    let read_reg_ord = read_reg false
    let read_reg_data = read_reg true

    let read_mem sem atom typ a ii =
      M.read_loc false (mk_read sem atom typ) (A.Location_global a) ii

    let read_mem_weak = read_mem PTX.WEAK false
    let read_mem_rlx = read_mem PTX.RLX false
    let read_mem_acq = read_mem PTX.ACQ false

    let mk_write sem atom typ loc v = Act.Access (Dir.W, loc, v, mk_annot sem atom typ)

    let write_loc sem atom typ loc v ii =
      M.mk_singleton_es (mk_write sem atom typ) ii

    let write_reg typ r v ii =
      write_loc PTX.WEAK false typ (A.Location_reg (ii.A.proc,r)) v ii

    let write_mem sem atom typ loc v ii =
      write_loc sem atom typ (A.Location_global loc) v ii

    let write_weak = write_mem PTX.WEAK false
    let write_rlx = write_mem PTX.RLX false
    let write_rel = write_mem PTX.REL false

    let do_op op r a b =
      (read_op a ii >>| read_op b ii) >>=
      (fun (va, vb) -> M.op op va vb) >>=
      (fun v -> write_reg r v ii)

    let build_semantics ii =
      let (guard, instr) = ii.A.inst in
      let instr_sem = match instr with
      | PTX.I_ADD (_, r, a, b) ->
          do_op OP.Add r a b >>! B.Next
      | PTX.I_AND (_, r, a, b) ->
          do_op OP.And r a b >>! B.Next
      | PTX.I_MOV (_, r, a) ->
          read_op a ii >>=
          fun v -> write_reg r v ii >>! B.Next
      | I_SETP (op, _, r, a, b) ->
          do_op (tr_cmp_op op) r a b >>! B.Next
      (*| I_LD ()*)

      M.addT (A.next_po_index ii.A.program_order_index)
        begin match guard with
        | None -> instr_sem
        | Some (PTX.Guard p) ->
            read_reg p ii >>*= (fun v -> M.choiceT v instr_sem (M.unitT B.Next))
        | Some (PTX.Guard p) ->
            read_reg p ii >>*= (fun v -> M.choiceT v (M.unitT B.Next) instr_sem)
        end

  end