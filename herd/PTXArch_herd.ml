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

(** Define PTX architecture *)

module Make (C:Arch_herd.Config)(V:Value.S) =
  struct
    include PTXBase

    let is_amo (guard, instr) = match instr with
    | I_CAS _ | I_ATOM _ -> true
    | _ -> false

    let pp_barrier_short = pp_barrier

    let reject_mixed = true

    (* annotation is a tuple of:
        operation semantics (i.g. release/acquire);
        operation scope (i.g. work-group/device);
        atomicity flag;
        size of operands
    *)
    type lannot = op_sem * scope * bool * MachSize.sz

    let get_machsize (_, _, _, sz) = sz

    let empty_annot = (WEAK, CTA, false, MachSize.Quad)

    let is_atomic (_, _, atom, _) -> atom

    let is_acquire (sem, _, _, _) = match sem with
    | ACQ -> true
    | _ -> false

    let is_release (sem, _, _, _) = match sem with
    | REL -> true
    | _ -> false

    let is_acq_rel (sem, _, _, _) = match sem with
    | ACQ_REL -> true
    | _ -> false

    let is_cta_scoped (_, scp, _, _) = match scp with
    | CTA -> true
    | _ -> false

    let is_gpu_scoped (_, scp, _, _) = match scp with
    | GPU -> true
    | _ -> false

    let is_sys_scoped (_, scp, _, _) = match scp with
    | SYS -> true
    | _ -> false

    let annot_sets =
      [ "X", is_atomic;
        "Acq", is_acquire;
        "Rel", is_release;
        "AcqRel", is_acq_rel;
        "cta", is_cta_scoped;
        "gpu", is_gpu_scoped;
        "sys", is_sys_scoped;
      ]

    let pp_annot (sem, scp, a, _) =
      let ssem = match sem with
      | WEAK -> ""
      | RLX -> "Rlx"
      | ACQ -> "Acq"
      | Rel -> "Rel"
      | AcqRel -> "AcqRel"
      in
      let sscp = match scp with
      | CTA -> ""
      | GPU -> "gpu"
      | SYS -> "sys"
      in
      let s = if sscp = "" then ssem else sprintf "%s[%s]" ssem sscp in
      if a then s ^ "*" else s

    let barrier_sets =
      List.map
        (fun bar -> pp_barrier bar, barrier_compare bar)
          [ Fence CTA; Fence GPU; Fence SYS;
            FenceSC CTA; FenceSC GPU; FenceSC SYS
          ]

    let is_isync _ = false

    let pp_isync = "???"

    module V = V

    include ArchExtra_herd.Make(C)(
      struct
        type arch_reg = reg

        let pp_reg = pp_reg
        let reg_compare = reg_compare

        type arch_instruction = instruction

        let fromto_of_instr _ = None

      end
    )

  end