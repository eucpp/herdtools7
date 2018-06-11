(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2014-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

{
module Make(O:LexUtils.Config) = struct
open Lexing
open LexMisc
open PTXParser
module PTX = PTXBase
module LU = LexUtils.Make(O)
}

let digit = [ '0'-'9' ]
let alpha = [ 'a'-'z' 'A'-'Z']
let name  = alpha (alpha|digit|'_' | '/' | '.' | '-')*
let num = digit+

rule token = parse
| [' ''\t''\r'] { token lexbuf }
| '\n' { incr_lineno lexbuf; token lexbuf }
| "(*" { LU.skip_comment lexbuf ; token lexbuf }
| '-' ? num as x { NUM (int_of_string x) }
| ';' { SEMI }
| ',' { COMMA }
| "." { DOT }
| '|' { PIPE }
| '[' { LBRK }
| ']' { RBRK }
| ':' { COLON }
| '@' { AMPERSAT }
| '!' { BANG }
(* Instructions *)
| "add" { I_ADD }
| "inc" { I_INC }
| "dec" { I_DEC }
| "and" { I_AND }
| "or" { I_OR }
| "xor" { I_XOR }
| "mov" { I_MOV }
| "setp" { I_SETP }
| "ld" { I_LD }
| "st" { I_ST }
| "cas" { I_CAS }
| "exch" { I_EXCH }
| "fence" { I_FENCE }
(* Scopes *)
| "cta" -> { PTX.CTA }
| "gpu" -> { PTX.GPU }
| "sys" -> { PTX.SYS }
(* Semantics modifiers *)
| "relaxed" -> { PTX.RLX }
| "release" -> { PTX.REL }
| "acquire" -> { PTX.ACQ }
| "acq_rel" -> { PTX.ACQ_REL }
| "sc"      -> { PTX.SC }
(* Comparison operators *)
| "eq" -> { PTX.C_EQ }
| "ne" -> { PTX.C_NE }
| "le" -> { PTX.C_LE }
| "lt" -> { PTX.C_LT }
| "ge" -> { PTX.C_GE }
| "gt" -> { PTX.C_GT }
(* Type modifiers *)
| "s16" { PTX.S16 }
| "s32" { PTX.S32 }
| "s64" { PTX.S64 }
| "u16" { PTX.U16 }
| "u32" { PTX.U32 }
| "u64" { PTX.U64 }
| "b16" { PTX.B16 }
| "b32" { PTX.B32 }
| "b64" { PTX.B64 }
| "pred" { PTX.PRED }
(* other *)
| name as x
  { match PTX.parse_reg x with
  | Some r -> ARCH_REG r
  | None -> NAME x }
| eof { EOF }
| ""  { error "PTX lexer" lexbuf }

{
let token lexbuf =
   let tok = token lexbuf in
   if O.debug then begin
     Printf.eprintf
       "%a: Lexed '%s'\n"
       Pos.pp_pos2
       (lexeme_start_p lexbuf,lexeme_end_p lexbuf)
       (lexeme lexbuf)
   end ;
   tok
end
}