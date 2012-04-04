(*---------------------------------------------------------------------------
   Copyright %%COPYRIGHT%%. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* N.B. error reporting is terrible, it's not our focus here. *)

type lexeme = [ `Ls | `Le | `A of string ]

(* Characters and their classes. *)

let ux_eoi = max_int                 (* End of input, outside unicode range. *)
let ux_await = max_int - 1            (* Await input, outside unicode range. *)
let u_lpar = 0x28                                          (* '(' character. *)
let u_rpar = 0x29                                          (* ')' character. *)
let is_white = function 0x20 | 0x09 | 0x0D | 0x0A -> true | _ -> false
let is_atom_char c = (0x21 <= c && c <= 0x27) || (0x30 <= c && c <= 0x7E)

(* Blocking (conceptually) codec. *)
 
module B = struct

  (* Decoding *)

  type decoder =
    { i : string;                                           (* Input string. *)
      mutable i_pos : int;                        (* Input current position. *)
      i_max : int;                                (* Input maximal position. *)
      atom : Buffer.t;                                  (* Buffer for atoms. *)
      mutable c : int;                               (* Character lookahead. *)
      mutable nest : int; }                          (* Parenthesis nesting. *)
      
  let decoder src = 
    { i = src; i_pos = -1; i_max = String.length src - 1; 
      atom = Buffer.create 255; c = ux_await; nest = 0; }

  let error d = d.nest <- 0 (* reset *); `Error      
  let atom_add d = Buffer.add_char d.atom (Char.chr d.c)
  let atom d = let a = Buffer.contents d.atom in (Buffer.clear d.atom; `A a)
  let readc d =
    if d.i_pos = d.i_max then d.c <- ux_eoi else 
    (d.i_pos <- d.i_pos + 1; d.c <- Char.code (String.unsafe_get d.i d.i_pos))
      
  let p_white d = while (is_white d.c) do readc d done
  let p_end d = if d.nest = 0 then `End else error d
  let p_ls d = readc d; d.nest <- d.nest + 1; `Lexeme `Ls
  let p_le d = readc d; d.nest <- d.nest - 1;
    if (d.nest < 0) then error d else `Lexeme `Le
    
  let p_atom d = 
    while (is_atom_char d.c) do (atom_add d; readc d) done; `Lexeme (atom d)

  let rec p_lexeme d =       
    if is_white d.c then (p_white d; p_lexeme d) else 
    if is_atom_char d.c then p_atom d else 
    if d.c = u_lpar then p_ls d else 
    if d.c = u_rpar then p_le d else 
    if d.c = ux_eoi then p_end d else 
    if d.c = ux_await then (readc d; p_lexeme d) else
    `Error

  let decode d = p_lexeme d

  (* Encoding *)

  type encoder = 
    { o : Buffer.t;                                        (* Output buffer. *)
      mutable nest : int;                            (* Parenthesis nesting. *)
      mutable last_a : bool }          (* [true] if last lexeme was an atom. *) 

  let encoder b = { o = b; nest = 0; last_a = false } 
  let invalid_seq () = invalid_arg "non well-formed sequence"
  let encode e v = match v with
  | `End -> if e.nest > 0 then invalid_seq () else ()
  | `Lexeme l -> match l with
    | `Le when e.nest = 0 -> invalid_seq ()
    | `Le -> e.nest <- e.nest - 1; e.last_a <- false; Buffer.add_char e.o ')'
    | `Ls -> e.nest <- e.nest + 1; e.last_a <- false; Buffer.add_char e.o '('
    | `A a -> 
        if String.length a = 0 then invalid_arg "empty atom" else
        begin 
          if e.last_a then Buffer.add_char e.o ' ';
          e.last_a <- true; 
          Buffer.add_string e.o a
        end
end

(* Non-blocking codec *)

module Nb = struct

  (* Decoding *)

  type decoder =
    { mutable i : string;                            (* Current input chunk. *)
      mutable i_min : int;                        (* Input minimal position. *)
      mutable i_pos : int;                        (* Input current position. *)
      mutable i_max : int;                        (* Input maximal position. *)
      mutable k :                                   (* Decoder continuation. *)
        decoder -> [ `Lexeme of lexeme | `Await | `End | `Error ];
      atom : Buffer.t;                                  (* Buffer for atoms. *)
      mutable c : int;                               (* Character lookahead. *)
      mutable nest : int; }                          (* Parenthesis nesting. *)

  let error k d = d.nest <- 0 (* reset *); k d `Error
  let atom_add d = Buffer.add_char d.atom (Char.chr d.c)
  let atom d = let a = Buffer.contents d.atom in (Buffer.clear d.atom; `A a)
  let readc d k =
    if d.i_max = -1 then (d.c <- ux_eoi; k d) else
    if d.i_pos = d.i_max then (d.c <- ux_await; d.k <- k; `Await) else
    begin 
      d.i_pos <- d.i_pos + 1; 
      d.c <- Char.code (String.unsafe_get d.i d.i_pos);
      k d
    end
  
  let rec p_white k d = if (is_white d.c) then readc d (p_white k) else k d
  let p_end k d = if d.nest = 0 then k d `End else error k d
  let p_ls k d = d.c <- ux_await; d.nest <- d.nest + 1; k d (`Lexeme `Ls)
  let p_le k d = d.c <- ux_await; d.nest <- d.nest - 1;
    if (d.nest < 0) then error k d else k d (`Lexeme `Le)

  let rec p_atom k d = 
    if (is_atom_char d.c) then (atom_add d; readc d (p_atom k)) else
    k d (`Lexeme (atom d))

  let rec p_lexeme k d =
    if is_white d.c then (p_white (p_lexeme k) d) else 
    if is_atom_char d.c then p_atom k d else
    if d.c = u_lpar then p_ls k d else 
    if d.c = u_rpar then p_le k d else 
    if d.c = ux_eoi then p_end k d else 
    error k d

  let rec ret d result = d.k <- p_lexeme ret; result

  let decoder () = 
    { i = ""; i_min = 0; i_max = 0; i_pos = -1; k = p_lexeme ret;
      atom = Buffer.create 255; c = ux_await; nest = 0; }

  let decode d s j l =
    d.i <- s;
    if l = 0 then (d.i_min <- 0; d.i_pos <- -1; d.i_max <- -1) else
    begin 
      d.i_min <- j; d.i_pos <- j - 1; d.i_max <- j + l - 1;
      if j < 0 || l < 0 || d.i_max > String.length s - 1 
      then invalid_arg "bounds";
    end;
    if d.c = ux_await then readc d (fun d -> d.k d) (* why ? *)  else d.k d
    
  let decoded d = (d.i_pos - d.i_min) + 1

  (* Encoding. N.B. the continuation passing style could be optimized here,
     but it's due to the simple nature of the language. *)

  type encoder = 
    { mutable o : string;                           (* Current output chunk. *)
      mutable o_start : int;                       (* Output start position. *)
      mutable o_pos : int;                       (* Output current position. *)
      mutable o_rem : int;            (* Remaining output length from o_pos. *)
      mutable k :                                   (* Encoder continuation. *)
              encoder -> [ `Await | `End | `Lexeme of lexeme ] -> 
              [ `Done | `Busy ];
      mutable nest : int;                            (* Parenthesis nesting. *)
      mutable last_a : bool; }         (* [true] if last lexeme was an atom. *) 

  let invalid_seq () = invalid_arg "non well-formed sequence"

  let done_ k e = k e `Done
  let busy k e = 
    let resume e = function 
    | `Await -> k e 
    | `End | `Lexeme _ -> invalid_arg "encoder is busy"
    in
    e.k <- resume; `Busy 

  let rec w_char c k e =
    if e.o_rem = 0 then busy (w_char c k) e else
    begin
      String.unsafe_set e.o e.o_pos c; 
      e.o_pos <- e.o_pos + 1; e.o_rem <- e.o_rem - 1; k e
    end

  let rec w_string s j l k e =
    if l > e.o_rem then
    begin
      String.unsafe_blit s j e.o e.o_pos e.o_rem; 
      e.o_pos <- e.o_pos + e.o_rem; 
      busy (w_string s (j + e.o_rem) (l - e.o_rem) k) e
    end else begin
      String.unsafe_blit s j e.o e.o_pos l; 
      e.o_pos <- e.o_pos + l; e.o_rem <- e.o_rem - l; k e
    end

  let w_atom a k e = 
    let len = String.length a in 
    if len = 0 then invalid_arg "empty atom" else 
    (e.last_a <- true; w_string a 0 len (done_ k) e)

  let _encode k e = function
  | `Await -> invalid_arg "encoder not busy"
  | `End -> if e.nest > 0 then invalid_seq () else k e `Done
  | `Lexeme l -> match l with
    | `Le when e.nest = 0 -> invalid_seq ()
    | `Le -> e.nest <- e.nest - 1; e.last_a <- false; w_char ')' (done_ k) e
    | `Ls -> e.nest <- e.nest + 1; e.last_a <- false; w_char '(' (done_ k) e
    | `A a -> if e.last_a then w_char ' ' (w_atom a k) e else w_atom a k e

  let rec ret e result = e.k <- _encode ret; result

  let encoder () = 
    { o = ""; o_start = 0; o_pos = 0; o_rem = 0; k = _encode ret; 
      nest = 0; last_a = false }

  let encode e v s j l = 
    if (j < 0 || l < 0 || j + l > String.length s) then invalid_arg "bounds"; 
    e.o <- s; e.o_start <- j; e.o_pos <- j; e.o_rem <- l; 
    e.k e v 
    
  let encoded e = (e.o_pos - e.o_start)
end

(*---------------------------------------------------------------------------
   Copyright %%COPYRIGHT%%
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:
     
   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)

