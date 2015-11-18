(*---------------------------------------------------------------------------
   Copyright (c) 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* N.B. error reporting is terrible, it's not our focus here. *)

let io_buffer_size = 65535                          (* IO_BUFFER_SIZE 4.00.0 *)

type lexeme = [ `Ls | `Le | `A of string ]

(* Characters and their classes. *)

let ux_eoi = max_int                 (* End of input, outside unicode range. *)
let ux_soi = max_int - 1           (* Start of input, outside unicode range. *)
let u_lpar = 0x28                                          (* '(' character. *)
let u_rpar = 0x29                                          (* ')' character. *)
let is_white = function 0x20 | 0x09 | 0x0D | 0x0A -> true | _ -> false
let is_atom_char c = (0x21 <= c && c <= 0x27) || (0x30 <= c && c <= 0x7E)

(* Blocking codec. *)

module B = struct

  (* Decoding *)

  type src = [ `Channel of in_channel | `Bytes of bytes ]
  type decoder =
    { src : src;                                            (* Input source. *)
      mutable i : bytes;                             (* Current input chunk. *)
      mutable i_pos : int;                   (* Next input position to read. *)
      mutable i_max : int;                (* Maximal input position to read. *)
      atom : Buffer.t;                                  (* Buffer for atoms. *)
      mutable c : int;                               (* Character lookahead. *)
      mutable nest : int; }                          (* Parenthesis nesting. *)

  let decoder src =
    let i, i_pos, i_max = match src with
    | `Bytes b -> b, 0, Bytes.length b - 1
    | `Channel _ -> Bytes.create io_buffer_size, max_int, 0
    in
    { src = (src :> src); i; i_pos; i_max; atom = Buffer.create 256;
      c = ux_soi; nest = 0;}

  let eoi d =
    d.i <- Bytes.empty; d.i_pos <- max_int; d.i_max <- 0; d.c <- ux_eoi

  let refill d = match d.src with
  | `Bytes _ -> eoi d
  | `Channel ic ->
      let rc = input ic d.i 0 (Bytes.length d.i) in
      if rc = 0 then (eoi d) else (d.i_pos <- 0; d.i_max <- rc - 1;)

  let rec readc d =
    if d.i_pos > d.i_max then (if d.c = ux_eoi then () else (refill d; readc d))
    else begin
      d.c <- Char.code (Bytes.unsafe_get d.i d.i_pos);
      d.i_pos <- d.i_pos + 1;
    end

  let atom_add d = Buffer.add_char d.atom (Char.chr d.c)
  let atom d = let a = Buffer.contents d.atom in (Buffer.clear d.atom; `A a)
  let error d = d.nest <- 0 (* reset *); `Error
  let r_white d = while (is_white d.c) do readc d done
  let r_end d = if d.nest = 0 then `End else error d
  let r_ls d = d.nest <- d.nest + 1; readc d; `Lexeme `Ls
  let r_le d = d.nest <- d.nest - 1; readc d;
    if (d.nest < 0) then error d else `Lexeme `Le

  let r_atom d =
    while (is_atom_char d.c) do (atom_add d; readc d) done; `Lexeme (atom d)

  let rec r_lexeme d =
    if is_white d.c then (r_white d; r_lexeme d) else
    if is_atom_char d.c then r_atom d else
    if d.c = u_lpar then r_ls d else
    if d.c = u_rpar then r_le d else
    if d.c = ux_eoi then r_end d else
    if d.c = ux_soi then (readc d; r_lexeme d) else
    `Error

  let decode = r_lexeme

  (* Encoding *)

  type dst = [ `Channel of out_channel | `Buffer of Buffer.t ]

  type encoder =
    { dst : dst;                                      (* Output destination. *)
      mutable o : bytes;                            (* Current output chunk. *)
      mutable o_pos : int;                 (* Next output position to write. *)
      mutable o_max : int;              (* Maximal output position to write. *)
      mutable nest : int;                            (* Parenthesis nesting. *)
      mutable last_a : bool }          (* [true] if last lexeme was an atom. *)

  let encoder dst =
    let o, o_pos, o_max = match dst with `Buffer _ | `Channel _ ->
      Bytes.create io_buffer_size, 0, io_buffer_size - 1
    in
    { dst = (dst :> dst); o; o_pos; o_max; nest = 0; last_a = false }

  let flush e = match e.dst with
  | `Buffer b -> Buffer.add_subbytes b e.o 0 e.o_pos; e.o_pos <- 0
  | `Channel oc -> output oc e.o 0 e.o_pos; e.o_pos <- 0

  let rec writec e c =
    if e.o_pos > e.o_max then (flush e; writec e c) else
    (Bytes.unsafe_set e.o e.o_pos c; e.o_pos <- e.o_pos + 1)

  let rec writes e s j l =
    let rem = e.o_max - e.o_pos + 1 in
    let len = if l > rem then rem else l in
    String.unsafe_blit s j e.o e.o_pos len;
    e.o_pos <- e.o_pos + len;
    if len < l then (flush e; writes e s (j + len) (l - len))

  let invalid_seq () = invalid_arg "non well-formed sequence"
  let encode e v = match v with
  | `End -> if e.nest > 0 then invalid_seq () else flush e
  | `Lexeme l -> match l with
    | `Le when e.nest = 0 -> invalid_seq ()
    | `Le -> e.nest <- e.nest - 1; e.last_a <- false; writec e ')'
    | `Ls -> e.nest <- e.nest + 1; e.last_a <- false; writec e '('
    | `A a ->
        let al = String.length a in
        if al = 0 then invalid_arg "empty atom" else
        begin
          if e.last_a then writec e ' ';
          e.last_a <- true; writes e a 0 al
        end
end

(* Non-blocking codec *)

module Nb = struct

  (* Decoding *)

  type src = [ `Channel of Pervasives.in_channel | `Bytes of bytes | `Manual ]
  type decoder =
    { src : src;                                            (* Input source. *)
      mutable i : bytes;                             (* Current input chunk. *)
      mutable i_pos : int;                        (* Input current position. *)
      mutable i_max : int;                        (* Input maximal position. *)
      atom : Buffer.t;                                  (* Buffer for atoms. *)
      mutable c : int;                               (* Character lookahead. *)
      mutable nest : int;                            (* Parenthesis nesting. *)
      mutable k :                                   (* Decoder continuation. *)
        decoder -> [ `Lexeme of lexeme | `Await | `End | `Error ]; }

  let eoi d =
    d.i <- Bytes.empty; d.i_pos <- max_int; d.i_max <- 0; d.c <- ux_eoi

  let decode_src d b j l =
    if (j < 0 || l < 0 || j + l > Bytes.length b) then invalid_arg "bounds";
    if (l = 0) then eoi d else
    (d.i <- b; d.i_pos <- j; d.i_max <- j + l - 1;)

  let refill k d = match d.src with
  | `Manual -> d.k <- k; `Await
  | `Bytes _ -> eoi d; k d
  | `Channel ic ->
      let rc = input ic d.i 0 (Bytes.length d.i) in
      decode_src d d.i 0 rc;
      k d

  let rec readc k d =
    if d.i_pos > d.i_max then
      (if d.c = ux_eoi then k d else refill (readc k) d)
    else begin
      d.c <- Char.code (Bytes.unsafe_get d.i d.i_pos);
      d.i_pos <- d.i_pos + 1;
      k d
    end

  let atom_add d = Buffer.add_char d.atom (Char.chr d.c)
  let atom d = let a = Buffer.contents d.atom in (Buffer.clear d.atom; `A a)
  let error k d = d.nest <- 0 (* reset *); k d `Error
  let rec r_white k d = if (is_white d.c) then readc (r_white k) d else k d
  let r_end k d = if d.nest = 0 then k d `End else error k d
  let r_ls k d = d.nest <- d.nest + 1; readc (fun d -> k d (`Lexeme `Ls)) d
  let r_le k d = d.nest <- d.nest - 1; readc (fun d ->
    if (d.nest < 0) then error k d else k d (`Lexeme `Le)) d

  let rec r_atom k d =
    if (is_atom_char d.c) then (atom_add d; readc (r_atom k) d) else
    k d (`Lexeme (atom d))

  let rec r_lexeme k d =
    if is_white d.c then (r_white (r_lexeme k) d) else
    if is_atom_char d.c then r_atom k d else
    if d.c = u_lpar then r_ls k d else
    if d.c = u_rpar then r_le k d else
    if d.c = ux_eoi then r_end k d else
    if d.c = ux_soi then (readc (r_lexeme k) d) else
    error k d

  let rec ret d result = d.k <- r_lexeme ret; result
  let decoder src =
    let i, i_pos, i_max = match src with
    | `Manual -> Bytes.empty, max_int, 0
    | `Bytes b -> b, 0, Bytes.length b - 1
    | `Channel _ -> Bytes.create io_buffer_size, max_int, 0
    in
    { src = (src :> src); i; i_pos; i_max; atom = Buffer.create 256;
      c = ux_soi; nest = 0; k = r_lexeme ret }

  let decode d = d.k d

  (* Encode *)

  type dst = [ `Channel of out_channel | `Buffer of Buffer.t | `Manual ]
  type encode = [ `Await | `End | `Lexeme of lexeme]
  type encoder =
    { dst : dst;                                      (* Output destination. *)
      mutable o : bytes;                            (* Current output chunk. *)
      mutable o_pos : int;                 (* Next output position to write. *)
      mutable o_max : int;              (* Maximal output position to write. *)
      mutable nest : int;                            (* Parenthesis nesting. *)
      mutable last_a : bool;           (* [true] if last lexeme was an atom. *)
      mutable k :                                   (* Encoder continuation. *)
        encoder -> encode -> [`Partial | `Ok ] }

  let encode_dst_rem e = e.o_max - e.o_pos + 1
  let encode_dst e b j l =
    if (j < 0 || l < 0 || j + l > Bytes.length b) then invalid_arg "bounds";
    e.o <- b; e.o_pos <- j; e.o_max <- j + l - 1

  let partial k e = function `Await -> k e
  | `Lexeme _ | `End -> invalid_arg "cannot encode now, use `Await first"

  let flush k e = match e.dst with
  | `Manual -> e.k <- partial k; `Partial
  | `Buffer b -> Buffer.add_subbytes b e.o 0 e.o_pos; e.o_pos <- 0; k e
  | `Channel oc -> output oc e.o 0 e.o_pos; e.o_pos <- 0; k e

  let rec writec c k e =
    if e.o_pos > e.o_max then flush (writec c k) e else
    (Bytes.unsafe_set e.o e.o_pos c; e.o_pos <- e.o_pos + 1; k e)

  let rec writes s j l k e =
    let rem = encode_dst_rem e in
    let len = if l > rem then rem else l in
    String.unsafe_blit s j e.o e.o_pos len;
    e.o_pos <- e.o_pos + len;
    if len < l then (flush (writes s (j + len) (l - len) k) e) else k e

  let invalid_seq () = invalid_arg "non well-formed sequence"
  let _encode k e v = match v with
  | `Await -> k e
  | `End -> if e.nest > 0 then invalid_seq () else flush k e
  | `Lexeme l -> match l with
    | `Le when e.nest = 0 -> invalid_seq ()
    | `Le -> e.nest <- e.nest - 1; e.last_a <- false; writec ')' k e
    | `Ls -> e.nest <- e.nest + 1; e.last_a <- false; writec '(' k e
    | `A a ->
        let al = String.length a in
        if al = 0 then invalid_arg "empty atom" else
        begin
          let w_atom e = e.last_a <- true; writes a 0 al k e in
          if e.last_a then writec ' ' w_atom e else w_atom e
        end

  let rec ret e = e.k <- _encode ret; `Ok
  let encoder dst =
    let o, o_pos, o_max = match dst with
    | `Manual -> Bytes.empty, 1, 0
    | `Buffer _ | `Channel _ ->
        Bytes.create io_buffer_size, 0, io_buffer_size - 1
    in
    { dst = (dst :> dst); o; o_pos; o_max; nest = 0; last_a = false;
      k = _encode ret }

  let encode e v = e.k e (v :> encode)

  module Manual = struct
    let src = decode_src
    let dst = encode_dst
    let dst_rem = encode_dst_rem
  end
end

module Enb = struct

  (* Decoding *)

  type src = unit -> (bytes * int * int) option

  let src_of_channel ic =
    let buf = Bytes.create io_buffer_size in
    fun () ->
      let rc = input ic buf 0 (Bytes.length buf) in
      if rc = 0 then None else Some (buf, 0, rc)

  let src_of_bytes b =
    let len = Bytes.length b in
    if len = 0 then fun () -> None else
    let eoi = ref false in
    fun () -> if !eoi then None else (eoi := true; Some (b, 0, len))

  type decoder =
    { src : src;                                            (* Input source. *)
      mutable i : bytes;                             (* Current input chunk. *)
      mutable i_pos : int;                   (* Next input position to read. *)
      mutable i_max : int;                (* Maximal input position to read. *)
      atom : Buffer.t;                                  (* Buffer for atoms. *)
      mutable c : int;                               (* Character lookahead. *)
      mutable nest : int; }                          (* Parenthesis nesting. *)

  let decoder src =
    { src; i = Bytes.empty; i_pos = max_int; i_max = 0;
      atom = Buffer.create 256; c = ux_soi; nest = 0;}

  let eoi d =
    d.i <- Bytes.empty; d.i_pos <- max_int; d.i_max <- 0; d.c <- ux_eoi

  let refill d = match d.src () with
  | None -> eoi d
  | Some (s, pos, len) -> d.i <- s; d.i_pos <- pos; d.i_max <- pos + len - 1

  let rec readc d =
    if d.i_pos > d.i_max then (if d.c = ux_eoi then () else (refill d; readc d))
    else begin
      d.c <- Char.code (Bytes.unsafe_get d.i d.i_pos);
      d.i_pos <- d.i_pos + 1;
    end

  let atom_add d = Buffer.add_char d.atom (Char.chr d.c)
  let atom d = let a = Buffer.contents d.atom in (Buffer.clear d.atom; `A a)
  let error d = d.nest <- 0 (* reset *); `Error
  let r_white d = while (is_white d.c) do readc d done
  let r_end d = if d.nest = 0 then `End else error d
  let r_ls d = d.nest <- d.nest + 1; readc d; `Lexeme `Ls
  let r_le d = d.nest <- d.nest - 1; readc d;
    if (d.nest < 0) then error d else `Lexeme `Le

  let r_atom d =
    while (is_atom_char d.c) do (atom_add d; readc d) done; `Lexeme (atom d)

  let rec r_lexeme d =
    if is_white d.c then (r_white d; r_lexeme d) else
    if is_atom_char d.c then r_atom d else
    if d.c = u_lpar then r_ls d else
    if d.c = u_rpar then r_le d else
    if d.c = ux_eoi then r_end d else
    if d.c = ux_soi then (readc d; r_lexeme d) else
    `Error

  let decode = r_lexeme

  (* Encoding *)

  type dst = (bytes * int * int) option -> unit

  type encoder =
    { dst : dst;                                      (* Output destination. *)
      mutable o : bytes;                            (* Current output chunk. *)
      mutable o_pos : int;                 (* Next output position to write. *)
      mutable o_max : int;              (* Maximal output position to write. *)
      mutable nest : int;                            (* Parenthesis nesting. *)
      mutable last_a : bool }          (* [true] if last lexeme was an atom. *)

  let dst_of_channel oc = function
  | Some (o, pos, len) -> output oc o pos len
  | None -> ()

  let dst_of_buffer buf = function
  | Some (o, pos, len) -> Buffer.add_subbytes buf o pos len
  | None -> ()

  let encoder ?(buf = Bytes.create io_buffer_size) dst =
    let o_max = Bytes.length buf - 1 in
    if o_max = 0 then invalid_arg "buf's length is empty" else
    { dst; o = buf; o_pos = 0; o_max; nest = 0; last_a = false }

  let flush e ~stop =
    if stop
    then (if e.o_pos <> 0 then e.dst (Some (e.o, 0, e.o_pos)); e.dst None)
    else e.dst (Some (e.o, 0, e.o_pos));
    e.o_pos <- 0

  let rec writec e c =
    if e.o_pos > e.o_max then (flush e ~stop:false; writec e c) else
    (Bytes.unsafe_set e.o e.o_pos c; e.o_pos <- e.o_pos + 1)

  let rec writes e s j l =
    let rem = e.o_max - e.o_pos + 1 in
    let len = if l > rem then rem else l in
    String.unsafe_blit s j e.o e.o_pos len;
    e.o_pos <- e.o_pos + len;
    if len < l then (flush e ~stop:false; writes e s (j + len) (l - len))

  let invalid_seq () = invalid_arg "non well-formed sequence"
  let encode e v = match v with
  | `End -> if e.nest > 0 then invalid_seq () else flush e ~stop:true
  | `Lexeme l -> match l with
    | `Le when e.nest = 0 -> invalid_seq ()
    | `Le -> e.nest <- e.nest - 1; e.last_a <- false; writec e ')'
    | `Ls -> e.nest <- e.nest + 1; e.last_a <- false; writec e '('
    | `A a ->
        let al = String.length a in
        if al = 0 then invalid_arg "empty atom" else
        begin
          if e.last_a then writec e ' ';
          e.last_a <- true; writes e a 0 al
        end
end


(*---------------------------------------------------------------------------
   Copyright (c) 2012 Daniel C. Bünzli
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
