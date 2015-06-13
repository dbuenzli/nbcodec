(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

module Lwt : sig
  type 'a t
  val run : 'a t -> 'a
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end = struct
  type 'a t = 'a
  let run a = a
  let return a = a
  let bind t f = f t
end

let ( >>= ) = Lwt.bind

module Lwt_io : sig
  type input_channel
  type output_channel
  val read_into : input_channel -> bytes -> int -> int -> int Lwt.t
  val write_from_exactly : output_channel -> bytes -> int -> int -> unit Lwt.t
end = struct
  type input_channel = ()
  type output_channel = ()
  let read_into ic b pos len = failwith "TODO"
  let write_from_exactly oc b pos len = failwith "TODO"
end

effect Lwt_read : (Lwt_io.input_channel * bytes) -> (bytes * int * int) option
effect Lwt_write : (Lwt_io.output_channel * (bytes * int * int) option) -> unit

let src_of_lwt_ic (ic : Lwt_io.input_channel) : Se.Enb.src =
  let buf = Bytes.create 65535 in
  fun () -> perform (Lwt_read (ic, buf))

let dst_of_lwt_oc (oc : Lwt_io.output_channel) : Se.Enb.dst =
  fun flush -> perform (Lwt_write (oc, flush))

let lwt_decoder ic = Se.Enb.decoder (src_of_lwt_ic ic)
let lwt_decode :
    Se.Enb.decoder -> [ `End | `Error | `Lexeme of Se.lexeme ] Lwt.t
  =
  fun dec ->
    try Lwt.return (Se.Enb.decode dec) with
    | effect (Lwt_read (ic, b)) k ->
        Lwt_io.read_into ic b 0 (Bytes.length b)
        >>= function
        | 0 -> continue k None
        | rc -> continue k (Some (b, 0, rc))

let lwt_encoder ?buf oc = Se.Enb.encoder ?buf (dst_of_lwt_oc oc)
let lwt_encode :
    Se.Enb.encoder -> [< `Lexeme of Se.lexeme | `End ] -> unit Lwt.t
  =
  fun enc v ->
    try Lwt.return (Se.Enb.encode enc v) with
    | effect (Lwt_write (oc, flush)) k ->
        match flush with
        | None -> continue k ()
        | Some (buf, pos, len) ->
            Lwt_io.write_from_exactly oc buf pos len
            >>= fun () -> continue k ()

(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli.
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
