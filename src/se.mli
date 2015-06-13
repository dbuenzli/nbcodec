(*---------------------------------------------------------------------------
   Copyright (c) 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Blocking and non-blocking streaming codecs for simplified s-expressions.

    We want to encode and decode the following simple s-expression grammar
    (expressed in the language of
    {{:http://tools.ietf.org/html/rfc5234}RFC 5234}).
{[
  doc = *sexp
 sexp = white / atom / list
white =  *( %x0020 / %x0009 / %x000A / %x000D )
 list =  %x0028 *sexp %x0029
 atom = *( %x0021-0027 / %x0030-007E ) ; most ASCII printable characters
]}
    This corresponds s-expressions without quoted tokens and
    comments. We assume an ASCII encoding.

    The module {!Se.B} is a blocking streaming codec for this grammar while
    {!Se.Nb} is a non-blocking streaming one. {!Se.Enb} is a non-blocking
    streaming codec written in direct style using effects.
*)

(** {1 Data model} *)

type lexeme = [ `Ls | `Le | `A of string ]
(** The type for s-expression lexemes. Atoms, list end and list start.

    A well-formed sequence of lexemes belongs to the language
    of the [doc] grammar :
{[
doc = *sexp
sexp = `A a / `Ls *sexp `Le
]}
    The codecs deal only with well-formed sequences or
    errors are returned. They also assume that the atom strings
    are immutable.
*)

(** {1 Codecs} *)

(** Blocking streaming codec. *)
module B : sig

  (** {1 Decoding} *)

  type src = [ `Channel of in_channel | `Bytes of bytes ]
  (** The type for input sources. *)

  type decoder
  (** The type for s-expressions decoders. *)

  val decoder : [< src] -> decoder
  (** [decoder src] is a decoder that inputs from [src]. *)

  val decode : decoder -> [ `Lexeme of lexeme | `End | `Error ]
  (** [decode d] is :
      {ul
      {- [`Lexeme l], if a lexeme [l] was decoded.}
      {- [`End], if the end of input was reached.}
      {- [`Error], if an error occured. If you are interested in
         a best-effort decoding you can still continue to decode
         after an error.}}

      {b Note.} Repeated invocation always eventually returns [`End], even in
      case of errors. *)

  (** {1 Encoding} *)

  type dst = [ `Channel of out_channel | `Buffer of Buffer.t ]
  (** The type for output destinations. *)

  type encoder
  (** The type for s-expressions encoders. *)

  val encoder : [< dst] -> encoder
  (** [encoder dst] is an encoder that outputs to [dst]. *)

  val encode : encoder -> [< `Lexeme of lexeme | `End ] -> unit
  (** [encode e v] encodes [v] on [e].

      {b Raises.} [Invalid_argument] if a non well-formed sequence
      of lexemes is encoded. *)
end

(** Non-blocking streaming codec. *)
module Nb : sig

  (** {1 Decoding} *)

  type src = [ `Channel of in_channel | `Bytes of bytes | `Manual ]
  (** The type for input sources. *)

  type decoder
  (** The type for s-expressions decoders. *)

  val decoder : [< src] -> decoder
  (** [decoder src] is a decoder that inputs from src. *)

  val decode : decoder -> [ `Lexeme of lexeme | `Await | `End | `Error ]
  (** [decode d] is:
      {ul
      {- [`Await] iff [d] has a [`Manual] input source and awaits
         for more input. The client must use {!Manual.src} to provide it.}
      {- [`Lexeme l], if a lexeme [l] was decoded.}
      {- [`End], if the end of input was reached.}
      {- [`Error], if an error occured. If you are interested in
         a best-effort decoding you can still continue to decode
         after an error.}}

      {b Note.} Repeated invocation always eventually returns [`End], even in
      case of errors. *)

  (** {1 Encoding} *)

  type dst = [ `Channel of out_channel | `Buffer of Buffer.t | `Manual ]
  (** The type for output destinations. *)

  type encoder
  (** The type for s-expression encoders. *)

  val encoder : [< dst] -> encoder
  (** [encoder dst] is an encoder that outputs to [dst]. *)

  val encode : encoder -> [< `Await | `Lexeme of lexeme | `End ] ->
    [ `Ok | `Partial ]
  (** [encode e v] is :
      {ul
      {- [`Partial] iff [e] has a [`Manual] destination and needs
          more output storage. The client must use {!Manual.dst} to provide
          a new buffer and then call {!encode} with [`Await] until [`Ok]
          is returned.}
      {- [`Ok] when the encoder is ready to encode a new [`Lexeme] or [`End].}}

      For [`Manual] destinations, encoding [`End] always returns
      [`Partial], continue with [`Await] until [`Ok] is
      returned at which point [Manual.dst_rem e] is guaranteed to be
      the size of the last provided buffer (i.e. nothing was written).

      {b Raises.} [Invalid_argument] if a non well-formed sequence
      of lexemes is encoded or if a [`Lexeme] or [`End] is encoded after
      a [`Partial] encode. *)

 (** {1 Manual sources and destinations} *)

  (** Manual sources and destinations. *)
  module Manual : sig

    val src : decoder -> bytes -> int -> int -> unit
    (** [src d b k l] provides [d] with [l] bytes to read,
        starting at [k] in [b]. This byte range is read by calls to {!decode}
        with [d] until [`Await] is returned. To signal the end of input
        call the function with [l = 0].

        {b Warning.} Do not use with non-[`Manual] decoder sources. *)

    val dst : encoder -> bytes -> int -> int -> unit
    (** [dst e b k l] provides [e] with [l] bytes to write,
        starting at [k] in [b]. This byte range is written by calls
        to {!encoder} with [e] until [`Partial] is returned.
        Use {!dst_rem} to know the remaining number of non-written
        free bytes in [b].

        {b Warning.} Do not use with non-[`Manual] encoder destinations.
    *)

    val dst_rem : encoder -> int
    (** [dst_rem e] is the remaining number of non-written,
        free bytes in the last buffer provided with {!dst}. *)
  end
end

(** Non-blocking streaming codec with support for effects. *)
module Enb : sig

  (** {1 Decoding} *)

  type src = unit -> (bytes * int * int) option
  (** The type for sources.

      A source is a function called by the decoder whenever it needs
      input data. The returned byte range must be immutable until
      the next request is made on the source by the decoder. The
      decoder guarantees that the bytes will not be mutated. A source
      returns [None] on end of input. *)

  val src_of_channel : in_channel -> src
  (** [src_of_channel ic] is a decoder source from [ic]. *)

  val src_of_bytes : bytes -> src
  (** [src_of_bytes b] is a decoder source from [bytes]. *)

  type decoder
  (** The type for s-expressions decoders. *)

  val decoder : src -> decoder
  (** [decoder src] is a decoder that inputs from [src]. *)

  val decode : decoder -> [ `Lexeme of lexeme | `End | `Error ]
  (** [decode d] is :
      {ul
      {- [`Lexeme l], if a lexeme [l] was decoded.}
      {- [`End], if the end of input was reached.}
      {- [`Error], if an error occured. If you are interested in
         a best-effort decoding you can still continue to decode
         after an error.}}

      {b Note.} Repeated invocation always eventually returns [`End], even in
      case of errors. *)

  (** {1 Encoding} *)

  type dst = (bytes * int * int) option -> unit
  (** The type for destinations.

      A destination is a function called by the decoder whenever it
      needs to output data. The byte range given to the destination is
      only valid until the destination function call returns. The
      destination must guarantee that it will not mutate the
      bytes. [None] is given to a destination to mean end of output. *)

  val dst_of_channel : out_channel -> dst
  (** [dst_of_channel oc] is a destination from [oc]. *)

  val dst_of_buffer : Buffer.t -> dst
  (** [dst_of_buffer buf] is destination from [buf]. *)

  type encoder
  (** The type for s-expressions encoders. *)

  val encoder : ?buf:bytes -> dst -> encoder
  (** [encoder dst] is an encoder that outputs to [dst]. If [buf] is specified,
      [buf] is used for the internal buffer.

      @raise Invalid_argument if [buf]'s length is [0]. *)

  val encode : encoder -> [< `Lexeme of lexeme | `End ] -> unit
  (** [encode e v] encodes [v] on [e].

      {b Raises.} [Invalid_argument] if a non well-formed sequence
      of lexemes is encoded. *)
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
