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
    {!Se.Nb} is a non-blocking streaming one.
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

(** Blocking streaming co/dec (push/pull). *)
module B : sig

  (** {1 Decoding} *)

  type decoder
  (** The type for s-expressions decoders. *)

  type src = [ `Channel of in_channel | `String of string ]
  (** The type for input sources. *)

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

  type encoder
  (** The type for s-expressions encoders. *)

  type dst = [ `Channel of out_channel | `Buffer of Buffer.t ]
  (** The type for output sources. *)
  
  val encoder : dst -> encoder 
  (** [encoder b] is an encoder that output to [b]. *)

  val encode : encoder -> [ `Lexeme of lexeme | `End ] -> unit
  (** [encode e v] encodes [v] on [e].

      {b Raises.} [Invalid_argument] if a non well-formed sequence
      of lexemes is encoded. *)
end

(** Non-blocking streaming co/dec (pull/push). *)
module Nb : sig

  (** {1 Decoding} *)

  type decoder
  (** The type for s-expressions decoders. *)
  
  val decoder : unit -> decoder 
  (** [decoder ()] is a decoder. *)

  val decode : decoder -> string -> int -> int -> 
    [ `Lexeme of lexeme | `Await | `End | `Error ]
  (** [decode d s k l] decodes with [d] by reading [l] bytes from [s] starting
      at [k]. It returns :
      {ul
      {- [`Lexeme l], if a lexeme [l] was decoded.}
      {- [`Await] if the decoding process needs more input.}
      {- [`End], if the end of input was reached.}
      {- [`Error], if an error occured. If you are interested in 
         a best-effort decoding you can still continue to decode
         after an error.}}
      Use {!decoded} to get the number of bytes that were actually 
      read. End of input must be signaled by calling {!decode} with [l = 0], 
      until [`End] is returned. 

      {b Note.} Repeated invocation always eventually returns [`End], even in 
      case of errors. *)

  val decoded : decoder -> int 
  (** [decoded d] is the number of bytes read by the last call to {!decoded}. *)

  (** {1 Encoding} *)

  type encoder 
  (** The type for s-expression encoders. *)

  val encoder : unit -> encoder
  (** [encoder ()] is an encoder for s-expressions. *)
  
  val encode : encoder -> [ `Lexeme of lexeme | `End | `Await] -> 
    string -> int -> int -> [ `Busy | `Done ]
  (** [encode e v s k l] encodes [v] with [e] by writing at most 
      [l] bytes on [s] starting at [k]. It returns: 
      {ul
      {- [`Busy], if [l] was too short to write all the data for [v]. In 
         that case [encode] must be called again with [`Await] until [`Done]
         is returned.}
      {- [`Done] if the last given element was sucessfully encoded and the 
         encoder is ready to get the next element.}}

      Use {!encoded} to get the number of bytes that were actually written.
      The end of the encoding process must be signalled by the client
      by calling [encode] with [`End] and possible subsequent [`Await]s
      until [`Done] is returned. 

      {b Raises.} [Invalid_argument] if a non well-formed sequence
      of lexemes is encoded or if a [`Lexeme] or [`End] is given
      while the encoder is [`Busy]. *)

  val encoded : encoder -> int
  (** [encoded d] is the number of bytes written by the last call to 
      {!encode}. *)
end
