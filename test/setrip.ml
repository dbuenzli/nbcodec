(*---------------------------------------------------------------------------
   Copyright (c) 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* IO tools  *)

let io_buffer_size = 4096                           (* IO_BUFFER_SIZE 3.12.1 *)
let unix_buffer_size = 16384                      (* UNIX_BUFFER_SIZE 3.12.1 *)

let rec unix_read fd s k l = try Unix.read fd s k l with
| Unix.Unix_error (Unix.EINTR, _, _) -> unix_read fd s k l

let rec unix_write fd s k l = try Unix.single_write fd s k l with
| Unix.Unix_error (Unix.EINTR, _, _) -> unix_write fd s k l

let rec unix_really_write fd s k l =
  let wc = unix_write fd s k l in
  if wc < l then unix_really_write fd s (k + wc) (l - wc) else ()

let string_of_channel use_unix ic =
  let b = Buffer.create unix_buffer_size in
  let input, s =
    if use_unix
    then unix_read (Unix.descr_of_in_channel ic), String.create unix_buffer_size
    else input ic, String.create io_buffer_size
  in
  let rec loop b input s =
    let rc = input s 0 (String.length s) in
    if rc = 0 then Buffer.contents b else
    (Buffer.add_substring b s 0 rc; loop b input s)
  in
  loop b input s

let string_to_channel use_unix oc s =
  if use_unix
  then unix_really_write (Unix.descr_of_out_channel oc) s 0 (String.length s)
  else output_string oc s

let dst_for sout = if sout then `Buffer (Buffer.create 255) else `Channel stdout
let src_for use_unix sin =
  if sin then `String (string_of_channel use_unix stdin) else `Channel stdin

let enb_src_for = function
| `String s -> Se.Enb.src_of_string s
| `Channel ic -> Se.Enb.src_of_channel ic

let enb_dst_for = function
| `Buffer b -> Se.Enb.dst_of_buffer b
| `Channel oc -> Se.Enb.dst_of_channel oc

(* Decode only *)

let decode_b src =
  let rec loop d = match Se.B.decode d with
  | `Lexeme l -> loop d
  | `End -> `Ok
  | `Error -> `Error
  in
  loop (Se.B.decoder src)

let decode_nb src =
  let rec loop d = match Se.Nb.decode d with
  | `Lexeme l -> loop d
  | `End -> `Ok
  | `Error -> `Error
  | `Await -> assert false
  in
  loop (Se.Nb.decoder src)

let decode_nb_unix usize fd =
  let rec loop d fd buf = match Se.Nb.decode d with
  | `Lexeme l -> loop d fd buf
  | `End -> `Ok
  | `Error -> `Error
  | `Await ->
      let rc = unix_read fd buf 0 (String.length buf) in
      Se.Nb.Manual.src d buf 0 rc; loop d fd buf
  in
  loop (Se.Nb.decoder `Manual) fd (String.create usize)

let decode_enb src =
  let rec loop d = match Se.Enb.decode d with
  | `Lexeme l -> loop d
  | `End -> `Ok
  | `Error -> `Error
  in
  loop (Se.Enb.decoder (enb_src_for src))

let decode_enb_unix usize fd =
  let rec loop d = match Se.Enb.decode d with
  | `Lexeme l -> loop d
  | `End -> `Ok
  | `Error -> `Error
  in
  let src =
    let db = String.create usize in
    fun () ->
      let rc = unix_read fd db 0 (String.length db) in
      if rc = 0 then None else Some (db, 0, rc)
  in
  loop (Se.Enb.decoder src)

let decode mode sin use_unix usize =
  let src = src_for use_unix sin in
  match mode with
  | `B -> decode_b src
  | `Nb ->
      if sin || not use_unix then decode_nb src else
      decode_nb_unix usize Unix.stdin
  | `Enb ->
      if sin || not use_unix then decode_enb src else
      decode_enb_unix usize Unix.stdin

(* Random encode only *)

let r_atom encode max =
  let r_char () = Char.chr (0x0061 + Random.int (26)) in (* random in [a-z]. *)
  let s = String.create (1 + Random.int max) in
  for i = 0 to String.length s - 1 do s.[i] <- r_char () done;
  encode (`Lexeme (`A s))

let rec r_list encode maxd maxl maxa =                (* not t.r., too lazy. *)
  let maxd = Random.int (maxd + 1) in
  encode (`Lexeme `Ls);
  r_list_els encode maxd maxl maxa;
  encode (`Lexeme `Le)

and r_list_els encode maxd maxl maxa =
  let len = Random.int (maxl + 1) in
  for i = 1 to len do
    if maxd = 0 || Random.bool () then r_atom encode maxa else
    r_list encode (maxd - 1) maxl maxa
  done

let r_doc encode maxd maxl maxa = r_list_els encode maxd maxl maxa; encode `End
let encode_b dst = Se.B.encode (Se.B.encoder dst)
let encode_nb dst =
  let e = Se.Nb.encoder dst in
  fun v -> match Se.Nb.encode e v with `Ok -> () | `Partial -> assert false

let encode_enb dst = Se.Enb.encode (Se.Enb.encoder (enb_dst_for dst))

let rec encode_unix fd e eb v = match Se.Nb.encode e v with `Ok -> ()
| `Partial ->
    unix_really_write fd eb 0 (String.length eb - Se.Nb.Manual.dst_rem e);
    Se.Nb.Manual.dst e eb 0 (String.length eb);
    encode_unix fd e eb `Await

let encode_nb_unix usize fd =
  let e = Se.Nb.encoder `Manual in
  let eb = String.create usize in
  Se.Nb.Manual.dst e eb 0 (String.length eb);
  encode_unix fd e eb

let encode_enb_unix usize fd =
  let buf = String.create usize in
  let dst = function
  | Some (o, pos, len) -> unix_really_write fd o pos len
  | None -> ()
  in
  Se.Enb.encode (Se.Enb.encoder ~buf dst)

type encode_f = [ `Lexeme of Se.lexeme | `End] -> unit

let r_encode mode sout use_unix usize rseed maxd maxl maxa =
  let dst = dst_for sout in
  let encode = match mode with
  | `B ->  encode_b dst
  | `Nb ->
      if sout || not use_unix then (encode_nb dst :> encode_f) else
      (encode_nb_unix usize Unix.stdout :> encode_f)
  | `Enb ->
      if sout || not use_unix then (encode_enb dst :> encode_f) else
      (encode_enb_unix usize Unix.stdout :> encode_f)
  in
  Random.init rseed; r_doc encode maxd maxl maxa;
  match dst with `Channel _ -> `Ok
  | `Buffer b -> string_to_channel use_unix stdout (Buffer.contents b); `Ok

(* Trip *)

let trip_b src dst =
  let rec loop d e = match Se.B.decode d with
  | `Lexeme _ as l -> Se.B.encode e l; loop d e
  | `End -> Se.B.encode e `End; `Ok
  | `Error -> `Error
  in
  loop (Se.B.decoder src) (Se.B.encoder dst)

let trip_nb src dst =
  let rec loop d e = match Se.Nb.decode d with
  | `Lexeme _ as l -> ignore (Se.Nb.encode e l); loop d e
  | `End -> ignore (Se.Nb.encode e `End); `Ok
  | `Error -> `Error
  | `Await -> assert false
  in
  loop (Se.Nb.decoder src) (Se.Nb.encoder dst)

let trip_enb src dst =
  let rec loop d e = match Se.Enb.decode d with
  | `Lexeme _ as l -> Se.Enb.encode e l; loop d e
  | `End -> Se.Enb.encode e `End; `Ok
  | `Error -> `Error
  in
  loop (Se.Enb.decoder (enb_src_for src)) (Se.Enb.encoder (enb_dst_for dst))

let trip_nb_unix usize fdi fdo =
  let rec loop fdi fdo d db e eb = match Se.Nb.decode d with
  | `Lexeme _ as v -> encode_unix fdo e eb v; loop fdi fdo d db e eb
  | `End -> encode_unix fdo e eb `End; `Ok
  | `Error -> `Error
  | `Await ->
      let rc = unix_read fdi db 0 (String.length db) in
      Se.Nb.Manual.src d db 0 rc; loop fdi fdo d db e eb
  in
  let d, db = Se.Nb.decoder `Manual, String.create usize in
  let e, eb = Se.Nb.encoder `Manual, String.create usize in
  Se.Nb.Manual.dst e eb 0 (String.length eb);
  loop fdi fdo d db e eb

let trip_enb_unix usize fdi fdo =
  let rec loop d e = match Se.Enb.decode d with
  | `Lexeme _ as l -> Se.Enb.encode e l; loop d e
  | `End -> Se.Enb.encode e `End; `Ok
  | `Error -> `Error
  in
  let db = String.create usize in
  let src =
    fun () ->
      let rc = unix_read fdi db 0 (String.length db) in
      if rc = 0 then None else Some (db, 0, rc)
  in
  let eb = String.create usize in
  let dst = function
  | None -> ()
  | Some (o, pos, len) -> unix_really_write fdo o pos len
  in
  loop (Se.Enb.decoder src) (Se.Enb.encoder ~buf:eb dst)

let trip mode sin sout use_unix usize =
  let src = src_for use_unix sin in
  let dst = dst_for sout in
  let r = match mode with
  | `B -> trip_b src dst
  | `Nb ->
      if sin || sout || not use_unix then trip_nb src dst else
      trip_nb_unix usize Unix.stdin Unix.stdout
  | `Enb ->
      if sin || sout || not use_unix then trip_enb src dst else
      trip_enb_unix usize Unix.stdin Unix.stdout
  in
  match dst with
  | `Channel _ -> r
  | `Buffer b -> string_to_channel use_unix stdout (Buffer.contents b); r

let main () =
  let str = Printf.sprintf in
  let log fmt = Printf.kfprintf (fun oc -> flush oc) stderr fmt in
  let exec = Filename.basename Sys.executable_name in
  let usage =
    str "Usage: %s <options>\n\
         recode s-expressions from stdin to stdout.\n\
         Options:" exec
  in
  let mode = ref `Nb in
  let decode_only = ref false in
  let encode_only = ref false in
  let sin = ref false in
  let sout = ref false in
  let use_unix = ref false in
  let usize = ref unix_buffer_size in
  let rseed = ref (Random.self_init (); Random.int (1 lsl 30 - 1)) in
  let maxd = ref 10 in
  let maxl = ref 50 in
  let maxa = ref 5 in
  let options = [
    "-b", Arg.Unit (fun () -> mode := `B), "use blocking codec.";
    "-enb", Arg.Unit (fun () -> mode := `Enb), "use effect non blocking codec.";
    "-dec", Arg.Set decode_only, "decode only, no encoding.";
    "-enc", Arg.Set encode_only, "(random) encode only, no decoding.";
    "-sin", Arg.Set sin, "input as string and decode the string.";
    "-sout", Arg.Set sout, "encode as string and output the string.";
    "-unix", Arg.Set use_unix, "use Unix IO (for non-blocking or sin, sout).";
    "-usize", Arg.Set_int usize,"Unix IO buffer sizes in bytes (non-blocking).";
    "-rseed", Arg.Set_int rseed, "random seed.";
    "-maxd", Arg.Set_int maxd, "maximal random depth of s-expressions.";
    "-maxl", Arg.Set_int maxl, "maximal random length of lists.";
    "-maxa", Arg.Set_int maxa, "maximal random atom length."; ]
  in
  Arg.parse options (fun _ -> raise (Arg.Bad "illegal argument")) usage;
  let r =
    if !encode_only then
      begin
        log "Random encoding with seed %d\n" !rseed;
        r_encode !mode !sout !use_unix !usize !rseed !maxd !maxl !maxa
      end
    else
    if !decode_only then decode !mode !sin !use_unix !usize else
    trip !mode !sin !sout !use_unix !usize
  in
  match r with `Error -> log "Decoding error !\n" | `Ok -> ()

let () = main ()

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
