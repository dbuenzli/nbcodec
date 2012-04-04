(*---------------------------------------------------------------------------
   Copyright %%COPYRIGHT%%. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

let dec_b s =
  let rec loop d = match Se.B.decode d with 
  | `Lexeme l -> loop d 
  | `End -> `Ok
  | `Error -> `Error 
  in
  loop (Se.B.decoder s)

let dec_nb s =
  let rec loop d s i l = match Se.Nb.decode d s i l with 
  | `Lexeme _ | `Await -> loop d s (i + Se.Nb.decoded d) (l - Se.Nb.decoded d)
  | `End -> `Ok
  | `Error -> `Error
  in
  loop (Se.Nb.decoder ()) s 0 (String.length s)

let string_of_channel i = 
  let mio = 1 lsl 20 in
  let b = Buffer.create mio in 
  let s = String.create mio in 
  let rec loop b s =
    let rc = input i s 0 mio in
    if rc = 0 then Buffer.contents b else
    (Buffer.add_substring b s 0 rc; loop b s)
  in 
  loop b s

let trip bsize i o = 
  let ib = String.create bsize in 
  let ob = String.create bsize in 
  let d = Sb.Nb.decoder () in 
  let e = Sb.Nb.encoder () in 
  while (true) do 
    let rc = Unix.read i 0 l in 
    match Sb.Nb.decode 
  done;


let main () = 
  let str = Printf.sprintf in
  let log fmt = Printf.kfprintf (fun oc -> flush oc) stderr fmt in
  let exec = Filename.basename Sys.executable_name in
  let usage = 
    str "Usage: %s <options> file\n\
         non-blocking recode from stdin and to stdout.\n\
         Options:" exec 
  in
  let bsize = ref (1 lsl 20) (* 1Mo *) in 
  let out = ref true in
  let options = [
    "-bsize", Arg.Set_int blocking, "input and output buffer size in bytes.";
    "-d", Arg.Clear out, "decode only, no encoding."; ]
  in
  Arg.parse options (fun _ -> ()) usage;
  trip bsize Unix.stdin Unix.stdout

let () = main ()

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




