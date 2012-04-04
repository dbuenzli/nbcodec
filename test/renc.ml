(*---------------------------------------------------------------------------
   Copyright %%COPYRIGHT%%. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

let r_char () = Char.chr (0x0061 + Random.int (26))      (* random in [a-z]. *)
let r_atom max =
  let s = String.create (1 + Random.int max) in 
  for i = 0 to String.length s - 1 do s.[i] <- r_char () done;
  `A s

let rec r_list md ml ma acc =                          (* not t.r., too lazy *)
  let rec aux l d acc = 
    if l = 0 then `Ls :: acc else 
    if d = 0 || Random.bool () then aux (l - 1) d (r_atom ma :: acc) else 
    aux (l - 1) d (r_list (d - 1) ml ma acc)
  in
  aux (Random.int (ml + 1)) (Random.int (md + 1)) (`Le :: acc)
  
let r_se_list md ml ma = 
  let rec aux l acc = 
    if l = 0 then acc else 
    aux (l - 1) ((r_list md ml ma acc))
  in
  aux (Random.int (ml + 1) (* in [0;ml] *)) []

let enc_b lexemes = 
  let b = Buffer.create 255 in
  let rec loop e = function 
  | [] -> Se.B.encode e `End; Buffer.contents b 
  | l :: ls -> Se.B.encode e (`Lexeme l); loop e ls 
  in
  loop (Se.B.encoder b) lexemes

let enc_nb lexemes =
  let b = Buffer.create 255 in 
  let s = String.create 1 in                   (* The fixed output string. *)
  let j = ref 0 in
  let l = ref (String.length s) in 
  let contents () = Buffer.add_substring b s 0 (!j + 1); Buffer.contents b in
  let flush () = 
    if !l = 0 then (Buffer.add_string b s; j := 0; l := String.length s)
  in
  let rec encode e v =
    let r = (flush (); Se.Nb.encode e v s !j !l) in
    j := !j + Se.Nb.encoded e; l := !l - Se.Nb.encoded e; 
    match r with `Busy -> encode e `Await | `Done -> ()
  in
  let rec loop e = function 
  | [] -> encode e `End; contents ()
  | l :: ls -> encode e (`Lexeme l); loop e ls
  in
  loop (Se.Nb.encoder ()) lexemes

let main () = 
  let str = Printf.sprintf in
  let exec = Filename.basename Sys.executable_name in
  let log fmt = Printf.kfprintf (fun oc -> flush oc) stderr fmt in
  let usage = 
    str "Usage: %s <options>\n\
         Random s-expression generator.\n\
         Options:" exec 
  in
  let max_depth = ref 7 in 
  let max_list_length = ref 50 in 
  let max_atom_length = ref 5 in 
  let seed = ref (Random.self_init (); Random.int (1 lsl 30 - 1)) in   
  let blocking = ref false in
  let options = [
    "-md", Arg.Set_int max_depth,
    "maximal depth of s-expressions.";
    "-ml", Arg.Set_int max_list_length, 
    "maximal length of lists.";
    "-ma", Arg.Set_int max_atom_length,
    "maximal atom length.";
    "-s", Arg.Set_int seed,
    "random seed.";
    "-b", Arg.Set blocking, 
    "use blocking encoder." ]
  in
  Arg.parse options (fun _ -> ()) usage; 
  Random.init !seed;
  log "Generating s-expression with seed %d\n" !seed;
  let se_list = r_se_list !max_depth !max_list_length !max_atom_length in
  let enc = if !blocking then enc_b else enc_nb in
  log "Encoding with %sblocking.\n" (if !blocking then "" else "non-");
  output_string stdout (enc se_list); 
  output_char stdout '\n'
                
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




