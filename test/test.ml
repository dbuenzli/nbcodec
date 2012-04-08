(*---------------------------------------------------------------------------
   Copyright %%COPYRIGHT%%. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

let lexemes_b i = 
  let rec loop acc d = match Se.B.decode d with 
  | `Lexeme s -> loop (s :: acc) d
  | `End -> `Lexemes (List.rev acc)
  | `Error -> `Error 
  in
  loop [] (Se.B.decoder (`String i))
    
let lexemes_nb blen i =
  let rec loop acc d i k blen ilen = match Se.Nb.decode d with
  | `Lexeme s -> loop (s :: acc) d i k blen ilen
  | `Error -> `Error
  | `End -> `Lexemes (List.rev acc)
  | `Await ->
      let blen' = if k + blen > ilen then ilen - k else blen in
      Se.Nb.decode_src d i k blen'; loop acc d i (k + blen') blen' ilen
  in
  loop [] (Se.Nb.decoder `Manual) i 0 blen (String.length i)

let decode_test lexemes =
  let ss src l = assert (lexemes src = `Lexemes l) in
  let err src = assert (lexemes src = `Error) in 
  ss "  " []; 
  ss " ( ) " [ `Ls ; `Le]; 
  ss "h(e(hey))" [ `A "h"; `Ls ; `A "e"; `Ls; `A "hey"; `Le; `Le ];
  ss "()(hey (ho dip))    "
    [ `Ls; `Le; `Ls; `A "hey"; `Ls; `A "ho"; `A "dip"; `Le; `Le ];
  err "  (hey ho dip))"

let () =
  if !Sys.interactive then () else
  begin 
    print_endline "Testing blocking decoder.";
    decode_test lexemes_b; 
    print_endline "Testing non-blocking decoder.";
    decode_test (lexemes_nb 1);
    print_endline "Testing non-blocking decoder.";
    decode_test (lexemes_nb 2);
    print_endline "Testing non-blocking decoder.";
    decode_test (lexemes_nb 3);
    print_endline "Testing non-blocking decoder.";
    decode_test (lexemes_nb 20);
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


