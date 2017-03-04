module Parse

open System.IO
open System.Text
open Microsoft.FSharp.Text.Lexing
open Fol

let fromString (str : string) : Expr =
    let lexbuf = (*Lexing. insert if using old PowerPack *)LexBuffer<char>.FromString(str)
    try 
      FolPar.Main FolLex.Token lexbuf
    with 
      | exn -> let pos = lexbuf.EndPos 
               failwithf "%s near line %d, column %d\n" 
                  (exn.Message) (pos.Line+1) pos.Column       