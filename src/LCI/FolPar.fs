// Implementation file for parser generated by fsyacc
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Text.Parsing.ParseHelpers
# 1 "/home/oscarftoro/Documentos/3.semester/Development/FSharp/Rivano/src/LCI/FolPar.fsy"

 (* File Fun/FunPar.fsy 
    Parser for a small First Order Logic Language;
    osto@itu.dk * 2016-02-06
  *)

 open Fol;

# 14 "/home/oscarftoro/Documentos/3.semester/Development/FSharp/Rivano/src/LCI/FolPar.fs"
// This type is the type of tokens accepted by the parser
type token = 
  | EOF
  | LPAR
  | RPAR
  | OR
  | AND
  | FALSE
  | NOT
  | TRUE
  | CSTBOOL of (bool)
  | CSTATOM of (string)
  | CSTINT of (int)
// This type is used to give symbolic names to token indexes, useful for error messages
type tokenId = 
    | TOKEN_EOF
    | TOKEN_LPAR
    | TOKEN_RPAR
    | TOKEN_OR
    | TOKEN_AND
    | TOKEN_FALSE
    | TOKEN_NOT
    | TOKEN_TRUE
    | TOKEN_CSTBOOL
    | TOKEN_CSTATOM
    | TOKEN_CSTINT
    | TOKEN_end_of_input
    | TOKEN_error
// This type is used to give symbolic names to token indexes, useful for error messages
type nonTerminalId = 
    | NONTERM__startMain
    | NONTERM_Main
    | NONTERM_Expr
    | NONTERM_AtExpr
    | NONTERM_Const

// This function maps tokens to integer indexes
let tagOfToken (t:token) = 
  match t with
  | EOF  -> 0 
  | LPAR  -> 1 
  | RPAR  -> 2 
  | OR  -> 3 
  | AND  -> 4 
  | FALSE  -> 5 
  | NOT  -> 6 
  | TRUE  -> 7 
  | CSTBOOL _ -> 8 
  | CSTATOM _ -> 9 
  | CSTINT _ -> 10 

// This function maps integer indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_EOF 
  | 1 -> TOKEN_LPAR 
  | 2 -> TOKEN_RPAR 
  | 3 -> TOKEN_OR 
  | 4 -> TOKEN_AND 
  | 5 -> TOKEN_FALSE 
  | 6 -> TOKEN_NOT 
  | 7 -> TOKEN_TRUE 
  | 8 -> TOKEN_CSTBOOL 
  | 9 -> TOKEN_CSTATOM 
  | 10 -> TOKEN_CSTINT 
  | 13 -> TOKEN_end_of_input
  | 11 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__startMain 
    | 1 -> NONTERM_Main 
    | 2 -> NONTERM_Expr 
    | 3 -> NONTERM_Expr 
    | 4 -> NONTERM_Expr 
    | 5 -> NONTERM_AtExpr 
    | 6 -> NONTERM_AtExpr 
    | 7 -> NONTERM_Const 
    | 8 -> NONTERM_Const 
    | 9 -> NONTERM_Const 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 13 
let _fsyacc_tagOfErrorTerminal = 11

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | EOF  -> "EOF" 
  | LPAR  -> "LPAR" 
  | RPAR  -> "RPAR" 
  | OR  -> "OR" 
  | AND  -> "AND" 
  | FALSE  -> "FALSE" 
  | NOT  -> "NOT" 
  | TRUE  -> "TRUE" 
  | CSTBOOL _ -> "CSTBOOL" 
  | CSTATOM _ -> "CSTATOM" 
  | CSTINT _ -> "CSTINT" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | EOF  -> (null : System.Object) 
  | LPAR  -> (null : System.Object) 
  | RPAR  -> (null : System.Object) 
  | OR  -> (null : System.Object) 
  | AND  -> (null : System.Object) 
  | FALSE  -> (null : System.Object) 
  | NOT  -> (null : System.Object) 
  | TRUE  -> (null : System.Object) 
  | CSTBOOL _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | CSTATOM _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | CSTINT _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
let _fsyacc_gotos = [| 0us; 65535us; 1us; 65535us; 0us; 1us; 4us; 65535us; 0us; 2us; 8us; 5us; 9us; 6us; 11us; 7us; 4us; 65535us; 0us; 4us; 8us; 4us; 9us; 4us; 11us; 4us; 4us; 65535us; 0us; 10us; 8us; 10us; 9us; 10us; 11us; 10us; |]
let _fsyacc_sparseGotoTableRowOffsets = [|0us; 1us; 3us; 8us; 13us; |]
let _fsyacc_stateToProdIdxsTableElements = [| 1us; 0us; 1us; 0us; 3us; 1us; 3us; 4us; 1us; 1us; 1us; 2us; 3us; 3us; 3us; 4us; 3us; 3us; 4us; 4us; 3us; 3us; 4us; 6us; 1us; 3us; 1us; 4us; 1us; 5us; 1us; 6us; 1us; 6us; 1us; 7us; 1us; 8us; 1us; 9us; |]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us; 2us; 4us; 8us; 10us; 12us; 16us; 20us; 24us; 26us; 28us; 30us; 32us; 34us; 36us; 38us; |]
let _fsyacc_action_rows = 16
let _fsyacc_actionTableElements = [|4us; 32768us; 1us; 11us; 8us; 15us; 9us; 13us; 10us; 14us; 0us; 49152us; 3us; 32768us; 0us; 3us; 3us; 8us; 4us; 9us; 0us; 16385us; 0us; 16386us; 0us; 16387us; 0us; 16388us; 3us; 32768us; 2us; 12us; 3us; 8us; 4us; 9us; 4us; 32768us; 1us; 11us; 8us; 15us; 9us; 13us; 10us; 14us; 4us; 32768us; 1us; 11us; 8us; 15us; 9us; 13us; 10us; 14us; 0us; 16389us; 4us; 32768us; 1us; 11us; 8us; 15us; 9us; 13us; 10us; 14us; 0us; 16390us; 0us; 16391us; 0us; 16392us; 0us; 16393us; |]
let _fsyacc_actionTableRowOffsets = [|0us; 5us; 6us; 10us; 11us; 12us; 13us; 14us; 18us; 23us; 28us; 29us; 34us; 35us; 36us; 37us; |]
let _fsyacc_reductionSymbolCounts = [|1us; 2us; 1us; 3us; 3us; 1us; 3us; 1us; 1us; 1us; |]
let _fsyacc_productionToNonTerminalTable = [|0us; 1us; 2us; 2us; 2us; 3us; 3us; 4us; 4us; 4us; |]
let _fsyacc_immediateActions = [|65535us; 49152us; 65535us; 16385us; 16386us; 65535us; 65535us; 65535us; 65535us; 65535us; 16389us; 65535us; 16390us; 16391us; 16392us; 16393us; |]
let _fsyacc_reductions ()  =    [| 
# 142 "/home/oscarftoro/Documentos/3.semester/Development/FSharp/Rivano/src/LCI/FolPar.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Fol.Expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (Microsoft.FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startMain));
# 151 "/home/oscarftoro/Documentos/3.semester/Development/FSharp/Rivano/src/LCI/FolPar.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 28 "/home/oscarftoro/Documentos/3.semester/Development/FSharp/Rivano/src/LCI/FolPar.fsy"
                                                               _1                   
                   )
# 28 "/home/oscarftoro/Documentos/3.semester/Development/FSharp/Rivano/src/LCI/FolPar.fsy"
                 : Fol.Expr));
# 162 "/home/oscarftoro/Documentos/3.semester/Development/FSharp/Rivano/src/LCI/FolPar.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'AtExpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 32 "/home/oscarftoro/Documentos/3.semester/Development/FSharp/Rivano/src/LCI/FolPar.fsy"
                                                              _1                    
                   )
# 32 "/home/oscarftoro/Documentos/3.semester/Development/FSharp/Rivano/src/LCI/FolPar.fsy"
                 : 'Expr));
# 173 "/home/oscarftoro/Documentos/3.semester/Development/FSharp/Rivano/src/LCI/FolPar.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 33 "/home/oscarftoro/Documentos/3.semester/Development/FSharp/Rivano/src/LCI/FolPar.fsy"
                                                               Dyadic("|",_1, _3)   
                   )
# 33 "/home/oscarftoro/Documentos/3.semester/Development/FSharp/Rivano/src/LCI/FolPar.fsy"
                 : 'Expr));
# 185 "/home/oscarftoro/Documentos/3.semester/Development/FSharp/Rivano/src/LCI/FolPar.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 34 "/home/oscarftoro/Documentos/3.semester/Development/FSharp/Rivano/src/LCI/FolPar.fsy"
                                                               Dyadic("&",_1, _3)   
                   )
# 34 "/home/oscarftoro/Documentos/3.semester/Development/FSharp/Rivano/src/LCI/FolPar.fsy"
                 : 'Expr));
# 197 "/home/oscarftoro/Documentos/3.semester/Development/FSharp/Rivano/src/LCI/FolPar.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'Const)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 38 "/home/oscarftoro/Documentos/3.semester/Development/FSharp/Rivano/src/LCI/FolPar.fsy"
                                                              _1                    
                   )
# 38 "/home/oscarftoro/Documentos/3.semester/Development/FSharp/Rivano/src/LCI/FolPar.fsy"
                 : 'AtExpr));
# 208 "/home/oscarftoro/Documentos/3.semester/Development/FSharp/Rivano/src/LCI/FolPar.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 39 "/home/oscarftoro/Documentos/3.semester/Development/FSharp/Rivano/src/LCI/FolPar.fsy"
                                                              _2                    
                   )
# 39 "/home/oscarftoro/Documentos/3.semester/Development/FSharp/Rivano/src/LCI/FolPar.fsy"
                 : 'AtExpr));
# 219 "/home/oscarftoro/Documentos/3.semester/Development/FSharp/Rivano/src/LCI/FolPar.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 42 "/home/oscarftoro/Documentos/3.semester/Development/FSharp/Rivano/src/LCI/FolPar.fsy"
                                                               Atom(_1)             
                   )
# 42 "/home/oscarftoro/Documentos/3.semester/Development/FSharp/Rivano/src/LCI/FolPar.fsy"
                 : 'Const));
# 230 "/home/oscarftoro/Documentos/3.semester/Development/FSharp/Rivano/src/LCI/FolPar.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : int)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 43 "/home/oscarftoro/Documentos/3.semester/Development/FSharp/Rivano/src/LCI/FolPar.fsy"
                                                               CInt(_1)             
                   )
# 43 "/home/oscarftoro/Documentos/3.semester/Development/FSharp/Rivano/src/LCI/FolPar.fsy"
                 : 'Const));
# 241 "/home/oscarftoro/Documentos/3.semester/Development/FSharp/Rivano/src/LCI/FolPar.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : bool)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 44 "/home/oscarftoro/Documentos/3.semester/Development/FSharp/Rivano/src/LCI/FolPar.fsy"
                                                               CBool(_1)            
                   )
# 44 "/home/oscarftoro/Documentos/3.semester/Development/FSharp/Rivano/src/LCI/FolPar.fsy"
                 : 'Const));
|]
# 253 "/home/oscarftoro/Documentos/3.semester/Development/FSharp/Rivano/src/LCI/FolPar.fs"
let tables () : Microsoft.FSharp.Text.Parsing.Tables<_> = 
  { reductions= _fsyacc_reductions ();
    endOfInputTag = _fsyacc_endOfInputTag;
    tagOfToken = tagOfToken;
    dataOfToken = _fsyacc_dataOfToken; 
    actionTableElements = _fsyacc_actionTableElements;
    actionTableRowOffsets = _fsyacc_actionTableRowOffsets;
    stateToProdIdxsTableElements = _fsyacc_stateToProdIdxsTableElements;
    stateToProdIdxsTableRowOffsets = _fsyacc_stateToProdIdxsTableRowOffsets;
    reductionSymbolCounts = _fsyacc_reductionSymbolCounts;
    immediateActions = _fsyacc_immediateActions;
    gotos = _fsyacc_gotos;
    sparseGotoTableRowOffsets = _fsyacc_sparseGotoTableRowOffsets;
    tagOfErrorTerminal = _fsyacc_tagOfErrorTerminal;
    parseError = (fun (ctxt:Microsoft.FSharp.Text.Parsing.ParseErrorContext<_>) -> 
                              match parse_error_rich with 
                              | Some f -> f ctxt
                              | None -> parse_error ctxt.Message);
    numTerminals = 14;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = (tables ()).Interpret(lexer, lexbuf, startState)
let Main lexer lexbuf : Fol.Expr =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 0))
