// Implementation file for parser generated by fsyacc
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Text.Parsing.ParseHelpers
# 1 "/home/oscarftoro/Documentos/3.semester/Development/FSharp/ProjectScaffold/src/LCI/FolPar.fsy"

 (* File Fun/FunPar.fsy 
    Parser for a small First Order Logic Language;
    osto@itu.dk * 2016-02-06
  *)

 open LCI.AST;

# 14 "/home/oscarftoro/Documentos/3.semester/Development/FSharp/ProjectScaffold/src/LCI/FolPar.fs"
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
    | NONTERM_Prop
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
    | 2 -> NONTERM_Prop 
    | 3 -> NONTERM_Prop 
    | 4 -> NONTERM_Prop 
    | 5 -> NONTERM_Const 
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
let _fsyacc_gotos = [| 0us; 65535us; 1us; 65535us; 0us; 1us; 3us; 65535us; 0us; 2us; 7us; 5us; 8us; 6us; 3us; 65535us; 0us; 4us; 7us; 4us; 8us; 4us; |]
let _fsyacc_sparseGotoTableRowOffsets = [|0us; 1us; 3us; 7us; |]
let _fsyacc_stateToProdIdxsTableElements = [| 1us; 0us; 1us; 0us; 3us; 1us; 3us; 4us; 1us; 1us; 1us; 2us; 3us; 3us; 3us; 4us; 3us; 3us; 4us; 4us; 1us; 3us; 1us; 4us; 1us; 5us; |]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us; 2us; 4us; 8us; 10us; 12us; 16us; 20us; 22us; 24us; |]
let _fsyacc_action_rows = 10
let _fsyacc_actionTableElements = [|1us; 32768us; 9us; 9us; 0us; 49152us; 3us; 32768us; 0us; 3us; 3us; 7us; 4us; 8us; 0us; 16385us; 0us; 16386us; 0us; 16387us; 0us; 16388us; 1us; 32768us; 9us; 9us; 1us; 32768us; 9us; 9us; 0us; 16389us; |]
let _fsyacc_actionTableRowOffsets = [|0us; 2us; 3us; 7us; 8us; 9us; 10us; 11us; 13us; 15us; |]
let _fsyacc_reductionSymbolCounts = [|1us; 2us; 1us; 3us; 3us; 1us; |]
let _fsyacc_productionToNonTerminalTable = [|0us; 1us; 2us; 2us; 2us; 3us; |]
let _fsyacc_immediateActions = [|65535us; 49152us; 65535us; 16385us; 16386us; 65535us; 65535us; 65535us; 65535us; 16389us; |]
let _fsyacc_reductions ()  =    [| 
# 137 "/home/oscarftoro/Documentos/3.semester/Development/FSharp/ProjectScaffold/src/LCI/FolPar.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : AST.prop)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (Microsoft.FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startMain));
# 146 "/home/oscarftoro/Documentos/3.semester/Development/FSharp/ProjectScaffold/src/LCI/FolPar.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : AST.prop)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 29 "/home/oscarftoro/Documentos/3.semester/Development/FSharp/ProjectScaffold/src/LCI/FolPar.fsy"
                                                               _1 
                   )
# 29 "/home/oscarftoro/Documentos/3.semester/Development/FSharp/ProjectScaffold/src/LCI/FolPar.fsy"
                 : AST.prop));
# 157 "/home/oscarftoro/Documentos/3.semester/Development/FSharp/ProjectScaffold/src/LCI/FolPar.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'Const)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 33 "/home/oscarftoro/Documentos/3.semester/Development/FSharp/ProjectScaffold/src/LCI/FolPar.fsy"
                                                               _1                   
                   )
# 33 "/home/oscarftoro/Documentos/3.semester/Development/FSharp/ProjectScaffold/src/LCI/FolPar.fsy"
                 : AST.prop));
# 168 "/home/oscarftoro/Documentos/3.semester/Development/FSharp/ProjectScaffold/src/LCI/FolPar.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : AST.prop)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : AST.prop)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 34 "/home/oscarftoro/Documentos/3.semester/Development/FSharp/ProjectScaffold/src/LCI/FolPar.fsy"
                                                               Disj("or",  _1, _3)  
                   )
# 34 "/home/oscarftoro/Documentos/3.semester/Development/FSharp/ProjectScaffold/src/LCI/FolPar.fsy"
                 : AST.prop));
# 180 "/home/oscarftoro/Documentos/3.semester/Development/FSharp/ProjectScaffold/src/LCI/FolPar.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : AST.prop)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : AST.prop)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 35 "/home/oscarftoro/Documentos/3.semester/Development/FSharp/ProjectScaffold/src/LCI/FolPar.fsy"
                                                               Conj("and", _1, _3)  
                   )
# 35 "/home/oscarftoro/Documentos/3.semester/Development/FSharp/ProjectScaffold/src/LCI/FolPar.fsy"
                 : AST.prop));
# 192 "/home/oscarftoro/Documentos/3.semester/Development/FSharp/ProjectScaffold/src/LCI/FolPar.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 39 "/home/oscarftoro/Documentos/3.semester/Development/FSharp/ProjectScaffold/src/LCI/FolPar.fsy"
                                                               Atom(_1)             
                   )
# 39 "/home/oscarftoro/Documentos/3.semester/Development/FSharp/ProjectScaffold/src/LCI/FolPar.fsy"
                 : 'Const));
|]
# 204 "/home/oscarftoro/Documentos/3.semester/Development/FSharp/ProjectScaffold/src/LCI/FolPar.fs"
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
let Main lexer lexbuf : AST.prop =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 0))
