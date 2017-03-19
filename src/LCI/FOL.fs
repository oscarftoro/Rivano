//namespace LCI

/// A tiny language to express First Order Logic formulas
///
/// ## Example
///
///     let h = Library.hello 1
///     printfn "%d" h
///
  
module LCI.Fol

(* Abstract Syntax *)
//type Prop = CBool of bool
type expr = 
          | CInt    of int 
          | CBool   of bool       
          | CString of string
          | Var     of string
          | Let     of string * expr * expr
          | Atom    of string
          | Dyadic  of string * expr * expr
          | Monadic of string * expr

(*environment, knowledge base or context *)
type 't env = (string * 't) list
type Boolean = Boolean of bool
(* return and value types of the environment *)
type value = 
  | Int     of int
  | Boolean of int
  | String  of string               

/// Check variable `x` in the environment `env`*)
/// returns the `value` of the variable
let rec lookup x env = 
  match env with
  | [] -> failwith (x + "variable not found in the environment")
  | (y,v)::t -> if x=y then v else lookup  x t;;

let bl2i (b:bool) : value =  if b then Boolean 1 else Boolean 0

//auxiliary functions
let getBool(b: value) : bool = 
  match b with
  | Boolean b -> if b = 1 then true else false
  | Int     _ -> false
  | String  _ -> false

let getDyadic (b1 : value) (b2: value) (op: string): bool =
  match op with
  | "&" ->
    getBool b1 && getBool b2
  | "|" -> 
    getBool b2 || getBool b2
  | _   -> failwithf ("Binary operator not supported for boolean expressions")
  
  
  /// Evaluator of logical expressions
  /// Returns integers or booleans
  /// ## Parameters
  ///  - `e` - expresion
  ///  - `env` - environment or context
let rec eval (e: expr) (env: value env) : value =
  match e with
  | CInt    i    -> Int i
  | CBool   b    -> if b then Int 1 else Int 0
  | Var     x    ->
      match lookup x env with
      | Int i     -> Int i
      | Boolean b -> Boolean b
      | String  s -> String s  (*not supported yet*)
  | Let (s,eRhs,lBody) -> 
     let xVal =  eval eRhs env
     let bodyEnv = (s,xVal) :: env
     eval lBody bodyEnv     
  | Dyadic(op, e1, e2) -> 
    let (b1, b2) = (eval e1 env,eval e2 env)
    if (getDyadic b1 b2 op) then Boolean 1 else Boolean 0 
  | Atom _        -> Int 0 (* not implemented *)
  | CString _     -> Int 0 (* not sure if necessary *)
  | Monadic (_,e) ->   (*negation operator*)    
                     if (eval e env = Boolean 1) then Boolean 0 else Boolean 1




