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

/// Check variable `x` in the environment `env`*)
/// returns the `value` of the variable
let rec lookup x env = 
  match env with
  | [] -> failwith (x + "Variable not found in the environment, yo")
  | (y,v)::t -> if x=y then v else lookup  x t;;

//Evaluator's auxiliary functions
//unwrapper Boolean b -> bool
let getBool(b: value) : bool = 
  match b with
  | Boolean b -> if b = 1 then true else false
  | Int     _ -> failwith("Integer value in getBool, yo")
//unwrapper Integer i -> int
let getInt (i: value) : int =
   match i with
   | Boolean _ -> failwith("Boolean value in getInt, yo")
   | Int     i -> i
//bool -> Boolean b converter
let bool2Boolean (b:bool) : value =  if b then Boolean 1 else Boolean 0
//int ->Int i converter
let int2Int (i:int) :value = Int i
  

let getDyadic (v1 : value) (v2: value) (op: string): value =
  match op with
  | "&" ->
    bool2Boolean (getBool v1 && getBool v2)
  | "|" -> 
    bool2Boolean (getBool v2 || getBool v2) 
  | "+" ->
    int2Int (getInt v1 + getInt v2)
  | "-" ->
    int2Int (getInt v1 - getInt v2)   
  | "*" ->
    int2Int (getInt v1 * getInt v2)
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
  | Let (s,eRhs,lBody) -> 
     let xVal =  eval eRhs env
     let bodyEnv = (s,xVal) :: env
     eval lBody bodyEnv     
  | Dyadic(op, e1, e2) -> 
    let (b1, b2) = (eval e1 env,eval e2 env)
    getDyadic b1 b2 op 
  | Atom _        -> Int 0 (* not implemented *)
  | Monadic (_,e) ->   (*negation operator*)    
    if (eval e env = Boolean 1) then Boolean 0 else Boolean 1




