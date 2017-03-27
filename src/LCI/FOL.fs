//namespace LCI

/// A tiny language to express First Order Logic formulas
///
/// ## Example
///
///     let h = Library.hello 1
///     printfn "%d" h
///  
module LCI.Fol

(*environment, knowledge base or context *)
type 't env = (string * 't) list
type Boolean = Boolean of bool
     
(* Abstract Syntax *)
//type Prop = CBool of bool
type expr = 
          | CInt    of int 
          | CBool   of bool  
          | CString of string  
          | CList   of expr list         
          | Var     of string
          | Let     of string * expr * expr
          | ForAll  of string * string * expr
          | ForAllL of string * expr list * expr    
          | Exists  of string * expr
          | Atom    of expr
          | Dyadic  of string * expr * expr
          | Monadic of string * expr


(* runtime values *) 
type Value = 
  | Int     of int
  | Boolean of int   
  | VList   of expr list            
 
/// Check variable `x` in the environment `env`*)
/// returns the `value` of the variable
let rec lookup x env : Value = 
  match env with
  | [] -> failwith (x + "Variable not found in the environment, yo")
  | (y,v)::t -> if x=y then v else lookup  x t;;

let rec lookupL x (env : expr env) : expr list =
  match env with
  | []      -> failwith (x + "Variable not found in the environment, yo")
  | (_,(CBool _)):: _ -> failwith("Boolean value in value env of lists") 
  | (_,(CInt _)):: _  -> failwith("Int value in value env of lists")
  | (y,(CList v)):: t -> if x=y then v else lookupL  x t;;

//Evaluator's auxiliary functions
//unwrapper Boolean b -> bool
let getBool(b: Value) : bool = 
  match b with
  | Boolean b -> b = 1
  | Int     _ -> failwith("Integer value in getBool, yo")
  
//unwrapper Integer i -> int
let getInt (i: Value) : int =
   match i with
   | Boolean _ -> failwith("Boolean value in getInt, yo")
   | Int     i -> i

//bool -> Boolean b converter
let bool2Boolean (b:bool) : Value =  if b then Boolean 1 else Boolean 0
//int ->Int i converter
let int2Int (i:int) :Value = Int i
  
let getDyadic (v1 : Value) (v2: Value) (op: string): Value =
  match op with
  | "∧" ->
    bool2Boolean (getBool v1 && getBool v2)
  | "∨" -> 
    bool2Boolean (getBool v1 || getBool v2) 
  | "→" ->
    bool2Boolean (not (getBool v1) || getBool v2)
  | "=" ->
    bool2Boolean (v1 = v2)    
  | ">" ->
    bool2Boolean (v1 > v2)  
  | "<" ->
    bool2Boolean (v1 < v2)  
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
let rec eval (e: expr) (env: Value env) : Value =
  match e with  
  | CInt    i    -> Int i
  | CBool   b    -> if b then Boolean 1 else Boolean 0
  | CList   l    -> VList l
  | Var     x    ->
      match lookup x env with
      | Int i     -> Int i
      | Boolean b -> Boolean b
      | VList v   -> VList v     
  | Let (s,eRhs,lBody) -> 
     let xVal =  eval eRhs env
     let bodyEnv = (s,xVal) :: env
     eval lBody bodyEnv     
  | ForAll(x,s,eBody) ->   
    let l1 = match lookup s env with
             | VList l -> l
             | Boolean _ -> failwith ("Boolean when list expected error in ForAll,yo")
             | Int  _    -> failwith ("Int when list expected in forAll, yo")

    env |>  eval (ForAllL (x, l1, eBody))
  | ForAllL(x, l, eBody) ->
    match l with  
    | h :: t -> 
      let h1 = match h with
               | CBool b -> bool2Boolean b
               | CInt  i -> Int i
               | CList l -> VList l
               | _       -> failwith("ForAllL error in head of list") 

      getDyadic (eval eBody ((x,h1) :: env)) (eval (ForAllL (x,t,eBody)) env) "∧"
    | []      -> eval eBody ((x,Boolean 1) :: env)
    
  | Exists(b,eBody) ->
    let (v1,v2) = 
      (eval eBody ((b,Boolean 0) :: env), eval eBody ((b,Boolean 1) :: env))
    getDyadic v1 v2 "∨"

  | Dyadic(op, e1, e2) -> 
    let (b1, b2) = (eval e1 env,eval e2 env)
    getDyadic b1 b2 op 
  | Atom _          -> Int 0 (* not implemented *)
  | Monadic ("¬",e) ->     (*negation operator*)    
    if (eval e env = Boolean 1) then Boolean 0 else Boolean 1
  |  _ -> failwith "Monadic operator no recognised, yo"

 
    
