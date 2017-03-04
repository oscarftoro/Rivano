//namespace LCI

/// A tiny language to express First Order Logic formulas
///
/// ## Example
///
///     let h = Library.hello 1
///     printfn "%d" h
///
  
module Fol

(* Abstract Syntax *)
//type Prop = CBool of bool
type Expr = 
          | CInt    of int 
          | CBool   of bool       
          | CString of string
          | Atom    of string
          | Dyadic  of string * Expr * Expr
          | Neg     of bool
          | Conj    of bool * bool
          | Disj    of bool * bool


(*environment, knowledge base or context *)
type 't env = (string * 't) list
(* value types of the environment*)
type value = 
  | Int of int
  | Boolean of bool 
  | String of string
 

let rec lookup env x = 
  match env with
  | [] -> failwith (x + "not found")
  | (y,v)::t -> if x=y then v else lookup t x;;

let neg(b: bool) : bool = not b

  /// Returns integers
  ///
  /// ## Parameters
  ///  - `e` - expresion
  ///  - `env` - environment or context

let rec eval (e: Expr) (env: value env) : int =
  match e with
  | CInt    i    -> i
  | CBool   b    -> if b then 1 else 0
  | Atom _       -> 0
  | CString _    -> 0
  | Neg  p       -> if p then 0 else 1
  | Conj (p1,p2) -> if p1 then (eval (CBool p2) env) else 0
  | Disj (p1,p2) -> if p1 then 0 else (eval(CBool p2) env)



