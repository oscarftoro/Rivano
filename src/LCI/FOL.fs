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

module Interpreter

let rec eval (e: Expr) (env: value env) : int =
  match e with
  | CInt    i    -> i
  | CBool   b    -> if b then 1 else 0
  | Dyadic(op, e1, e2) -> 
      let i1 = eval e1 env
      let i2 = eval e2 env
      match op with
      | "&" -> if i1 = 1 then i2 else 0 
      | "|" -> if i1 = 1 then 1 else i2 

  | Atom _       -> 0
  | CString _    -> 0
  | Neg  p       -> if p then 0 else 1




