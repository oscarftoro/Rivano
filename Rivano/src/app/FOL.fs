namespace Dk.Nqn

/// A tiny language to express First Order Logic formulas
///
/// ## Example
///
///     let h = Library.hello 1
///     printfn "%d" h
///  
  module Fol =

    (*environment, knowledge base or context *)
    type 't env = (string * 't) list
    and Boolean = Boolean of bool
         
    (* Abstract Syntax *)
    //type Prop = CBool of bool
    and expr = 
              | CInt    of int 
              | CBool   of bool  
              | CString of string  
              | CList   of expr list         
              | Var     of string
              | Let     of string * expr * expr
              | Quant   of string * string * string * expr
              | QuantL  of string * string * value list * expr    
              | Atom    of expr
              | Dyadic  of string * expr * expr
              | Monadic of string * expr


    (* runtime values *) 
    and value = 
      | VInt     of int
      | VBoolean of int   
      | VList   of value list    
      | VString of string //to support name of variables in ZFLists     
      | VDouble of double
    /// Check variable `x` in the environment `env`*)
    /// returns the `value` of the variable
    let rec lookup x env : value = 
      match env with
      | [] -> failwith (x + "Variable not found in the environment, yo")
      | (y,v)::t -> if x=y then v else lookup  x t
    
    let lookupVList s env : list<value> =  
      match lookup s env with
      | VList l   ->  l
      | VBoolean _ -> failwith ("Boolean when list expected error in lookupVList,yo")
      | VInt     _ -> failwith ("Int when VList was expected in lookupVList, yo")
      | __________ -> failwith ("Whethever type when VList was expected in lookupVList" )

    let rec lookupL x (env : expr env) : expr list =
      match env with
      | []      -> failwith (x + "Variable not found in the environment, yo")
      | (_,(CBool _)):: _ -> failwith("Boolean value in value env of lists") 
      | (_,(CInt _)):: _  -> failwith("Int value in value env of lists")
      | (y,(CList v)):: t -> if x=y then v else lookupL  x t
      | _ -> failwith("que shusha")

    //Evaluator's auxiliary functions
    //unwrapper Boolean b -> bool
    let getBool(b: value) : bool = 
      match b with
      | VBoolean b -> b = 1
      | VInt     _ -> failwith("Integer value in getBool, yo")
      | __________ -> failwith("not a Bool in getBool, yo")
      
    //unwrapper Integer i -> int
    let getInt (i: value) : int =
       match i with
       | VBoolean _ -> failwith("Boolean value in getInt, yo")
       | VInt     i -> i
       | __________ -> failwith("not an Int in getInt, yo")

    //bool -> Boolean b converter
    let bool2Boolean (b:bool) : value =  if b then VBoolean 1 else VBoolean 0
    //int ->Int i converter
    let int2Int (i:int) :value = VInt i
      
    let getDyadic (v1 : value) (v2: value) (op: string): value =
      match op with
      | "∧" ->
        bool2Boolean (v1 |> getBool  &&  v2 |> getBool)
      | "∨" ->
        bool2Boolean (v1 |> getBool  ||  v2 |> getBool)  
      | "→" ->
        bool2Boolean (not (v1 |> getBool) ||  v2 |> getBool )
      | "=" ->
        bool2Boolean (getInt v1 = getInt v2)    
      | ">" ->
        bool2Boolean ((getInt v1) > (getInt v2))  
      | "<" ->
        bool2Boolean ((getInt v1) < (getInt v2))  
      | "+" ->
        getInt v1 + getInt v2 |> int2Int 
      | "-" ->
        getInt v1 - getInt v2 |> int2Int
      | "*" ->
        getInt v1 * getInt v2 |> int2Int
      | _   -> failwithf ("Binary operator not supported for boolean expressions")

      /// Evaluator of logical expressions
      /// Returns integers or booleans
      /// ## Parameters
      ///  - `e` - expresion
      ///  - `env` - environment or context
    let rec eval (e: expr) (env: value env) : value =
      match e with  
      | CInt    i    -> VInt i
      | CString s ->  VString s 
      | CBool   b    -> if b then VBoolean 1 else VBoolean 0
      | CList   l    -> VList(List.fold (fun acc x -> (eval x env)::acc  ) [] l |> List.rev) 
      | Var     x    ->
          match lookup x env with
          | VInt i     -> VInt i
          | VBoolean b -> VBoolean b
          | VList v    ->  VList v    
          | VString s  ->  VString s
          | VDouble d       -> VDouble d

      | Let (s,eRhs,lBody) -> 
         let xVal =  eval eRhs env
         let bodyEnv = (s,xVal) :: env
         eval lBody bodyEnv     
      //u is a string representing the unique identifier 
      | Quant(u,x,s,eBody) ->  //at this point s is a string, the address of a list in the environment
        let l1 =  (lookupVList s env) 
       
        env |>  eval (QuantL (u,x, l1, eBody))
      | QuantL(u,x, l, eBody) -> // but here s is now l, a list of values
        let baseCase = match u with
                       | "∧" -> VBoolean 1
                       | _   -> VBoolean 0 //the zero element in the base case for disjunction has to be  false
        match l with  
        | h :: t -> 
          let h1 = match h with
                   | VBoolean b -> VBoolean b
                   | VInt  i    -> VInt i
                   | VList l    -> VList l
                   | _          -> failwith("Universal Quantifier error in head of list")       
          //printf "an eval of eBody: %A " <| eval eBody ((x,h1) :: env)
          //printf "the eBody %A" eBody
          //printfn "the operator: %A" u
          getDyadic (eval eBody ((x,h1) :: env)) (eval (QuantL (u,x,t,eBody)) env) u 
        | []      -> baseCase 
        
      | Dyadic(op, e1, e2) -> 
        let (b1, b2) = (eval e1 env,eval e2 env)
        getDyadic b1 b2 op 
      | Atom a          -> eval a env (* not implemented *)
      | Monadic ("¬",e) ->     (*negation operator*)    
        if (eval e env = VBoolean 1) then VBoolean 0 else VBoolean 1
      |  _ -> failwith "Monadic operator no recognised, yo"

     
    
