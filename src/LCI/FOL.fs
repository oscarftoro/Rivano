namespace LCI

/// A tiny language to express First Order Logic formulas
///
/// ## Example
///
///     let h = Library.hello 1
///     printfn "%d" h
///

module AST = 

  type prop = Atom  of string
            | Neg   of prop
            | Conj  of prop * prop
            | Disj  of prop * prop

  
     
// module FOL = 
  
  /// Returns 42
  ///
  /// ## Parameters
  ///  - `num` - whatever

