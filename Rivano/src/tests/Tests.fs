
module Tests 

  open Expecto
  open Dk.Nqn.Fol (*
  open Dk.Nqn.Parse

  [<Tests>]
  let tests =
    testList "tests" [
      testList "logic, arithmetics, strings and atoms" [
        testCase "parsing, lexing and evaluating let expressions" <| fun () -> 
          let let01 = fromString("let x = atom \"hola\" in x end") // Let ("x",Atom (CString "hola"),Var "x")
          let ev01 = eval let01 []  
          Expect.equal ev01 (VString "hola") "Parsing and evaluating an atom string: let x = atom \"hola\" in x end" 
        
        testCase "parsing, lexing and evaluating substractions" <| fun () ->
          let sub01 = fromString("let y = 8 in y - 3 end")
          let ev02 = eval sub01 [];
          Expect.equal ev02 (VInt 5) "8 - 3 should be 5" 

        testCase "parsing, lexing and evaluating or" <| fun () -> 
          let or01 = fromString("true or false") //Dyadic ("∨",CBool true,CBool false)
          let evOr01 = eval or01 []
          Expect.equal evOr01 (VBoolean 1) "true or false should be true"
      
        testCase "parsing, lexing and evaluating implication" <| fun () -> 
          let imp01 = fromString("true => false")//Dyadic ("→",CBool true,CBool false)
          let evimp01 = eval imp01 []
          Expect.equal evimp01 (VBoolean 0) "true implies false should be false"

        testCase "parsing, lexing and evaluating atomic expressions" <| fun () -> 
          //Atom(Dyadic (">",Dyadic ("+",CInt 3,CInt 2),Atom (Dyadic ("-",CInt 2,CInt 1))))
          let at01 = fromString("atom 3 + 2 > atom 2 - 1")
          let evat01 = eval at01 []
          Expect.equal evat01 (VBoolean 1) "atom 3 + 2 > atom 2 - 1 is true"

        testCase "parsing, lexing and evaluating arithmetic equality" <| fun () -> 
          let eq01 = fromString("2 = 2")//Dyadic ("=",CInt 2,CInt 2)
          let evEq01 = eval eq01 []
          Expect.equal evEq01 (VBoolean 1) "2 = 2 are equal"
          
        testCase "parsing, lexing and evaluating lists" <| fun () -> 
          //Let ("x",CList [CInt 1; CInt 2; CInt 3; CInt 4],Var "x")
          let list01 = fromString("let x = [1,2,3,4] in x end")
          let evList01 = eval list01 []
          Expect.equal evList01 (VList [VInt 1; VInt 2; VInt 3; VInt 4]) "a list of numbers is aV list of VInt"
      
        testCase "parsing, lexing and evaluating trivial universal equality" <| fun () ->
          //Let ("x",CList [CBool true; CBool false; CBool false; CBool true],Var "x") 
          let list02 = fromString("let x = [true, false,false,true] in x end")
          let evList02 = eval list02 []
          Expect.equal evList02 (VList [VBoolean 1; VBoolean 0; VBoolean 0; VBoolean 1]) "list of booleans should be list of VBooleans" 
      
        testCase "parsing, lexing and evaluating arithmetic equality" <| fun () ->
          //Let
          //  ("X",CList [CInt 1; CInt 2; CInt 3; CInt 4],
          //  UniQt ("∧","x","X",Dyadic ("=",Var "x",Var "x")))
          let Ueq01 = fromString("let X = [1,2,3,4] in forall x in X. x = x end")
          let evUEq01 = eval Ueq01 []
          Expect.equal evUEq01 (VBoolean 1) "2 = 2 are equal"

        testCase "parsing, lexing and evaluating universal quantification with >" <| fun () ->
        //Let
        //("X",CList [CInt 1; CInt 2; CInt 3; CInt 4],
        // UniQt ("∧","x","X",Dyadic (">",Var "x",CInt 5)))
          let fa01  = fromString("let X = [1,2,3,4] in forall x in X. x > 5 end")
          let evf01 = eval fa01 []
          Expect.equal evf01  (VBoolean 0) "for all x in [1,2,3,4]. x > 5 is false"

        testCase "parsing, lexing and evaluating universal quantification2 with <" <| fun () ->
        //Let
        //("X",CList [CInt 1; CInt 2; CInt 3; CInt 4],
        // UniQt ("∧","x","X",Dyadic ("<",Var "x",CInt 5)))
          let fa02  = fromString("let X = [1,2,3,4] in forall x in X. x < 5 end")
          let evf02 = eval fa02 []
          Expect.equal evf02  (VBoolean 1) "for all x in [1,2,3,4]. x < 5 is false"

        testCase "parsing, lexing and evaluating existential quantification with =" <| fun () ->
        // Let
        // ("X",CList [CInt 1; CInt 2; CInt 3; CInt 4],
        // UniQt ("∨","x","X",Dyadic ("=",Var "x",CInt 0)))
          let ex01  = fromString("let X = [1,2,3,4] in exists x in X. x = 0 end")
          let evEx01 = eval ex01 []
          Expect.equal evEx01  (VBoolean 0) "there exists an x in [1,2,3,4]. x = 0 is false"

        testCase "parsing, lexing and evaluating existential quantification with =" <| fun () ->
        // Let
        // ("X",CList [CInt 1; CInt 2; CInt 3; CInt 4],
        // UniQt ("∨","x","X",Dyadic ("=",Var "x",CInt 0)))
          let ex02  = fromString("let X = [1,2,3,4] in exists x in X. x = 3 end")
          let evEx02 = eval ex02 []
          Expect.equal evEx02  (VBoolean 1) "there exists an x in [1,2,3,4]. x = 3 is false"

        testCase "parsing, lexing and evaluating arithmetic expressions with >" <| fun () ->
          let gt01   = fromString("12 > 3") //Dyadic (">",CInt 12,CInt 3) 
          let evgt01 = eval gt01 [] 
          Expect.equal evgt01 (VBoolean 1) "12  is bigger than 3, of course"

        testCase "parsing, lexing and evaluating arithmetic expressions with <" <| fun () ->
          let lt01   = fromString("12 < 3")
          let evlt01 = eval lt01 []
          Expect.equal evlt01 (VBoolean 0) "12 is less than 3 is a falacy"
        
      ]
    ]

   *)