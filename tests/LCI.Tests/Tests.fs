module LCI.Fol.Tests
(*a reference to the type System.RunTime*)
open LCI.Fol

open NUnit.Framework
open FsCheck
open FsCheck.NUnit

[<Test>]
let ``hello returns 42`` () =
  
  Assert.AreEqual(42,42)


[Fact]
public void RevRevIsOrig(){
    Prop.ForAll<int[]>(xs => xs.Reverse().Reverse().SequenceEqual(xs))
        .QuickCheckThrowOnFailure();
}