// include Fake libs
#r "./packages/FAKE/tools/FakeLib.dll"

open Fake
open System
// Directories
let buildDir  = "./build/"
let deployDir = "./deploy/"


// Filesets
let appReferences  =
    !! "/**/*.csproj"
    ++ "/**/*.fsproj"

// version info
let version = "0.1"  // or retrieve from CI server


// Oscar's Targets
let subfold          = "app"
let fsYacc           = "packages" </> "FsLexYacc" </> "build" </> "fsyacc.exe"
let fsLex            = "packages" </> "FsLexYacc" </> "build" </> "fslex.exe"
let FSLexYaccRuntime = "packages" </> "FsLexYacc.Runtime" </> "lib"
                       </> "portable-net45+netcore45+wpa81+wp8+MonoAndroid10+MonoTouch10"
                       </> "FsLexYacc.Runtime.dll"

let builder (target : string) =
  let (name,arg1,arg2) =
      match target with
       | "parser" ->
         let (n,a1,a2) = ("Parser", "--module FolPar ", "FolPar.fsy ")
         (n,a1,a2)
       | "lexer"  ->
         let (n,a1,a2) = ("Lexer", " ", "FolLex.fsl --unicode")
         (n,a1,a2)
       | _        ->
         let (n,a1,a2) = ("", "","")
         (n,a1,a2)
         
  let progPath : string =
      match target with
        | "parser" -> fsYacc
        | "lexer"  -> fsLex
        | _        -> "Wtf!"

  trace ("Building " </> name)

  let srcPath = __SOURCE_DIRECTORY__ </> "Rivano/src/" </> subfold

  let messages = ref []
  let appendMessage msg =  messages := msg :: !messages
  let exitCode  =
     ExecProcessWithLambdas
       (fun info ->

          info.FileName  <- progPath 
          info.Arguments <- arg1 </> srcPath </> arg2  ) (TimeSpan.FromSeconds(30.)) true appendMessage appendMessage
  let output = !messages |> List.rev |> Array.ofList
  printfn "Output from FsYacc:"
  for line in output do
    printfn "\t%s" line
                
  if exitCode <> 0 then
    failwithf "Process failed with code %d" exitCode

Target "Parser"(fun _ ->
  builder("parser")
            
)


Target "Lexer"(fun _ ->
  builder("lexer")
)


// Targets
Target "Clean" (fun _ ->
    CleanDirs [buildDir; deployDir]
)

Target "Build" (fun _ ->
    // compile all projects below src/app/
    MSBuildDebug buildDir "Build" appReferences
    |> Log "AppBuild-Output: "
)

Target "Deploy" (fun _ ->
    !! (buildDir + "/**/*.*")
    -- "*.zip"
    |> Zip buildDir (deployDir + "ApplicationName." + version + ".zip")
)

// Build order
"Clean"
  ==> "Build"
  ==> "Parser"
  ==> "Lexer"
  ==> "Deploy"

// start build
RunTargetOrDefault "Build"
