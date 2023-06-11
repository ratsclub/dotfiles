open System

open Fake.Core
open Fake.DotNet
open Fake.Tools
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators

module dotnet =
    let build cmdParams args = DotNet.exec cmdParams "build" args
    let run cmdParams args = DotNet.exec cmdParams "run" args

let testProjects =
    !! (__SOURCE_DIRECTORY__ </> ".." </> "tests/**/*.fsproj")

let srcProjects =
    !! (__SOURCE_DIRECTORY__ </> ".." </> "src/**/*.fsproj")

let ``clean`` _ =
    let artifactDirs project =
        project
        |> Seq.collect (fun project ->
            [ project </> ".." </> "bin";
              project </> ".." </> "obj"; ])

    let allDirs =
        srcProjects
        |> Seq.append testProjects
        |> artifactDirs

    allDirs |> Shell.cleanDirs

let ``run dotnet build`` _ =
    srcProjects
    |> Seq.iter (fun project ->
        dotnet.build id (sprintf "%s" project)
        |> fun r -> if r.ExitCode <> 0 then failwithf "Running %s on .NET Core failed" project)

let ``run tests`` _ =
    testProjects
    |> Seq.iter (fun project ->
        dotnet.run id (sprintf "--project %s" project)
        |> fun r -> if r.ExitCode <> 0 then failwithf "Running %s on .NET Core failed" project)

let initializeTargets () =
    // Targets
    Target.create "clean" ``clean``
    Target.create "build" ``run dotnet build``
    Target.create "test" ``run tests``

    // Dependencies
    "clean"
    ==> "build"
    ==> "test"
    |> ignore

[<EntryPoint>]
let main argv =
    argv
    |> Array.toList
    |> Context.FakeExecutionContext.Create false "build.fsx"
    |> Context.RuntimeContext.Fake
    |> Context.setExecutionContext

    let target =
      match argv |> Array.tryHead with
      | Some target -> target
      | None -> "build"

    initializeTargets ()
    Target.runOrDefaultWithArguments target

    0 // return an integer exit code
