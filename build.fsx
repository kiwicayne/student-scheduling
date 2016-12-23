// include Fake libs
#r "./packages/FAKE/tools/FakeLib.dll"

open Fake
open Fake.Testing

let solutionFile = "SchedulingAndGrouping.sln"

// Directories
let buildDir  = "./build/"
let deployDir = "./deploy/"
let packagesDir = "./packages"

// Filesets
let appReferences  =
    !! "/**/*.csproj"
    ++ "/**/*.fsproj"

let testReferences = !! "src/*.Tests/**/*.fsproj"

// version info
let version = "0.1"  // or retrieve from CI server

// Targets
Target "Clean" (fun _ ->
    CleanDirs [buildDir; deployDir]
)

Target "Restore" (fun _ ->
    solutionFile
    |> RestoreMSSolutionPackages (fun p ->
         { p with             
             OutputPath = packagesDir
             Retries = 4 })
)

Target "Build" (fun _ ->
    // compile all projects below src/app/
    MSBuildDebug buildDir "Build" appReferences
    |> Log "AppBuild-Output: "
)

Target "Test" (fun _ ->
    // Work around bug in console test runner by adding a binding redirect to the console runner
    // so it can use FSharp.Core 4.4.0.0.  Hopefully this can be removed in a later version.
    "./tools/xunit/xunit.console.exe.config"
    |> CopyFile (packagesDir @@ "xunit.runner.console/tools/xunit.console.exe.config")

    !! (buildDir @@ "*.Tests.dll")
        |> xUnit (fun p -> 
            { p with                 
                ToolPath = packagesDir @@ "xunit.runner.console/tools/xunit.console.exe" })
)

Target "Deploy" (fun _ ->
    !! (buildDir + "/**/*.*")
    -- "*.zip"
    |> Zip buildDir (deployDir + "ApplicationName." + version + ".zip")
)

Target "Default" DoNothing

// Build order
"Clean"
  ==> "Restore"
  ==> "Build"
  ==> "Test"
  ==> "Deploy"
  ==> "Default"  

// start build
RunTargetOrDefault "Default"
