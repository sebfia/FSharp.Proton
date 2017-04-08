#r @"packages/build/FAKE/tools/FakeLib.dll"
#r "System.IO.Compression.FileSystem"


open Fake
open Fake.Git
open Fake.AssemblyInfoFile
open Fake.ReleaseNotesHelper
open System
open System.IO
open Fake.Testing.Expecto

let project = "Proton"

let summary = "A Chiron-like library for serializing and deserializing any instance to and from protobuf."

let description = summary

let configuration = "Release"

let path = "." |> FullName

let deployDir = "./deploy"

let dotnetSDKPath = System.Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData) </> "dotnetcore" |> FullName

//let dotnetExePath =
//    dotnetSDKPath </> (if isWindows then "dotnet.exe" else "dotnet")
//    |> FullName

let dotnetExePath = if isWindows then "dotnet.exe" else "dotnet"

// Helper active pattern for project types
let (|Fsproj|Csproj|Vbproj|Shproj|) (projFileName:string) =
    match projFileName with
    | f when f.EndsWith("fsproj") -> Fsproj
    | f when f.EndsWith("csproj") -> Csproj
    | f when f.EndsWith("vbproj") -> Vbproj
    | f when f.EndsWith("shproj") -> Shproj
    | _                           -> failwith (sprintf "Project file %s not supported. Unknown project type." projFileName)

// Read additional information from the release notes document
let release = LoadReleaseNotes "RELEASE_NOTES.md"
let packageVersion = SemVerHelper.parse release.NugetVersion


// Generate assembly info files with the right version & up-to-date information
Target "AssemblyInfo" (fun _ ->
    let getAssemblyInfoAttributes projectName =
        [ Attribute.Title (projectName)
          Attribute.Product project
          Attribute.Description summary
          Attribute.Version release.AssemblyVersion
          Attribute.FileVersion release.AssemblyVersion
          Attribute.Configuration configuration ]

    let getProjectDetails projectPath =
        let projectName = System.IO.Path.GetFileNameWithoutExtension(projectPath)
        ( projectPath,
          projectName,
          System.IO.Path.GetDirectoryName(projectPath),
          (getAssemblyInfoAttributes projectName)
        )

    !! "src/**/*.??proj"
    |> Seq.map getProjectDetails
    |> Seq.iter (fun (projFileName, projectName, folderName, attributes) ->
        match projFileName with
        | Fsproj -> CreateFSharpAssemblyInfo (folderName </> "AssemblyInfo.fs") attributes
        | Csproj -> CreateCSharpAssemblyInfo ((folderName </> "Properties") </> "AssemblyInfo.cs") attributes
        | Vbproj -> CreateVisualBasicAssemblyInfo ((folderName </> "My Project") </> "AssemblyInfo.vb") attributes
        | Shproj -> ()
        )
)

// Clean build results
Target "Clean" (fun _ ->
    CleanDirs ["bin"; "temp"; "docs/output"; deployDir ]
)

Target "Build" (fun _ ->
    let result =
        ExecProcess (fun info ->
            info.FileName <- dotnetExePath
            info.WorkingDirectory <- path
            info.Arguments <- "restore") TimeSpan.MaxValue
    if result <> 0 then failwith "Restore failed"

    let result =
        ExecProcess (fun info ->
            info.FileName <- dotnetExePath
            info.WorkingDirectory <- path
            info.Arguments <- "build") TimeSpan.MaxValue
    if result <> 0 then failwith "Build failed"
)

Target "Deploy" (fun _ ->
    let result =
        ExecProcess (fun info ->
            info.FileName <- dotnetExePath
            info.WorkingDirectory <- path
            info.Arguments <- "publish -c Release -o \"" + FullName deployDir + "\"") TimeSpan.MaxValue
    if result <> 0 then failwith "Publish failed"
)

Target "All" DoNothing

"Clean"
  ==> "AssemblyInfo"
  ==> "Build"
  ==> "Deploy"
  ==> "All"
  //==> "CreateDockerImage"
  //==> "PrepareRelease"
  //==> "Deploy"

RunTargetOrDefault "All"