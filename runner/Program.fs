open System

open System.Diagnostics
open System.IO
open System.Reflection
open System.Runtime.CompilerServices
open System.Runtime.Loader
open Ionide.ProjInfo.Types

module Projects =
    open Ionide.ProjInfo
    
    let getParameters (slnFile: FileInfo) =
        let toolsPath = Init.init slnFile.Directory None
        let defaultLoader: IWorkspaceLoader = WorkspaceLoader.Create(toolsPath, [])
        let graphLoader: IWorkspaceLoader = WorkspaceLoaderViaProjectGraph.Create(toolsPath, [])
        let subscription: System.IDisposable = defaultLoader.Notifications.Subscribe(fun msg ->
            printfn $"ionide changes: {msg.ProjFile}"
        )
        let projectOptions = defaultLoader.LoadSln(slnFile.FullName) |> Seq.toArray
        // let d = ProjectLoader.getProjectInfo (Path.Combine(root, "sample.fsproj")) [] BinaryLogGeneration.Off []
        // projectOptions.Length
        
        projectOptions
        // match d with
        // | Ok d -> d
        // | Error errorValue -> failwith errorValue

module Compiler =
    let checker = FSharp.Compiler.CodeAnalysis.FSharpChecker.Create(
        projectCacheSize = 1000,
        keepAssemblyContents = true,
        keepAllBackgroundResolutions = true,
        keepAllBackgroundSymbolUses = true,
        enableBackgroundItemKeyStoreAndSemanticClassification = true,
        enableParallelCheckingWithSignatureFiles = true,
        parallelReferenceResolution = true
    )
    
    let compile (p: ProjectOptions) =
        let parameters = [|
                yield ""
                yield "--nowin32manifest"
                yield p.OtherOptions[0].Replace("obj", "bin")
                yield! p.OtherOptions |> Seq.skip 1
                yield! p.SourceFiles
            |]
        // printfn $"P: %A{parameters}"
        let compileErrors, _ = 
            checker.Compile(parameters)
            |> Async.RunSynchronously
            
        printfn $"Error: %A{compileErrors}"
        ()
        
    let computeWithCli (p: ProjectOptions) =
        let startInfo = ProcessStartInfo()
        startInfo.FileName <- "dotnet"
        startInfo.WorkingDirectory <- FileInfo(p.ProjectFileName).Directory.FullName
        let args = [
            "build"
            "--no-restore"
            "--no-dependencies"
        ]
        for a in args do
          startInfo.ArgumentList.Add(a)
        startInfo.UseShellExecute <- false
        startInfo.CreateNoWindow <- true
        use p = new Process()
        p.StartInfo <- startInfo
        p.Start() |> ignore

        p.WaitForExitAsync() |> Async.AwaitTask |> Async.RunSynchronously
        
    let compileAll (projects: ProjectOptions seq) =
        projects
        |> Seq.iter (fun project -> compile project)

module TestsRunner =
    let callEntryPoint (entryPoint: MethodInfo) =
        let args = [|""|]
        entryPoint.Invoke(null, [|args :> obj|]) :?> int
        
    let searchEntryPoint (assembly:Assembly) =
        if isNull assembly.EntryPoint
        then
            assembly.DefinedTypes
            |> Seq.filter (fun t -> t.Name = "Program")
            |> Seq.collect (fun t -> t.DeclaredMethods)
            |> Seq.filter (fun m -> m.Name = "main")
            |> Seq.head
        else assembly.EntryPoint
    
    module DynamicAssemblies =
        // https://docs.microsoft.com/en-us/dotnet/standard/assembly/unloadability
        type AssemblyLoadContextWithUnloadSupport(path) =
            inherit AssemblyLoadContext(isCollectible = true, name = Guid.NewGuid().ToString())
            let resolver = AssemblyDependencyResolver(path)
            
            override x.Load(name: AssemblyName) =
                printfn $"Try load assembly: {name.Name}"
                let assemblyPath = resolver.ResolveAssemblyToPath(name)
                printfn $"Load assembly: {name.Name} -> {assemblyPath}"
                if isNull assemblyPath |> not
                then x.LoadFromAssemblyPath(assemblyPath)
                else
                    // TODO
                    if name.Name = "sample2"
                    then x.LoadFromAssemblyPath(Path.Combine(path.Replace("sample", "sample2"), "sample2.dll"))
                    else null
        
        [<MethodImpl(MethodImplOptions.NoInlining)>]
        let executeTests (p: ProjectOptions) =
            let loader = AssemblyLoadContextWithUnloadSupport(FileInfo(p.TargetPath).Directory.FullName)
            p.PackageReferences
            |> Seq.iter (fun package -> loader.LoadFromAssemblyPath package.FullPath |> ignore)
            // let assembly = loader.LoadFromAssemblyPath(p.TargetPath)
            use f = File.Open(p.TargetPath, FileMode.Open)
            let assembly = loader.LoadFromStream(f)
            let weakAssembly = WeakReference(assembly)
            
            let result =
                searchEntryPoint assembly
                |> callEntryPoint
            
            loader.add_Unloading(fun c -> printfn $"Unloading: {c.Name}")
            loader.Unload()
            result, weakAssembly
        
        let rec waitClean (weak: WeakReference) =
            GC.Collect()
            GC.WaitForPendingFinalizers()
            match weak.IsAlive with
            | true -> waitClean weak
            | false -> ()
        
        let executeTestsWithClean (p: ProjectOptions) =
            let result, assembly = executeTests p
            
            waitClean assembly
            
            result
    
    module CommandLine =       
        let executeCommand executable (args: string list) =
            let startInfo = ProcessStartInfo()
            startInfo.FileName <- executable
            startInfo.WorkingDirectory <- FileInfo(args[0]).Directory.FullName
            for a in args do
              startInfo.ArgumentList.Add(a)
            startInfo.UseShellExecute <- false
            startInfo.CreateNoWindow <- true
            use p = new Process()
            p.StartInfo <- startInfo
            p.Start() |> ignore

            p.WaitForExitAsync() |> Async.AwaitTask |> Async.RunSynchronously
            p.ExitCode
        
        let executeTests (p: ProjectOptions) =
            let outputDirectory = FileInfo(p.TargetPath).Directory.FullName
            p.ReferencedProjects
            |> Seq.collect (fun referencedProject ->
                let directory = FileInfo(referencedProject.ProjectFileName).Directory.FullName
                p.OtherOptions
                |> Ionide.ProjInfo.FscArguments.references
                |> Seq.filter (fun path -> path.StartsWith(directory))
            )
            |> Seq.iter (fun f ->
                let inF = f
                let outF = Path.Combine(outputDirectory, FileInfo(f).Name)
                File.Copy(f, Path.Combine(outputDirectory, FileInfo(f).Name), true)
            )
            
            executeCommand "dotnet" [p.TargetPath]
        
module FileWatch =
    let mutable lastRead = DateTime.MinValue
    
    let onChange (projects: ProjectOptions[]) runTests (file: FileSystemEventArgs) =
        printfn $"%A{file.ChangeType} %s{file.FullPath}"
        
        let lastWriteTime = File.GetLastWriteTime(file.FullPath)
        printfn $"lastWriteTime: %A{lastWriteTime}"
        
        match lastWriteTime = lastRead with
        | true -> printfn "<- ignored"
        | false ->
            lastRead <- lastWriteTime
            try
                match projects |> Array.tryFind (fun p -> p.SourceFiles |> List.contains file.FullPath) with
                | None -> printfn "<- no project impacted"
                | Some project ->
                    printfn $"Begin compile: {project.ProjectFileName}"
                    let watch = Stopwatch.StartNew();
                    Compiler.compile project
                    watch.Stop()
                    printfn $"End Compilation: {watch.Elapsed}"
                    printfn $"Run tests"
                    watch.Restart()
                    runTests ()
                    watch.Stop()
                    printfn $"End: {watch.Elapsed}"
            with
            | e ->
                printfn $"%A{e.Message}"
                printfn $"%A{e}"
                
    let listen root (projects: ProjectOptions[]) runTests =
        let watcher = new FileSystemWatcher()
        watcher.Path <- root
        watcher.Filter <- "*.fs"
        watcher.NotifyFilter <- NotifyFilters.LastWrite
        watcher.IncludeSubdirectories <- true
        watcher.Changed.Add (onChange projects runTests)
        watcher.EnableRaisingEvents <- true
        
        watcher

let rec findSln (directory: DirectoryInfo) name =
    match directory.EnumerateFiles(name) |> Seq.toList with
    | [] -> findSln directory.Parent name
    | sln::_ -> sln

[<EntryPoint>]
let main _ =
    printfn "Start"
    printfn "Current Directory: %s" (DirectoryInfo(".").FullName)
    
    let sln = findSln (DirectoryInfo(".")) "tests.sln"
    printfn $"sln: %s{sln.FullName}"
    
    let runTests projects () =
        projects
        |> Seq.iter (fun project ->
            let result, _ = TestsRunner.DynamicAssemblies.executeTests project
            printfn $"Test Result: %i{result}"
        )
    
    let watch = Stopwatch.StartNew()
    let projects = Projects.getParameters sln
    watch.Stop()
    printfn $"End init: {watch.Elapsed}"
    
    use _ = FileWatch.listen sln.Directory.FullName projects (runTests projects)
    
    watch.Restart()
    Compiler.compileAll projects
    watch.Stop()
    printfn $"End compilation: {watch.Elapsed}"
    
    runTests projects ()
    
    Console.ReadLine() |> ignore
    0
