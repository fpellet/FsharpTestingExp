open System

open System.IO
open System.Reflection
open System.Runtime.CompilerServices
open System.Runtime.Loader
open FSharp.Compiler.SourceCodeServices

let root = Path.GetFullPath("../../../../sample")
let binDirectory = Path.Combine(root, "bin/Debug/netcoreapp3.1/")
let output = Path.Combine(binDirectory, "sample.dll")

let dependenciesRootDirectory = Path.GetFullPath("../../../../packages")
let dependencies =
    [
        "Expecto\\lib\\netstandard2.0\\Expecto.dll"
        "FSharp.Core\\lib\\netstandard2.0\\FSharp.Core.dll"
    ] |> List.map (fun name -> Path.Combine(dependenciesRootDirectory, name))
let sources =
    [
        "Module1.fs"
        "Module2.fs"
        "Module3.fs"
        "Module4.fs"
        "Module5.fs"
        "Module6.fs"
        "Program.fs"
    ] |> List.map (fun name -> Path.Combine(root, name))
    
let arguments = [|
    "fsc.exe"
    "-o:" + output
    "--nowin32manifest"
    "-g"
    "--debug:portable"
    "--define:TRACE"
    "--define:DEBUG"
    "--define:NETCOREAPP"
    "--define:NETCOREAPP3_1"
    "--optimize-"
    "--tailcalls-"
    yield! dependencies |> Seq.map (fun path -> "-r:" + path)
    "--nocopyfsharpcore"
    "--simpleresolution"
    yield! sources
|]

let searchEntryPoint (assembly:Assembly) =
    if isNull assembly.EntryPoint
    then
        assembly.DefinedTypes
        |> Seq.filter (fun t -> t.Name = "Program")
        |> Seq.collect (fun t -> t.DeclaredMethods)
        |> Seq.filter (fun m -> m.Name = "main")
        |> Seq.head
    else assembly.EntryPoint

let callEntryPoint (entryPoint: MethodInfo) =
    let args = [|""|]
    entryPoint.Invoke(null, [|args :> obj|]) :?> int

module DynamicAssemblies =
    let compile (checker: FSharpChecker) =
        let compileErrors, _, assembly = 
            checker.CompileToDynamicAssembly(arguments, Some(stdout,stderr))
            |> Async.RunSynchronously
            
        printfn "Error: %A" compileErrors
        match assembly with
        | None -> 1
        | Some assembly ->
            searchEntryPoint assembly
            |> callEntryPoint

module StaticAssemblies =
    // https://docs.microsoft.com/en-us/dotnet/standard/assembly/unloadability
    type AssemblyLoadContextWithUnloadSupport(path) =
        inherit AssemblyLoadContext(isCollectible = true)
        let resolver = AssemblyDependencyResolver(path)
        
        override x.Load(name: AssemblyName) =
            let assemblyPath = resolver.ResolveAssemblyToPath(name)
            if isNull assemblyPath |> not
            then x.LoadFromAssemblyPath(assemblyPath)
            else null

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let compileWithoutClean (checker: FSharpChecker) =
        let compileErrors, _ = 
            checker.Compile(arguments)
            |> Async.RunSynchronously
            
        printfn "Error: %A" compileErrors
        
        let loader = AssemblyLoadContextWithUnloadSupport(binDirectory)
        dependencies
        |> Seq.iter (fun path -> loader.LoadFromAssemblyPath path |> ignore)
        let assembly = loader.LoadFromAssemblyPath(output)
        let weakAssembly = WeakReference(assembly)

        let result =
            searchEntryPoint assembly
            |> callEntryPoint
        
        loader.Unload()
        result, weakAssembly
    
    let rec waitClean (weak: WeakReference) =
        GC.Collect()
        GC.WaitForPendingFinalizers()
        match weak.IsAlive with
        | true -> waitClean weak
        | false -> ()
    
    let compile (checker: FSharpChecker) =
        let result, assembly = compileWithoutClean checker
        
        waitClean assembly
        
        result

let checker = FSharpChecker.Create()

let compile () =
    let result = DynamicAssemblies.compile checker
//    let result = StaticAssemblies.compile checker
    printfn "Result: %i" result
    
let mutable lastRead = DateTime.MinValue

let onChange (file: FileSystemEventArgs) =
    printfn "%A %s" file.ChangeType file.FullPath
    
    let lastWriteTime = File.GetLastWriteTime(file.FullPath)
    match lastWriteTime = lastRead with
    | true -> printfn "<- ignored"
    | false ->
        lastRead <- lastWriteTime
        compile ()

[<EntryPoint>]
let main argv =
    use watcher = new FileSystemWatcher()
    watcher.Path <- root
    watcher.Filter <- "*.fs"
    watcher.NotifyFilter <- NotifyFilters.LastWrite
    watcher.Changed.Add onChange
    watcher.EnableRaisingEvents <- true
    
    compile ()
    
    Console.ReadLine() |> ignore
    0
