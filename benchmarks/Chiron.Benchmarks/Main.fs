module ChironB.Benchmarks.Program

open BenchmarkDotNet.Configs
open BenchmarkDotNet.Analysers
open BenchmarkDotNet.Diagnosers
//open BenchmarkDotNet.Diagnostics.Windows
open BenchmarkDotNet.Validators
open BenchmarkDotNet.Running

[<EntryPoint>]
let main argv =

    let sw = System.Diagnostics.Stopwatch.StartNew()
    let a = System.Text.Json.JsonDocument.Parse(System.IO.File.ReadAllText @"F:\development\chiron\benchmarks\Chiron.Benchmarks\annotation.json")
    sw.Stop()
    printfn "aj: %A %A" sw.Elapsed.TotalSeconds a

    let switcher = BenchmarkSwitcher thisAssembly
    let _ = switcher.Run argv
    0
