namespace ChironB.Benchmarks

open Chiron
open BenchmarkDotNet.Attributes
open Newtonsoft.Json

module Bench =
    open System.IO
    open System.Text

    let resetStream (stream : #Stream) =
        stream.Seek(0L, SeekOrigin.Begin) |> ignore

    module Chiron =
        let inline parse (stream : #Stream) : Json =
            let reader = new StreamReader(stream)
            reader.ReadToEnd()
            |> Json.parse
            |> JsonResult.getOrThrow

        // let inline parseAndDeserialize (stream : #Stream) : 'a =
        //     let reader = new StreamReader(stream)
        //     reader.ReadToEnd()
        //     |> Json.parse
        //     |> Json.deserialize

    module JsonNET =
        open Newtonsoft.Json

        let serializer = JsonSerializer.CreateDefault()

        let inline deserialize<'a> (stream : #Stream) : 'a =
            let jsonReader = new StreamReader(stream, Encoding.UTF8)
            let reader = new JsonTextReader(jsonReader, CloseInput = false)
            serializer.Deserialize<'a> reader


        let inline deserialize2 (str : string) : Chiron.JsonResult<Chiron.Json>=
            let jsonReader = new StringReader(str)
            let reader = new JsonTextReader(jsonReader, CloseInput = false)
            let r = serializer.Deserialize(reader)
 
            Unchecked.defaultof<_>

    module Fast =

        open System.Text.Json
        open System.Collections.Generic

        let rec toJson (d : JsonElement) : Json =
            match d.ValueKind with
            | JsonValueKind.Null -> Json.Null
            | JsonValueKind.Array ->
                if d.GetArrayLength() > 0 then
                    d.EnumerateArray() |> Seq.toList |> List.map toJson |> Json.Array
                else
                    Json.Array []
            | JsonValueKind.False -> Json.False
            | JsonValueKind.True -> Json.True
            | JsonValueKind.Number -> d.GetRawText() |> Json.Number
            | JsonValueKind.Object ->
                d.EnumerateObject()
                |> Seq.map (fun e ->
                        (e.Name, toJson e.Value)
                   )
                |> Map.ofSeq
                |> JsonObject.ofMap
                |> Json.Object
            | JsonValueKind.String -> d.GetString() |> Json.String
            | JsonValueKind.Undefined -> Json.Null // TODO wrong
            | _ -> failwith "unknown type"

        let deserialize (str : string) : JsonResult<Json> =
            let doc =
                try
                    str |> JsonDocument.Parse |> Choice1Of2
                with e -> Choice2Of2 e
            match doc with
            | Choice1Of2 r ->
                try
                    r.RootElement |> toJson |> JsonResult.JPass
                with e ->
                    e.Message |> JsonFailureReason.InvalidJson |> JsonFailure.SingleFailure |> JFail
            | Choice2Of2 e ->
                e.Message |> JsonFailureReason.InvalidJson |> JsonFailure.SingleFailure |> JFail

        let rec toJsonOpt (d : JsonElement) : Json =
            match d.ValueKind with
            | JsonValueKind.Null -> Json.Null
            | JsonValueKind.Array ->
                let slowPath () =
                    d.EnumerateArray() |> Seq.toList |> List.map toJsonOpt |> Json.Array
                let fastPath() =
                    let e0 = d.Item 0
                    if e0.ValueKind = JsonValueKind.String then
                        let len = d.GetArrayLength()
                        let arr = Array.zeroCreate len
                        try
                            for i in 0 .. len - 1 do
                                let ei = d.Item i
                                if ei.ValueKind <> JsonValueKind.String then failwith ""
                                else
                                    arr.[i] <- ei.GetString() |> Json.String
                            arr |> Json.FastArray |> Some
                        with e -> None
                    else
                        None
                if d.GetArrayLength() > 0 then
                    match fastPath () with
                        | Some r -> r
                        | None -> slowPath()
                else
                    Json.Array []
            | JsonValueKind.False -> Json.False
            | JsonValueKind.True -> Json.True
            | JsonValueKind.Number -> d.GetRawText() |> Json.Number
            | JsonValueKind.Object ->
                let r = Dictionary<string,Json>()
                for e in d.EnumerateObject() do
                    r.[e.Name] <- toJsonOpt e.Value

                (r :> IReadOnlyDictionary<_,_>)
                |> JsonObject.FastReadObject
                |> Json.Object
            | JsonValueKind.String -> d.GetString() |> Json.String
            | JsonValueKind.Undefined -> Json.Null // TODO wrong
            | _ -> failwith "unknown type"

        let deserializeOpt (str : string) : JsonResult<Json> =
            let doc =
                try
                    str |> JsonDocument.Parse |> Choice1Of2
                with e -> Choice2Of2 e
            match doc with
            | Choice1Of2 r ->
                try
                    r.RootElement |> toJsonOpt |> JsonResult.JPass
                with e ->
                    e.Message |> JsonFailureReason.InvalidJson |> JsonFailure.SingleFailure |> JFail
            | Choice2Of2 e ->
                e.Message |> JsonFailureReason.InvalidJson |> JsonFailure.SingleFailure |> JFail


[<Config(typeof<CoreConfig>)>]
type ParseTest () =
    let mutable jsonString = "<null>"

    [<GlobalSetup>]
    member this.Setup () =
        jsonString <- loadJsonResourceAsString this.Name

    //[<Params("error", "fparsec", "user", "prettyuser", "social", "annotation")>]
    [<Params("annotation")>]
    member val Name = "<null>" with get, set

    [<Benchmark>]
    member __.Chiron_New () : Chiron.JsonResult<Chiron.Json> =
        Chiron.Parsing.Json.parse jsonString

    [<Benchmark>]
    member __.Chiron_TextJson () : Chiron.JsonResult<Chiron.Json> =
        Bench.Fast.deserialize jsonString

    [<Benchmark>]
    member __.Chiron_TextJsonOpt () : Chiron.JsonResult<Chiron.Json> =
        Bench.Fast.deserializeOpt jsonString

//[<Config(typeof<CoreConfig>)>]
//type FormatTest () =
//    let mutable jsonN = Chiron.Json.Null

//    [<GlobalSetup>]
//    member this.Setup () =
//        jsonN <-
//            loadJsonResourceAsString this.Name
//            |> Chiron.Parsing.Json.parse
//            |> Chiron.JsonResult.getOrThrow

//    [<Params("error", "fparsec", "user", "prettyuser", "social")>]
//    member val Name = "<null>" with get, set

//    [<Benchmark>]
//    member __.Chiron_New () =
//        Chiron.Formatting.Json.format jsonN

//[<Config(typeof<CoreConfig>)>]
//type FormatVariableLengthStrings () =
//    let mutable simpleJson = Chiron.Json.Null
//    let mutable escapedJson = Chiron.Json.Null

//    [<Params(10, 100, 1000, 10000, 100000)>]
//    member val public strlen = 1 with get, set

//    [<GlobalSetup>]
//    member x.Setup () =
//        let simple = String.replicate x.strlen "a"
//        simpleJson <- Chiron.Json.String simple
//        let escaped = String.replicate (x.strlen / 10) "\\u0004\\n\\\""
//        escapedJson <- Chiron.Json.String escaped

//    [<Benchmark>]
//    member __.Simple_New () =
//        Chiron.Formatting.Json.format simpleJson

//    [<Benchmark>]
//    member __.Escaped_New () =
//        Chiron.Formatting.Json.format escapedJson
