open System.IO
open System

let execute (cycles: int list) (instruction: string) : int list =
    let lastValue = cycles |> List.last

    match instruction with
    | "noop" -> cycles @ [ lastValue ]
    | _ -> cycles @ [ lastValue + Int32.Parse(instruction.Split(' ')[1]) ]

let mapToCycles (instruction: string) : string list =
    match instruction with
    | "noop" -> [ instruction ]
    | _ -> [ "noop"; instruction ]

[<EntryPoint>]
let main args =
    let instructions = File.ReadLines args[0] |> List.ofSeq

    let cycles = instructions |> List.collect mapToCycles |> List.fold execute [ 1 ]

    [ 20; 60; 100; 140; 180; 220 ]
    |> List.map (fun c -> c * (cycles |> List.item (c - 1)))
    |> List.sum
    |> printfn "%d"

    cycles
    |> List.mapi (fun c v ->
        match abs (v - c % 40) with
        | 0
        | 1 -> '#'
        | _ -> '.')
    |> List.chunkBySize 40
    |> List.map String.Concat
    |> List.iter (printfn "%s")

    0
