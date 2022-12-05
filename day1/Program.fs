// For more information see https://aka.ms/fsharp-console-apps

open System.IO

let arraySplit arr = seq {
    let mutable a = 0
    let length = Array.length arr - 1
    for i in 0 .. length do
        if arr[i] = ""
        then
            yield arr[a..i - 1]
            a <- i + 1
    if a < length
    then yield arr[a..length]
}

[<EntryPoint>]
let main args =
    let input = File.ReadLines(args[0])
    let part1 = 
        input
        |> Array.ofSeq
        |> arraySplit
        |> Array.ofSeq
        |> Array.map (fun x -> x |> Array.map int |> Array.sum)
        |> Array.sortDescending
        |> Array.head
        |> string
        
    let part2 = 
        input
        |> Array.ofSeq
        |> arraySplit
        |> Array.ofSeq
        |> Array.map (fun x -> x |> Array.map int |> Array.sum)
        |> Array.sortDescending
        |> Array.take 3
        |> Array.sum
        |> string
    
    printfn $"Part1: %s{part1}"
    printfn $"Part2: %s{part2}"
    0
    
