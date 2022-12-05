// For more information see https://aka.ms/fsharp-console-apps

open System.IO

let isRock char =
    match char with
    | "A"
    | "X" -> true
    | _ -> false

let isPaper char =
    match char with
    | "B"
    | "Y" -> true
    | _ -> false

let isScissors char =
    match char with
    | "C"
    | "Z" -> true
    | _ -> false

let isWin a b =
    match b with
    | x when isRock b && isScissors a -> true
    | y when isPaper b && isRock a -> true
    | z when isScissors b && isPaper a -> true
    | _ -> false

let typeScore x =
    match x with
    | a when isRock x -> 1
    | b when isPaper x -> 2
    | c when isScissors x -> 3
    | _ -> 0

let play (rps: string []) : int =
    let a = rps[0]
    let b = rps[1]

    match b with
    | win when isWin a b -> 6 + typeScore b
    | loss when isWin b a -> 0 + typeScore b
    | _ -> 3 + typeScore b

let deriveSign (rps: string []) : string =
    let a = rps[0]
    let b = rps[1]

    match b with
    | "X" ->
        [| "A"; "B"; "C" |]
        |> Array.find (fun x -> isWin x a)
    | "Z" ->
        [| "A"; "B"; "C" |]
        |> Array.find (isWin a)
    | _ -> a

[<EntryPoint>]
let main args =
    let input = File.ReadLines(args[0])

    let part1 =
        input
        |> Array.ofSeq
        |> Array.map (fun x -> x.Split(" ") |> play)
        |> Array.sum

    printfn $"Part1: %d{part1}"

    let part2 =
        input
        |> Array.ofSeq
        |> Array.map (fun x ->
            x.Split(" ")
            |> fun arr -> [| arr[0]; deriveSign arr |] |> play)
        |> Array.sum
    
    printfn $"Part2: %d{part2}"

    0
