open System.IO

let spreadInput (c: char) (acc: string list) =
    match acc.Length with
    | x when x < 1 -> [ string c ]
    | x when x < 4 -> string c + List.head acc  :: acc
    | _ -> string c + (List.head acc).Substring(0, 3) :: acc

let spreadInputPart2 (c: char) (acc: string list) =
    match acc.Length with
    | x when x < 1 -> [ string c ]
    | x when x < 14 -> string c + List.head acc  :: acc
    | _ -> string c + (List.head acc).Substring(0, 13) :: acc

[<EntryPoint>]
let main args =
    let input =
        File.ReadLines args[0]
        |> List.ofSeq

    let part1 =
        input
        |> List.head
        |> List.ofSeq
        |> fun x -> (x, []) ||> List.foldBack spreadInput
        |> List.findIndex (fun x ->
            let distinct = Seq.distinct x |> Seq.length
            distinct = 4)
        |> Operators.(+) 4
        
    let part2 =
        input
        |> List.head
        |> List.ofSeq
        |> fun x -> (x, []) ||> List.foldBack spreadInputPart2
        |> List.findIndex (fun x ->
            let distinct = Seq.distinct x |> Seq.length
            distinct = 14)
        |> Operators.(+) 14
            
    printfn "%d" part1
    printfn "%d" part2
    0
