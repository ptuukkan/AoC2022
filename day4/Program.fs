open System.IO

let isOverlap (arr: string[]) =
    let elf1, elf2 =
        arr
        |> Array.map int
        |> Array.splitAt 2
    (elf1[0] >= elf2[0] && elf1[1] <= elf2[1]) || (elf1[0] <= elf2[0] && elf1[1] >= elf2[1])

let isPartialOverlap (arr: string[]) =
    let elf1, elf2 =
        arr
        |> Array.map int
        |> Array.splitAt 2
    Set.intersect (Set.ofArray [|elf1[0]..elf1[1]|]) (Set.ofArray [|elf2[0]..elf2[1]|])
    |> (Set.isEmpty >> not)



[<EntryPoint>]
let main args =
    let input = File.ReadLines(args[0])
    let part1 =
        input
        |> List.ofSeq
        |> List.map (fun x -> x.Split[|','; '-'|])
        |> List.filter isOverlap
        |> List.length

    printfn "%d" part1

    let part2 =
        input
        |> List.ofSeq
        |> List.map (fun x -> x.Split[|','; '-'|])
        |> List.filter isPartialOverlap
        |> List.length

    printfn "%d" part2
    0
