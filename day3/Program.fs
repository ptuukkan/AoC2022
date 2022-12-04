open System.IO
open System

let halfString str =
    let half = Operators.(/) (String.length str) 2
    (str.Substring(0, half), str.Substring(half))

let intersect (str1: string, str2: string) =
    Set.ofSeq str1
    |> Set.intersect (Set.ofSeq str2)
    |> Set.toList

let charToScore c =
    if Char.IsUpper c
    then int c - 38
    else int c - 96

let intersectChunk (chunk: list<string>) =
    Set.ofSeq chunk[0]
    |> Set.intersect (Set.ofSeq chunk[1])
    |> Set.intersect (Set.ofSeq chunk[2])
    |> Set.toList


[<EntryPoint>]
let main args =
    let input = File.ReadLines(args[0])
    // let input =
    //     seq {
    //         "vJrwpWtwJgWrhcsFMMfFFhFp"
    //         "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
    //         "PmmdzqPrVvPwwTWBwg"
    //         "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
    //         "ttgJtRGJQctTZtZT"
    //         "CrZsJsPPZsGzwwsLwLmpwMDw"
    //     }

    let part1 =
        input
        |> List.ofSeq
        |> List.map halfString
        |> List.map intersect
        |> List.map (List.map charToScore)
        |> List.map List.sum
        |> List.sum

    let part2 =
        input
        |> List.ofSeq
        |> List.chunkBySize 3
        |> List.map intersectChunk
        |> List.map (List.map charToScore)
        |> List.map List.sum
        |> List.sum

    printfn "%d" part1
    printfn "%d" part2
    0


