open System.IO
open System

type Tree = { Height: int; Attribute: int }

let noteVisibleTrees (previousTrees: Tree list) (currentTree: Tree) =
    match previousTrees |> List.tryLast with
    | None -> previousTrees @ [ { currentTree with Attribute = 1 } ]
    | Some tree ->
        match tree.Attribute with
        | 1 when currentTree.Height > tree.Height -> previousTrees @ [ { currentTree with Attribute = 1 } ]
        | 0 when currentTree.Height <= tree.Height -> previousTrees @ [ { currentTree with Attribute = 0 } ]
        | _ ->
            match previousTrees |> List.tryFind (fun x -> x.Height >= currentTree.Height) with
            | None -> previousTrees @ [ { currentTree with Attribute = 1 } ]
            | Some _ -> previousTrees @ [ { currentTree with Attribute = 0 } ]

let viewingDistance (previousTrees: Tree list) (currentTree: Tree) =
    match previousTrees |> List.tryFindIndexBack (fun x -> x.Height >= currentTree.Height) with
    | None -> [ { currentTree with Attribute = previousTrees |> List.length } ]
    | Some index -> [ { currentTree with Attribute = (previousTrees |> List.length) - index } ]

let processWest processFunction forest : Tree list list =
    forest |> List.map (List.fold processFunction [])

let processEast processFunction forest : Tree list list =
    forest |> List.map (List.rev >> List.fold processFunction [] >> List.rev)

let processNorth processFunction forest : Tree list list =
    forest
    |> List.transpose
    |> List.map (List.fold processFunction [])
    |> List.transpose

let processSouth processFunction forest : Tree list list =
    forest
    |> List.transpose
    |> List.map (List.rev >> List.fold processFunction [] >> List.rev)
    |> List.transpose

let mapForest mapFunction west north east south : Tree list list =
    west
    |> List.map2 (List.map2 mapFunction) north
    |> List.map2 (List.map2 mapFunction) east
    |> List.map2 (List.map2 mapFunction) south

let part1 forest =
    let visibleFromWest = forest |> processWest noteVisibleTrees
    let visibleFromNorth = forest |> processNorth noteVisibleTrees
    let visibleFromEast = forest |> processEast noteVisibleTrees
    let visibleFromSouth = forest |> processSouth noteVisibleTrees

    let visibleTreesCount =
        visibleFromWest
        |> mapForest
            (fun tree1 tree2 -> { tree1 with Attribute = tree1.Attribute + tree2.Attribute })
            visibleFromEast
            visibleFromNorth
            visibleFromSouth
        |> List.map (fun line -> line |> List.filter (fun tree -> tree.Attribute = 1) |> List.length)
        |> List.sum

    printfn "%d" visibleTreesCount

let part2 forest =
    let scenicScoreWest = forest |> processWest viewingDistance
    let scenicScoreNorth = forest |> processNorth viewingDistance
    let scenicScoreEast = forest |> processEast viewingDistance
    let scenicScoreSouth = forest |> processSouth viewingDistance
    
    scenicScoreSouth |> List.iter (printfn "%0A")

    let highestScenicScore =
        scenicScoreWest
        |> mapForest
            (fun tree1 tree2 -> { tree1 with Attribute = tree1.Attribute * tree2.Attribute })
            scenicScoreNorth
            scenicScoreEast
            scenicScoreSouth
        |> List.map (List.map (fun x -> x.Attribute) >> List.max)
        |> List.max

    printfn "%d" highestScenicScore

[<EntryPoint>]
let main args =
    let forest =
        File.ReadLines args[0]
        |> List.ofSeq
        |> List.map List.ofSeq
        |> List.map (
            List.map (fun x ->
                { Height = (string >> Int32.Parse) x
                  Attribute = 0 })
        )

    part1 forest
    // part2 forest
    0
