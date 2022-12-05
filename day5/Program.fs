open System.IO
open System.Text.RegularExpressions

let splitInput input =
    let index = input |> List.findIndex (fun elem -> elem = "")
    input |> List.splitAt index

let createStacks input =
    let initialValue = [[]; []; []]
    let foldFunction stacks (elem: string) =
        let stack1 = stacks[0] @ [elem[1]]
        let stack2 = stacks[1] @ [elem[5]]
        let stack3 = stacks[2] @ [elem[9]]
        [stack1, stack2, stack3]
    List.fold foldFunction initialValue input

let removeEmptyElements (stacks: char list * char list * char list) =
    let stack1, stack2, stack3 = stacks
    let newStack1 = List.filter (fun x -> x <> ' ') stack1
    let newStack2 = List.filter (fun x -> x <> ' ') stack2
    let newStack3 = List.filter (fun x -> x <> ' ') stack3
    (newStack1, newStack2, newStack3)

[<EntryPoint>]
let main args =
    let input = File.ReadLines args[0]
    let stacksInput, instructionsInput =
        input
        |> List.ofSeq
        |> splitInput
    let stacks =
        stacksInput 
        |> List.truncate (List.length stacksInput - 1)
        |> createStacks
        |> removeEmptyElements
    let instructions =
        instructionsInput
        |> List.filter (fun x -> x <> "")
        |> List.map (fun x -> Regex.Matches(x, "[0-9]+"))
        |> List.ofSeq
        

    printfn "%0A" stacks
    printfn "%0A" instructions

    0
