open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

let splitInput input =
    let index = input |> List.findIndex (fun elem -> elem = "")
    input |> List.splitAt index

let createStacks (input: string list) =
    let stackRow = input |> List.head
    let stacksCount = stackRow.Substring(stackRow.Length - 2, 1) |> int
    let initialValue = [1..stacksCount] |> List.map (fun x -> Stack<char>())
    let foldFunction (stacks: Stack<char> list) (elem: string) =
        let push (stack: Stack<char>) e =
            if e <> ' ' then stack.Push(e)
        [0..stacksCount - 1]
        |> List.iter (fun x -> push stacks[x] elem[x * 4 + 1])
        stacks
    input
    |> List.tail
    |> List.fold foldFunction initialValue

let arrangeOneByOne (stacks: Stack<char> list) (instructions: Match list) =
    let amount = int instructions[0].Value
    let source = int instructions[1].Value - 1
    let destination = int instructions[2].Value - 1
    [1..amount] |> List.iter (fun x -> stacks[destination].Push(stacks[source].Pop()))

let arrangeMultiple (stacks: Stack<char> list) (instructions: Match list) =
    let amount = int instructions[0].Value
    let source = int instructions[1].Value - 1
    let destination = int instructions[2].Value - 1
    let temp = Stack<char>()
    [1..amount] |> List.iter (fun x -> temp.Push(stacks[source].Pop()))
    [1..amount] |> List.iter (fun x -> stacks[destination].Push(temp.Pop()))
    
[<EntryPoint>]
let main args =
    let input = File.ReadLines args[0]
    let stacksInput, instructionsInput =
        input
        |> List.ofSeq
        |> splitInput
    let instructions =
        instructionsInput
        |> List.filter (fun x -> x <> "")
        |> List.map (fun x -> Regex.Matches(x, "[0-9]+"))
        |> List.map List.ofSeq
    let stacks1 =
        stacksInput 
        |> List.rev
        |> createStacks
    let stacks2 =
        stacksInput 
        |> List.rev
        |> createStacks       

    instructions
        |> List.iter (arrangeOneByOne stacks1)
    instructions
        |> List.iter (arrangeMultiple stacks2)
     
    let part1 = stacks1 |> List.map (fun x -> x.Peek())
    let part2 = stacks2 |> List.map (fun x -> x.Peek())
        
    printfn $"part1: %0A{part1}"
    printfn $"part1: %0A{part2}"
    0
