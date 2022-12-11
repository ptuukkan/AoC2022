open System.IO
open System

type Monkey =
    { id: int
      items: bigint array
      op: bigint -> bigint
      postOp: bigint -> bigint
      test: bigint -> int
      inspectionCount: bigint }

let parseOperation (input: string) =
    let split = input.Trim().Split(" ")

    let op =
        match split[1] with
        | "*" -> Operators.(*)
        | _ -> Operators.(+)

    fun (x: bigint) ->
        op
            x
            (match split[2] with
             | "old" -> x
             | i -> Int32.Parse(i) |> bigint)

let parseTest (input: string array) =
    let last (s: string) =
        Int32.Parse(s.Split(" ") |> Array.last) |> bigint

    let operand = last input[0]
    let trueDestination = last input[1]
    let falseDestination = last input[2]

    fun x ->
        match x % operand = bigint 0 with
        | true -> int trueDestination
        | false -> int falseDestination

let parseMonkey (index: int) (input: string array) (postOp: bigint -> bigint) =
    let items =
        input[ 1 ].Split(":")[1]
        |> fun x -> x.Split(",")
        |> Array.map (fun x -> Int32.Parse(x) |> bigint)

    let op = input[ 2 ].Split("=")[1] |> parseOperation
    let test = input[3..5] |> parseTest

    { id = index
      items = items
      op = op
      postOp = postOp
      test = test
      inspectionCount = bigint 0 }

let turn (monkeys: Monkey array) (monkey: Monkey) =
    let throwItem (monkeys: Monkey array) (item: bigint) =
        let newWorryLevel = item |> monkey.op |> monkey.postOp
        let destinationMonkey = newWorryLevel |> monkey.test

        let newMonkeys =
            monkeys
            |> Array.updateAt
                destinationMonkey
                { monkeys[destinationMonkey] with
                    items = ([| newWorryLevel |] |> Array.append monkeys[destinationMonkey].items) }
            |> Array.updateAt
                monkey.id
                { monkeys[monkey.id] with
                    items =
                        match monkeys[monkey.id].items.Length with
                        | 0
                        | 1 -> [||]
                        | _ -> monkeys[monkey.id].items |> Array.tail
                    inspectionCount = monkeys[monkey.id].inspectionCount + bigint 1 }

        newMonkeys

    monkeys[monkey.id].items |> Array.fold throwItem monkeys

let round (monkeys: Monkey array) _ = monkeys |> Array.fold turn monkeys

[<EntryPoint>]
let main args =
    let input =
        File.ReadAllText args[0]
        |> fun x -> x.Split("\r\n\r\n")
        |> Array.map (fun x -> x.Split("\r\n"))

    let monkeys1 =
        input
        |> Array.mapi (fun i x -> parseMonkey i x (fun (x: bigint) -> x / bigint 3))

    let monkeys2 =
        input
        |> Array.mapi (fun i x -> parseMonkey i x (fun (x: bigint) -> x % bigint 9699690))  //cheat

    [ 1..20 ]
    |> List.fold round monkeys1
    |> Array.sortByDescending (fun x -> x.inspectionCount)
    |> Array.take 2
    |> fun x -> x[0].inspectionCount * x[1].inspectionCount
    |> printfn "%A"

    [ 1..10000 ]
    |> List.fold round monkeys2
    |> Array.sortByDescending (fun x -> x.inspectionCount)
    |> Array.take 2
    |> fun x -> x[0].inspectionCount * x[1].inspectionCount
    |> printfn "%A"

    0
