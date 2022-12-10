open System.IO
open System

let follow tail head =
    match head, tail with
    | _ when head > tail -> tail + 1
    | _ when head < tail -> tail - 1
    | _ -> tail

let moveHead (motion: string) (head: int * int) : int * int =
    let x, y = head

    match motion with
    | "U" -> (x, y + 1)
    | "R" -> (x + 1, y)
    | "D" -> (x, y - 1)
    | _ -> (x - 1, y)

let moveTail (head: int * int) (tail: int * int) : int * int =
    let headx, heady = head
    let tailx, taily = tail
    let deltax = abs (headx - tailx)
    let deltay = abs (heady - taily)

    match deltax, deltay with
    | 2, _ -> follow tailx headx, follow taily heady
    | _, 2 -> follow tailx headx, follow taily heady
    | _ -> tail


let simulateRope (rope: (int * int) list * (int * int) list) (motion: string) =
    let knots, tailPostitions = rope

    let moveKnot (rope: (int * int) list) (knot: int * int) =
        match rope |> List.tryLast with
        | None -> [ moveHead motion knot ]
        | Some head -> rope @ [ moveTail head knot ]

    let newRope = knots |> List.fold moveKnot []

    (newRope, (newRope |> List.last) :: tailPostitions)

let rope knots =
    [ 1..knots ] |> List.map (fun _ -> 0, 0)

let spreadMotions (motion: string) =
    let split = motion.Split(' ')
    [ 1 .. Int32.Parse(split[1]) ] |> List.map (fun _ -> split[0])

[<EntryPoint>]
let main args =
    let motions = File.ReadLines args[0] |> List.ofSeq |> List.collect spreadMotions

    let part1 =
        motions
        |> Seq.fold simulateRope ((rope 2), ([ 0, 0 ]))
        |> fun x -> snd x
        |> Seq.distinct
        |> Seq.length

    let part2 =
        motions
        |> Seq.fold simulateRope ((rope 10), ([ 0, 0 ]))
        |> fun x -> snd x
        |> Seq.distinct
        |> Seq.length

    printfn "%d" part1
    printfn "%d" part2
    0
