open System.IO

type File = { Name: string; Size: int }

type Directory =
    { Name: string
      Directories: Directory list
      Files: File list }

type DirectoryTotalSize =
    { Name: string
      TotalSize: int
      Directories: DirectoryTotalSize list }

let dirToList (dir: Directory) : Directory list = [ dir ]

let sumCurrentDirFileSizes (files: File list) : int =
    files |> List.map (fun x -> x.Size) |> List.sum

let rec calculateDirSize (currentDir: Directory) : DirectoryTotalSize =
    match currentDir.Directories with
    | a when a.Length = 0 ->
        { Name = currentDir.Name
          TotalSize = currentDir.Files |> sumCurrentDirFileSizes
          Directories = [] }
    | _ ->
        let contents = currentDir.Directories |> List.map calculateDirSize

        let totalSize =
            contents
            |> List.map (fun x -> x.TotalSize)
            |> List.sum
            |> (+) (currentDir.Files |> sumCurrentDirFileSizes)

        { Name = currentDir.Name
          TotalSize = totalSize
          Directories = contents }

let addDirectory (currentDir: Directory) (name: string) : Directory =
    match currentDir.Directories |> List.tryFind (fun x -> x.Name = name) with
    | None ->
        { currentDir with
            Directories =
                currentDir.Directories
                |> List.append (
                    [ { Name = name
                        Directories = []
                        Files = [] } ]
                ) }
    | Some _ -> currentDir

let addFile (currentDir: Directory) (name: string) (size: int) : Directory =
    match currentDir.Files |> List.tryFind (fun x -> x.Name = name) with
    | None -> { currentDir with Files = currentDir.Files |> List.append ([ { Name = name; Size = size } ]) }
    | Some _ -> currentDir

let parseOutput (output: string seq) (rootDir: Directory) : Directory =
    let mutable outputNumber = 0

    let rec parseLine (rootDir: Directory) : Directory =
        let mutable currentDir = rootDir
        let mutable continueLooping = true

        while continueLooping do
            currentDir <-
                match Seq.tryItem outputNumber output with
                | None ->
                    continueLooping <- false
                    currentDir
                | Some line ->
                    outputNumber <- outputNumber + 1

                    match line.Split(' ') with
                    | command when command[0] = "$" ->
                        match command[1] with
                        | _ when command[1] = "cd" && command[2] = ".." ->
                            continueLooping <- false
                            currentDir
                        | _ when command[1] = "cd" && command[2] <> "/" ->
                            let subdir =
                                currentDir.Directories |> List.find (fun x -> x.Name = command[2]) |> parseLine

                            { currentDir with
                                Directories =
                                    currentDir.Directories
                                    |> List.filter (fun x -> x.Name <> subdir.Name)
                                    |> List.append (dirToList subdir) }
                        | _ -> currentDir
                    | dir when dir[0] = "dir" -> addDirectory currentDir dir[1]
                    | file -> addFile currentDir file[1] (int file[0])

        currentDir

    parseLine rootDir

let rec flatten (currentDir: DirectoryTotalSize) =
    seq {
        yield currentDir

        for dir in currentDir.Directories do
            yield! flatten dir
    }


[<EntryPoint>]
let main args =
    let commands = File.ReadLines(args[0])

    let rootDir =
        { Name = "/"
          Directories = []
          Files = [] }

    let part1 =
        rootDir
        |> parseOutput commands
        |> calculateDirSize
        |> flatten
        |> Seq.filter (fun x -> x.TotalSize < 100000)
        |> Seq.map (fun x -> x.TotalSize)
        |> Seq.sum

    printfn "%d" part1

    let dirs = rootDir |> parseOutput commands |> calculateDirSize
    let spaceNeeded = 30000000 - (70000000 - dirs.TotalSize)

    let part2 =
        dirs
        |> flatten
        |> Seq.filter (fun x -> x.TotalSize > spaceNeeded)
        |> Seq.map (fun x -> x.TotalSize)
        |> Seq.sort
        |> Seq.head

    printfn "%d" part2
    0
