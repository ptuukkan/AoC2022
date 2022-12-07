open System.IO

type File = { Name: string; Size: int }

type Directory =
    { Name: string
      Directories: Directory list
      Files: File list }

let executeCommands commands rootDir =
    rootDir
    

[<EntryPoint>]
let main args =
    let input = File.ReadLines(args[0])
    let rootDir =
        { Name = "/"
          Directories = []
          Files = [] }
    rootDir
    |> executeCommands input
        
     

    0
