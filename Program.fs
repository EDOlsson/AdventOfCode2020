// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open Day01

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

[<EntryPoint>]
let main argv =
    let day1Part1 = Day01.CalculatePart1

    printfn "Day 1 part 1 : %d" day1Part1

    0