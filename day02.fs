module Day02

let private readInput =
    System.IO.File.ReadAllLines "./day-2-input.txt"

type private Entry =
    {
        MinCount: int
        MaxCount: int
        Letter: char
        Password: string
    }

let private (|InterpretedMatch|_|) pattern input =
    if isNull input then None
    else
        let m = System.Text.RegularExpressions.Regex.Match(input, pattern)
        if m.Success then Some [for x in m.Groups-> x]
        else None

let private parseEntry entry =
    let pattern = "(\d+)-(\d+) (\w): (\w+)"

    match entry with
    | InterpretedMatch pattern [_; minCount; maxCount; letter; password] ->
        Some({MinCount = System.Int32.Parse minCount.Value; MaxCount = System.Int32.Parse maxCount.Value; Letter = letter.Value.[0]; Password = password.Value })
    | _ ->
        None

let private evaluateEntry entry =
    let count = entry.Password
                |> Seq.filter (fun c -> c = entry.Letter)
                |> Seq.length
    
    entry.MinCount <= count && count <= entry.MaxCount

let CalculatePart1 =
    let entries = readInput
                  |> Array.choose parseEntry
                  |> Array.map evaluateEntry
                  |> Array.filter id
                  |> Array.length

    entries