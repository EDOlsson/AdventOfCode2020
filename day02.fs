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

let private evaluateEntry2 entry =
    let position1Matches = (entry.Password.Length >= entry.MinCount - 1) && (entry.Password.[entry.MinCount - 1] = entry.Letter)
    let position2Matches = (entry.Password.Length >= entry.MaxCount - 1) && (entry.Password.[entry.MaxCount - 1] = entry.Letter)

    (position1Matches && not position2Matches) || (not position1Matches && position2Matches)

let CalculatePart2 =
    let entries = readInput
                  |> Array.choose parseEntry
                  |> Array.map evaluateEntry2
                  |> Array.filter id
                  |> Array.length

    entries

let TestPart2 =
    let testCases = [|
        { MinCount = 1; MaxCount = 3; Letter = 'a'; Password = "abcde" }
        { MinCount = 1; MaxCount = 3; Letter = 'b'; Password = "cdefg" }
        { MinCount = 2; MaxCount = 9; Letter = 'c'; Password = "ccccccccc" } |]

    printfn "1-3 a: abcde is %s" <| if evaluateEntry2 testCases.[0] then "VALID" else "INVALID"
    printfn "1-3 b: cdefg is %s" <| if evaluateEntry2 testCases.[1] then "VALID" else "INVALID"
    printfn "2-9 c: ccccccccc is %s" <| if evaluateEntry2 testCases.[2] then "VALID" else "INVALID"
