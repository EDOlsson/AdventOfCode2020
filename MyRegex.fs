module MyRegEx

let (|InterpretedMatch|_|) pattern input =
    if isNull input then None
    else
        let m = System.Text.RegularExpressions.Regex.Match(input, pattern)
        if m.Success then Some [for x in m.Groups-> x]
        else None
