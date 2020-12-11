module Day06

let private readAndParseInput =
    let rec readGroup (reader : System.IO.StreamReader) questions =
        if isNull reader then questions else
            match (reader.ReadLine(), reader.EndOfStream) with
            | (x, false) when x.Length > 0 -> readGroup reader (x :: questions)
            | (x, true) when x.Length > 0 -> x :: questions
            | (_, _) -> questions

    use reader = new System.IO.StreamReader("./day-6-input.txt")

    let s = seq {
        while not reader.EndOfStream do
            yield readGroup reader []
        }

    Seq.toList s

let private coalesceGroupAnswers (answerGroup : string list) =
    let createSet (answers : string) =
        answers
        |> Seq.fold (fun letters c -> Set.add c letters) Set.empty<char>

    answerGroup
    |> List.fold (fun letters answer -> Set.union letters (createSet answer)) Set.empty<char>

let CalculatePart1 =
    readAndParseInput
    |> List.map coalesceGroupAnswers
    |> List.sumBy (fun s -> s.Count)

let coalesceGroupAnswers2 (answerGroup : string list) =
    let createSet (answers : string) =
        answers
        |> Seq.fold (fun letters c -> Set.add c letters) Set.empty<char>

    answerGroup
    |> List.map createSet
    |> List.reduce (Set.intersect)

let CalculatePart2 =
    readAndParseInput
    |> List.map coalesceGroupAnswers2
    |> List.sumBy (fun s -> s.Count)