module Day03

let private readInput =
    System.IO.File.ReadAllLines "./day-3-input.txt"

type private Location = {
    X : int
    Y : int
}

type private TobogganAngle = {
    HorizontalDelta : int
    VerticalDelta : int
}

let private evaulatePosition (course : string[]) location =
    let row = course.[location.Y]

    let column = location.X % row.Length
    row.[column]

let private advancePosition angle oldLocation =
    { oldLocation with X = oldLocation.X + angle.HorizontalDelta; Y = oldLocation.Y + angle.VerticalDelta }

let CalculatePart1 =
    let course = readInput

    let angle = { HorizontalDelta = 3; VerticalDelta = 1 }

    let positions = [ for row in 1..course.Length - 1 -> { X = angle.HorizontalDelta * row; Y = angle.VerticalDelta * row } ]

    positions
    |> List.map (fun pos -> evaulatePosition course pos)
    |> List.filter (fun c -> c = '#')
    |> List.length