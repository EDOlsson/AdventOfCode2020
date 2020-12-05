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
    let spot = row.[column]

    spot

let private evaluateCourseAtAngle (course : string[]) angle =
    let positions = [ for i in 0 .. course.Length -> { X = angle.HorizontalDelta * i; Y = angle.VerticalDelta * i } ]

    positions
    |> List.takeWhile (fun pos -> pos.Y < course.Length)
    |> List.map (fun pos -> evaulatePosition course pos)
    |> List.filter (fun c -> c = '#')
    |> List.length

let CalculatePart1 =
    let course = readInput

    let angle = { HorizontalDelta = 3; VerticalDelta = 1 }

    evaluateCourseAtAngle course angle

let CalculatePart2 =
    let course = readInput
    let angles = [ { HorizontalDelta = 1; VerticalDelta = 1 }
                   { HorizontalDelta = 3; VerticalDelta = 1 }
                   { HorizontalDelta = 5; VerticalDelta = 1 }
                   { HorizontalDelta = 7; VerticalDelta = 1 }
                   { HorizontalDelta = 1; VerticalDelta = 2 } ]

    (List.map ((fun a -> evaluateCourseAtAngle course a) >> (fun count -> (int64 count))) angles)
    |> List.reduce ( * )

let TestPart2 =
    let course = [|"..##......."
                   "#...#...#.."
                   ".#....#..#."
                   "..#.#...#.#"
                   ".#...##..#."
                   "..#.##....."
                   ".#.#.#....#"
                   ".#........#"
                   "#.##...#..."
                   "#...##....#"
                   ".#..#...#.#" |]

    let angles = [ { HorizontalDelta = 1; VerticalDelta = 1 }
                   { HorizontalDelta = 3; VerticalDelta = 1 }
                   { HorizontalDelta = 5; VerticalDelta = 1 }
                   { HorizontalDelta = 7; VerticalDelta = 1 }
                   { HorizontalDelta = 1; VerticalDelta = 2 } ]

    angles
    |> List.map (fun a -> evaluateCourseAtAngle course a)
    |> List.reduce ( * )