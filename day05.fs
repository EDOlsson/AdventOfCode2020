module Day05

let private readInput =
    System.IO.File.ReadAllLines "./day-5-input.txt"

type private EncodedSeat = {
    EncodedRow : string
    EncodedCol : string
}

type private DecodedSeat = {
    Row : int
    Col : int
}

let private parseInput input =
    let parseSingleSeat entry =
        let pattern = @"([FB]{7})([RL]{3})"
        match entry with
        | MyRegEx.InterpretedMatch pattern [_; row; col] -> Some { EncodedRow = row.Value; EncodedCol = col.Value }
        | _ -> None

    input
    |> Array.choose parseSingleSeat

let private decodeSeat seat =
    let decodeRow (encodedRow : string) =
        let stringifiedBinary = encodedRow.Replace('F', '0').Replace('B', '1')
        System.Convert.ToInt32(stringifiedBinary, 2)

    let decodeCol (encodedCol : string) =
        let stringifiedBinary = encodedCol.Replace('R', '1').Replace('L', '0')
        System.Convert.ToInt32(stringifiedBinary, 2)

    { Row = decodeRow seat.EncodedRow; Col = decodeCol seat.EncodedCol }

let private calculateSeatId decodedSeat =
    decodedSeat.Row * 8 + decodedSeat.Col

let CalculatePart1 =
    (Array.map (decodeSeat >> calculateSeatId) (parseInput readInput))
    |> Array.max