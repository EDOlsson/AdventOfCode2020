module Day01

let private readDay1Input =
    System.IO.File.ReadAllLines "./day-1-input.txt"

let CalculatePart1 =
    let input = readDay1Input
                |> Array.map (System.Int32.Parse)

    let winners = [ for lhs in input do
                         for rhs in input do
                            if (lhs + rhs) = 2020 then yield lhs * rhs ]
    
    List.head winners
