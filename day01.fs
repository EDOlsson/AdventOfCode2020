module Day01

let private readDay1Input =
    System.IO.File.ReadAllLines "./day-1-input.txt"
    |> Array.map System.Int32.Parse

let CalculatePart1 =
    let input = readDay1Input

    let winners = [ for lhs in input do
                         for rhs in input do
                            if (lhs + rhs) = 2020 then yield lhs * rhs ]
    
    List.head winners

let CalculatePart2 =
    let input = readDay1Input

    let winners = [ for e1 in input do
                        for e2 in input do
                            for e3 in input do
                                if (e1 + e2 + e3) = 2020 then yield e1 * e2 * e3 ]

    List.head winners
