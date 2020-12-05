module Day04

type private PassportCategory =
    | BirthYear
    | IssueYear
    | ExpireYear
    | Height
    | HairColor
    | EyeColor
    | PassportId
    | CountryId

type private ParsedPassportValue = {
    Category : PassportCategory
    Value : string
}

type private Passport = {
    Values : ParsedPassportValue[] 
    }

let private readInput =
    let lines = System.IO.File.ReadAllLines "./day-4-input.txt"

    let numberOfBlankLines = lines
                             |> Array.filter (fun ln -> ln.Length = 0)
                             |> Array.length
    printfn "There are %d blank lines in the file" numberOfBlankLines

    lines
    
let private combineInputLines (input : string[]) =
    let mutable currentPassport = ""
    [ for line in (Array.concat (seq { input; [|""|] })) do
        if line.Length > 0
        then
            currentPassport <- sprintf "%s %s" currentPassport line
        else
            yield currentPassport.Trim()
            currentPassport <- "" ]


let private parsePassport ( passportEntry : string ) =
    let parsePassportCategory (categoryValuePair : string) =
        let pairs = categoryValuePair.Split([|":"|], System.StringSplitOptions.None)
        match pairs with
        | [| "byr"; v |] -> { Category = BirthYear; Value = v }
        | [| "iyr"; v |] -> { Category = IssueYear; Value = v }
        | [| "eyr"; v |] -> { Category = ExpireYear; Value = v }
        | [| "hgt"; v |] -> { Category = Height; Value = v }
        | [| "hcl"; v |] -> { Category = HairColor; Value = v }
        | [| "ecl"; v |] -> { Category = EyeColor; Value = v }
        | [| "pid"; v |] -> { Category = PassportId; Value = v }
        | [| "cid"; v |] -> { Category = CountryId; Value = v }
        | _ -> failwith <| sprintf "Unknown category %s" pairs.[0]
        
    let entries = passportEntry.Split([|" "|], System.StringSplitOptions.RemoveEmptyEntries)
                    |> Array.map parsePassportCategory
    { Values = entries }

let private validatePassport passport =
    let hasCategory values category =
        let entry = values |> Array.filter (fun v -> v.Category = category)
        let length = Array.length entry
        length > 0
    
    let hasBirthYear = hasCategory passport.Values BirthYear
    let hasIssueYear = hasCategory passport.Values IssueYear
    let hasExpiryYear = hasCategory passport.Values ExpireYear
    let hasHeight = hasCategory passport.Values Height
    let hasHairColor = hasCategory passport.Values HairColor
    let hasEyeColor = hasCategory passport.Values EyeColor
    let hasPassportId = hasCategory passport.Values PassportId

    hasBirthYear && hasIssueYear && hasExpiryYear && hasHeight && hasHairColor && hasEyeColor && hasPassportId

let CalculatePart1 =
    let combinedLines = combineInputLines readInput
    let passports = List.map parsePassport combinedLines

    passports
    |> List.map validatePassport
    |> List.filter id
    |> List.length

let TestPart1 =
    let testData = [| "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd"
                      "byr:1937 iyr:2017 cid:147 hgt:183cm"
                      ""
                      "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884"
                      "hcl:#cfa07d byr:1929"
                      ""
                      "hcl:#ae17e1 iyr:2013"
                      "eyr:2024"
                      "ecl:brn pid:760753108 byr:1931"
                      "hgt:179cm"
                      ""
                      "hcl:#cfa07d eyr:2025 pid:166559648"
                      "iyr:2011 ecl:brn hgt:59in" |]

    let testLines = combineInputLines testData
    let passports = List.map parsePassport testLines
    if List.length passports <> 4 then failwith <| sprintf "Expected 4 passports to be parsed but found %d instead" (List.length passports)

    passports
    |> List.map validatePassport
    |> List.filter id
    |> List.length
