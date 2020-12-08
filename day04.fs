module Day04

open MyRegEx

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

let private hasCategory values category =
        let entry = values |> Array.filter (fun v -> v.Category = category)
        let length = Array.length entry
        length > 0

let private validatePassport passport =
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

let private validatePassportPart2 passport =
    let validateYear minYear maxYear (year : string) =
        let (isValidNumber, yearValue) = System.Int32.TryParse(year)
        match (isValidNumber, yearValue) with
        | (false, _) -> false
        | (true, theYear) -> minYear <= theYear && theYear <= maxYear

    let isBirthYearValid = validateYear 1920 2002
    let isIssueYearValid = validateYear 2010 2020
    let isExpiryYearValid = validateYear 2020 2030

    let validateHeight height =
        let heightPattern = @"(\d+)(cm|in)"
        match height with
        | InterpretedMatch heightPattern [_; parsedHeight; units] ->
            let (isValidNumber, heightValue) = System.Int32.TryParse(parsedHeight.Value)
            if isValidNumber then
                if units.Value = "cm"
                then
                    150 <= heightValue && heightValue <= 193
                else
                    59 <= heightValue && heightValue <= 76
            else
                false
        | _ -> false

    let validateHairColor hairColor =
        let hairColorPattern = "#[0-9|a-f]{6}"
        match hairColor with
        | InterpretedMatch hairColorPattern [_] -> true
        | _ -> false

    let validateEyeColor eyeColor =
        match eyeColor with
        | "amb" -> true
        | "blu" -> true
        | "brn" -> true
        | "gry" -> true
        | "grn" -> true
        | "hzl" -> true
        | "oth" -> true
        | _ -> false
    
    let validatePassportId passportId =
        let passportIdPattern = "[0-9]{9}"
        match passportId with
        | InterpretedMatch passportIdPattern [ _ ] -> true
        | _ -> false

    let passportValue passport category =
        let value = passport.Values |> Array.filter (fun v -> v.Category = category) |> Array.head
        value.Value

    let passportHasCategory = hasCategory passport.Values

    let checkPassportCategory passport category validateCategory =
        let isValid = passportHasCategory category && validateCategory (passportValue passport category)
        if not isValid then printf "[X] %A" category else printf "."
        isValid

    let checkThisPassport = checkPassportCategory passport

    let printPassport passport =
        passport.Values
        |> Array.sortBy (fun p -> p.Category)
        |> Array.iter (fun p -> printf "%A: %s  " p.Category p.Value)

    printPassport passport

    let isPassportValid = checkThisPassport BirthYear isBirthYearValid &&
                          checkThisPassport IssueYear isIssueYearValid &&
                          checkThisPassport ExpireYear isExpiryYearValid &&
                          checkThisPassport Height validateHeight &&
                          checkThisPassport HairColor validateHairColor &&
                          checkThisPassport EyeColor validateEyeColor &&
                          checkThisPassport PassportId validatePassportId
    
    printfn ""

    isPassportValid

let CalculatePart2 =
    let combinedLines = combineInputLines readInput
    let passports = List.map parsePassport combinedLines

    passports
    |> List.map validatePassportPart2
    |> List.filter id
    |> List.length      // outputs 148, but 147 is the answer. Not sure where the off-by-one error is
