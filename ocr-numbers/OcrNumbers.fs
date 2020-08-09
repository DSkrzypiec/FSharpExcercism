module OcrNumbers

[<Literal>]
let OcrNumbersHight = 4
let OcrNumbersWidth = 3

// Input grid size validation.
let validOcrNumberInput (input : string list) : bool =
    let len = input |> List.length
    let ncharByRows = input |> List.map String.length
    let hightOK = len = OcrNumbersHight
    let widthMin = ncharByRows |> List.min
    let widthMax = ncharByRows |> List.max
    let widthOK = widthMin = widthMin && widthMin % OcrNumbersWidth = 0
    hightOK && widthOK

// Remapping OCR lines into integers.
let pipesAndUnderscoresToInt (str : string) = 
        str 
        |> Seq.toList
        |> List.mapi (
            fun i c -> 
                match c with
                | '|' -> pown 4 (i + 1)
                | '_' -> pown 5 (i + 1)
                | _ -> 0) 
        |> List.sum

// Single OCR number to int.
let ocrNumberToInt (str : string list) =
    let ocrNumberRowsToInt = str |> Seq.toArray |> Array.map pipesAndUnderscoresToInt
    match ocrNumberRowsToInt with
    | [|25; 68; 93; 0|] -> "0"
    | [|0; 64; 64; 0|]  -> "1"
    | [|25; 89; 29; 0|] -> "2"
    | [|25; 89; 89; 0|] -> "3"
    | [|0; 93; 64; 0|]  -> "4"
    | [|25; 29; 89; 0|] -> "5"
    | [|25; 29; 93; 0|] -> "6"
    | [|25; 64; 64; 0|] -> "7"
    | [|25; 93; 93; 0|] -> "8"
    | [|25; 93; 89; 0|] -> "9"
    | _ -> "?"

let splitSingleRow (str : string) : string list =
    str 
    |> Seq.toList
    |> List.chunkBySize OcrNumbersWidth
    |> List.map System.String.Concat

let splitIntoSingleOcr (str : string list) =
    if List.isEmpty str then
        failwith "Expected non-empty list"

    let splittedRows = str |> List.map splitSingleRow
    let numbers = splittedRows.[0] |> List.length
    [0 .. (numbers - 1)]
    |> List.map (fun i -> splittedRows |> List.map (fun e -> e.[i]))

let convert (input : string list) = 
    match validOcrNumberInput input with
    | false -> None
    | true -> 
        input 
        |> splitIntoSingleOcr 
        |> List.map ocrNumberToInt
        |> System.String.Concat
        |> Some
     