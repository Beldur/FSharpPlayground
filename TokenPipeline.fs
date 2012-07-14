open System
open System.IO
open System.Text.RegularExpressions

type Token = (string * string)
type TokenList = (Token * int) list
let blackList = ["to"; "the"; "of"; "a"; "and"; "it"; "i"; "or"; "no"; "is"; "in"; "are"]

/// Returns a Token list for a word input list
let getTokenStream (input : string list) : Token list =
    let filterNthElements div rem =
        input
            |> List.mapi (fun index elem -> index + 1, elem)
            |> List.choose (fun (i, v) -> if i % div = rem then Some v else None)
    
    // Use Seq here to ignore unequal list length
    Seq.zip (filterNthElements 5 2) (filterNthElements 5 0) |> Seq.toList

/// Convert an input string to a list of words and remove any non alpha numeric value
let getWordList input =
    Regex.Replace(input, "[^a-zA-Z0-9 ]", "").Trim().ToLower().Split(' ')
    |> Array.toList
    |> List.filter (fun elem -> not (List.exists (fun el -> el = elem) blackList))

/// Get token list for input string
let getTokens input =
    let wordList = getWordList input

    let rec search wordList acc =
        match wordList with
        | _::first::_::_::second::_ ->
            let value = (first, second)
            
            // If value already exists, we increase its counter by 1
            if Set.exists (fun elem -> fst elem = value) acc then
                let acc = Set.map (fun elem ->
                    if fst elem = value then (fst elem, (snd elem) + 1)
                    else elem) acc
                search wordList.Tail acc
            else
                search wordList.Tail (Set.add (value, 1) acc)
        | _ -> acc
    search wordList Set.empty<(Token * int)>
    |> Set.toList

/// Converts a file with words to a list of parsed tokens
let getTokenList file : TokenList =
    File.ReadLines(file)
        |> Seq.fold (fun lst line ->
            lst @ getTokens line
        ) []

/// Reads a directory and returns a Label Map with corresponding word tokens
let readInput directory : Map<string, TokenList> =
    Directory.EnumerateFiles(directory)
        |> Seq.fold (fun acc file ->
            let filename = Path.GetFileNameWithoutExtension file
            acc.Add(filename, getTokenList file)
        ) Map.empty<string, TokenList>
    
/// Compare two Token lists
let compare (left:TokenList) (right:TokenList) : float =
    (*let leftSum  = left  |> List.fold (fun acc elem -> acc + snd elem) 0
    let rightSum = right |> List.fold (fun acc elem -> acc + snd elem) 0*)
    let findElem list item = list |> List.tryFind (fun elem -> fst elem = item)

    let counter = left |> List.fold (fun acc elem ->
        let value = fst elem
        match findElem right value with
        | Some v ->
            let leftCounter = float(snd elem)
            let rightCounter = float(snd v)
            let calcValue = if leftCounter > rightCounter then rightCounter / leftCounter else leftCounter / rightCounter

            printfn "Found %A in right %A calcValue %A" elem v calcValue
            acc + calcValue
        | _ -> acc) 0.0

    printfn "counter %A" counter
    counter

let labelMap = readInput "Input"
//printfn "%A" labelMap

let input = getTokens "But I must explain to you how all this mistaken idea of denouncing pleasure and praising pain was born and I will give you a complete account of the system, and expound the actual teachings of the great explorer of the truth, the master-builder of human happiness. No one rejects, dislikes, or avoids pleasure itself, because it is pleasure, but because those who do not know how to pursue pleasure rationally encounter consequences that are extremely painful."
//let input = getTokens "how all this mistaken idea how all this mistaken idea"
let res = compare input (labelMap.Item "English")


Console.ReadLine() |> ignore