module Day3

open System.Text.RegularExpressions

let rxMul: Regex = Regex(@"mul\((\d+)\,(\d+)\)", RegexOptions.Compiled)
let rxStart: Regex = Regex(@"do\(\)", RegexOptions.Compiled)
let rxStop: Regex = Regex(@"don\'t\(\)", RegexOptions.Compiled)

let getMatches (s: string) =
    let matches = rxMul.Matches s
    Seq.map (fun (m: Match) -> (m.Groups[1], m.Groups[2])) matches |> Seq.toList

let part1 (lines: string list) =
    let allLines = String.concat "" lines
    let matches = getMatches allLines
    printf "%d\n" (List.sum (List.map (fun (m1: Group, m2: Group) -> (m1.Value |> int) * (m2.Value |> int)) matches))

let getFlags (s: string) (r: Regex) =
    let matches = r.Matches s
    Seq.map (fun (m: Match) -> m.Groups[0]) matches |> Seq.toList


let rec nextFlag (words: (Group * bool) list) index =
    match words with
    | (g, b)::_ when g.Index < index -> b
    | (g, _)::t when g.Index >= index -> nextFlag t index
    | _ -> true


let rec getValue words (matches: list<Group * Group>) =
    match matches with
    | (g1, g2)::tail ->
        let v = if nextFlag words g1.Index then (g1.Value |> int) * (g2.Value |> int) else 0
        v + getValue words tail
    | [] -> 0


// 7267868 = too low
// 79985780 = too high
let part2 (lines: string list) =
    let allLines = String.concat "" lines
    let startWords = List.map (fun g -> (g, true)) (getFlags allLines rxStart)
    let stopWords = List.map (fun g -> (g, false)) (getFlags allLines rxStop)
    let words = List.sortBy (fun (g: Group, _) -> g.Index) (startWords@stopWords)

    let matches = List.rev (getMatches allLines)

    printf "%d\n" (getValue (List.rev words) matches)
