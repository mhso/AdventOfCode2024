open System.IO

let days: ((string list -> int) list) list = [
    [Day1.part1; Day1.part2];
    [Day2.part1; Day2.part2];
    [Day3.part1; Day3.part2];
    [Day4.part1; Day4.part2];
    [Day5.part1; Day5.part2]
]

[<EntryPoint>]
let main args =
    let day = args[0] |> int
    let part = args[1] |> int

    let input = if args.Length > 2 && args[2] = "test" then "test" else "input"

    let lines = File.ReadLines($@"D:\AdventOfCode2024\Day{day}\{input}.txt") |> Seq.toList
    
    printf "%A" (days[day-1][part-1] lines)

    0
