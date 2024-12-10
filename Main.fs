open System.IO

let days: ((string list -> unit) list) list = [
    [Day1.part1; Day1.part2];
    [Day2.part1; Day2.part2];
    [Day3.part1; Day3.part2];
    [Day4.part1; Day4.part2];
    [Day5.part1; Day5.part2];
    [Day6.part1; Day6.part2];
    [Day7.part1; Day7.part2];
    [Day8.part1; Day8.part2];
    [Day9.part1; Day9.part2];
    [Day10.part1; Day10.part2]
]

[<EntryPoint>]
let main args =
    let day = args[0] |> int
    let part = args[1] |> int

    let input = if args.Length > 2 && args[2] = "test" then "test" else "input"

    let lines = File.ReadLines($@"D:\AdventOfCode2024\Day{day}\{input}.txt") |> Seq.toList
    
    days[day-1][part-1] lines

    0
