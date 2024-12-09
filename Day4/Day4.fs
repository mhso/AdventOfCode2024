module Day4

open System.Text.RegularExpressions

let xmasRe1: Regex = Regex(@"M.S.A.M.S", RegexOptions.Compiled)
let xmasRe2: Regex = Regex(@"M.M.A.S.S", RegexOptions.Compiled)
let xmasRe3: Regex = Regex(@"S.S.A.M.M", RegexOptions.Compiled)
let xmasRe4: Regex = Regex(@"S.M.A.S.M", RegexOptions.Compiled)

let regexes = [xmasRe1; xmasRe2; xmasRe3; xmasRe4]

let count (s: char list) =
    List.sumBy (fun (s: char list) -> if System.String(List.toArray s) = "XMAS" then 1 else 0) (List.windowed 4 s)

let revList (l: list<list<char>>) =
    List.map List.rev l

let part1 (lines: string list) =
    let w = lines[0].Length
    let h = lines.Length
    let charArr = List.map (fun (s: string) -> s.ToCharArray()) lines
    let arr = Array2D.init h w (fun x y -> charArr[y][x])

    let diag1_x = seq {
        for start_x in [0..(Array2D.length1 arr) - 1] do
            seq {
                for i in [0..min ((Array2D.length1 arr) - start_x - 1) ((Array2D.length2 arr) - 1)] do 
                yield arr[start_x + i, i]
            }
    }
    let diag1_y = seq {
        for start_y in [1..(Array2D.length2 arr) - 1] do
            seq {
                for i in [0..min ((Array2D.length1 arr) - 1) ((Array2D.length2 arr) - start_y - 1)] do 
                yield arr[i, start_y + i]
            }
    }

    let diag2_x = seq {
        for start_x in [0..(Array2D.length1 arr) - 1] do
            seq {
                for i in [0..min ((Array2D.length1 arr) - start_x - 1) ((Array2D.length2 arr) - 1)] do 
                yield arr[Array2D.length1 arr - (start_x + i) - 1, i]
            }
    }
    let diag2_y = seq {
        for start_y in [1..(Array2D.length2 arr) - 1] do
            seq {
                for i in [0..min ((Array2D.length1 arr) - 1) ((Array2D.length2 arr) - start_y - 1)] do 
                yield arr[Array2D.length1 arr - i - 1, start_y + i]
            }
    }

    let diag1_x_l = Seq.toList (Seq.map Seq.toList diag1_x)
    let diag1_y_l = Seq.toList (Seq.map Seq.toList diag1_y)
    let diag2_x_l = Seq.toList (Seq.map Seq.toList diag2_x)
    let diag2_y_l = Seq.toList (Seq.map Seq.toList diag2_y)

    let diagSequences = (
        diag1_x_l @ (revList diag1_x_l) @ diag1_y_l @ (revList diag1_y_l) @ diag2_x_l @ (revList diag2_x_l) @ diag2_y_l @ (revList diag2_y_l)
    )

    let horizontal = seq {
        for y in [0..(Array2D.length2 arr) - 1] do
            seq {
                for x in [0..Array2D.length1 arr - 1] do 
                yield arr[x, y]
            }
    }
    let horizontal_l = Seq.toList (Seq.map Seq.toList horizontal)
    let horizontalSeq = horizontal_l @ (revList horizontal_l)

    let vertical = seq {
        for x in [0..(Array2D.length1 arr) - 1] do
            seq {
                for y in [0..Array2D.length2 arr - 1] do 
                yield arr[x, y]
            }
    }
    let vertical_l = Seq.toList (Seq.map Seq.toList vertical)
    let verticalSeq = vertical_l @ (revList vertical_l)

    let allSeq = diagSequences @ horizontalSeq @ verticalSeq

    printf "%d\n" (List.sumBy count allSeq)

let matchPart (a: char array, b: char array,  c: char array) =
    let s = System.String(a) + System.String(b) + System.String(c)
    List.sumBy (fun (re: Regex) -> if re.IsMatch(s) then 1 else 0) regexes

let countPart (l: char array list) =
    let w1 = Array.windowed 3 l[0]
    let w2 = Array.windowed 3 l[1]
    let w3 = Array.windowed 3 l[2]

    Array.sumBy matchPart (Array.zip3 w1 w2 w3)

let count2d (s: char array list) =
    List.sumBy (fun (s: char array list) -> countPart s) (List.windowed 3 s)

let part2 (lines: string list) =
    let w = lines[0].Length
    let h = lines.Length
    let charArr = List.map (fun (s: string) -> s.ToCharArray()) lines

    printf "%d\n" (List.sumBy count2d (List.windowed 3 charArr))
