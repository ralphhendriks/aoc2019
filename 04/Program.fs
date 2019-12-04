open System

[<EntryPoint>]
let main _ =
    let digits n =
        let rec loop n =
            if n <= 0 then []
            else n%10 :: loop (n/10)
        List.rev (loop n)

    let increasing (pairs: (int * int) list) =
        Seq.forall (fun (x, y) -> x <= y) pairs

    let adjacentSame (pairs: (int * int) list) =
        Seq.exists (fun (x, y) -> x = y) pairs

    let adjacentSameOnlyGroupOfTwo (pairs: (int * int) list) =
        Seq.filter (fun (x, y) -> x = y) pairs
        |> Seq.countBy fst
        |> Seq.exists (fun (_, cnt) -> cnt = 1)

    let pairs =
        { 137683 .. 596253 }
        |> Seq.map digits
        |> Seq.map List.pairwise

    let answer1 =
        pairs
        |> Seq.filter (fun num -> (increasing num) && (adjacentSame num))
        |> Seq.length
    printfn "Answer 1: %i" answer1

    let answer2 =
        pairs
        |> Seq.filter (fun num -> (increasing num) && (adjacentSameOnlyGroupOfTwo num))
        |> Seq.length
    printfn "Answer 2: %i" answer2

    Console.ReadKey() |> ignore
    0 // return an integer exit code
