open System
open System.IO

let wire (code: string) =
    code.Split(',')
    |> Seq.map (fun x -> List.init (int x.[1..]) (fun _ ->
        match x.[0] with 
        | 'R' -> (1, 0)
        | 'U' -> (0, 1)
        | 'L' -> (-1, 0)
        | 'D' -> (0, -1)
        | _ -> invalidArg "dir" "Unknown direction"))
    |> List.concat
    |> List.scan (fun (x0, y0) (x1, y1) -> (x0 + x1, y0 + y1)) (0, 0)

[<EntryPoint>]
let main _ =
    //let wire1 = wire "R8,U5,L5,D3"
    //let wire2 = wire "U7,R6,D4,L4"

    let input = File.ReadAllLines("input.txt")
    let wire1 = wire input.[0]
    let wire2 = wire input.[1]
    let intersects = Set.intersect (Set.ofList wire1) (Set.ofList wire2) |> Set.toList

    let distances =
        intersects
        |> List.map (fun (x, y) -> abs x + abs y)
        |> List.sort
    printfn "Answer 1: %i" distances.[1]

    let lengths =
        intersects
        |> List.map (fun x ->
            List.findIndex (fun elem -> elem = x) wire1 + List.findIndex (fun elem -> elem = x) wire2)
        |> List.sort
    printfn "Answer 2: %i" lengths.[1]

    Console.ReadKey() |> ignore
    0 // return an integer exit code
