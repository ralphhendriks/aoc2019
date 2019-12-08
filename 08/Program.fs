open System
open System.IO

[<EntryPoint>]
let main _ =

    //let width = 3
    //let height = 2
    //let image = "123456789012"

    let width = 25
    let height = 6
    let pageSize = width * height
    let image = 
        File.ReadAllLines("input.txt")
        |> Array.exactlyOne

    let layers = Seq.chunkBySize pageSize image
    let answer1 =
        layers
        |> Seq.map (Seq.countBy (fun x -> x) >> Map.ofSeq)
        |> Seq.minBy (fun m -> m.['0'])
        |> (fun m -> m.['1'] * m.['2'])

    printfn "Answer 1: %i" answer1

    let mergeLayers front back =
        Array.map2 (fun frontPixel backPixel ->
            match (frontPixel, backPixel) with | ('2', b) -> b | (f, b) -> f) front back

    printfn "Answer 2:"
    Seq.foldBack mergeLayers layers (Array.init pageSize (fun _ -> '2'))
    |> Seq.chunkBySize width
    |> Seq.iter (fun line ->
        line
        |> Array.map (fun c -> match c with | '1' -> '*' | _ -> ' ')
        |> System.String
        |> printfn "%s")

    Console.ReadKey() |> ignore
    0 // return an integer exit code
