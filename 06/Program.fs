open System
open System.IO

[<EntryPoint>]
let main _ =
    let directOrbits = 
        File.ReadAllLines("input.txt")
        |> Seq.map (fun line -> line.Split(')') |> function [|a; b|] -> b, a | _ -> failwith "Invalid array length")
        |> Map.ofSeq

    let rec allOrbits = function
    | "COM" -> []
    | x -> directOrbits.[x] :: (allOrbits directOrbits.[x])

    let totalOrbits =
        directOrbits
        |> Map.toSeq
        |> Seq.map (fun (k, _) -> allOrbits k |> List.length)
        |> Seq.sum

    printfn "Answer 1: %i" totalOrbits

    let allSanOrbits = allOrbits "SAN"
    let allYouOrbits = allOrbits "YOU"

    let rec orbitDistance findIndex = function
    | [] -> failwith "No path found"
    | v::vs ->
        match (List.tryFindIndex (fun x -> x = v) allSanOrbits) with
        | None -> orbitDistance (findIndex + 1) vs
        | Some dist -> findIndex + dist

    printfn "Answer 2: %i" (orbitDistance 0 allYouOrbits)

    Console.ReadKey() |> ignore
    0 // return an integer exit code
