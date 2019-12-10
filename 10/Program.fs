open System
open System.IO

[<EntryPoint>]
let main _ =

    let asteroids =
        File.ReadLines("input.txt")
        |> Seq.mapi (fun y line -> Seq.mapi (fun x c -> match c with | '#' -> Some (x, y) | _ -> None) line)
        |> Seq.concat
        |> Seq.choose (fun x -> x)
        |> Seq.toList

    let remove e = List.filter(fun x -> x <> e)

    let dot (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

    let cross (x1, y1) (x2, y2) = x1 * y2 - y1 * x2

    let twoPi = 2.0 * Math.PI

    let angle v1 v2 = (Math.Atan2(float (cross v1 v2), float (dot v1 v2)) + twoPi) % twoPi

    let angleToAxis (sx, sy) (x, y) = angle (0, -1) (x - sx, y - sy)

    // looking from asteroid c to asteroid 1, is the sight blocked by asteroid 2?
    let isBlockedBy (xc, yc) (x1, y1) (x2, y2) =
        let dx1 = x1 - xc
        let dy1 = y1 - yc
        let dx2 = x2 - xc
        let dy2 = y2 - yc
        let cross = dx1 * dy2 - dy1 * dx2
        match (cross, abs dx1, abs dy1) with
        | 0, dx, dy when dx >= dy ->
            match xc, x1, x2 with
            | vc, v1, v2 when vc > v1 && vc > v2 -> true
            | vc, v1, v2 when vc < v1 && vc < v2 -> true
            | _ -> false
        | 0, _, _ ->
            match yc, y1, y2 with
            | vc, v1, v2 when vc > v1 && vc > v2 -> true
            | vc, v1, v2 when vc < v1 && vc < v2 -> true
            | _ -> false
        | _ -> false

    let rec visibility ref (visible, invisible) asteroids =
        match asteroids, visible, invisible with
        | [], v, i -> (v, i)
        | ah::at, v, i when (List.exists (fun a -> (a <> ah) && (isBlockedBy ref ah a)) asteroids) -> visibility ref (v, ah::i) at
        | ah::at, v, i -> visibility ref (ah::v, i) at

    let station =
        asteroids
        |> List.map (fun a -> (a, (visibility a ([], []) (remove a asteroids)) |> fst |> List.length))
        |> List.maxBy snd

    printfn "Answer 1: %i" (snd station)

    let rec visibilityRings ref rings asteroids =
        match (visibility ref ([], []) asteroids) with
        | v, [] -> v::rings
        | v, i -> visibilityRings ref (v::rings) i

    asteroids
    |> remove (fst station)
    |> visibilityRings (fst station) []
    |> List.rev
    |> List.map (List.map (fun a -> (a, angleToAxis (fst station) a)) >> List.sortBy snd)
    |> List.concat
    |> List.item 199
    |> fst
    |> fun a -> printfn "Answer 2: %i" ((fst a) * 100 + (snd a))

    Console.ReadKey() |> ignore
    0 // return an integer exit code
