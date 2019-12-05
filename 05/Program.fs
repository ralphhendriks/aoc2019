open System
open System.IO

type state = Running | Finished
type parameterMode = Position | Immediate

[<EntryPoint>]
let main _ =
    let cycle ip outputs (mem: int array) input =
        let opCode = mem.[ip]
        let intToMode = function | 0 -> Position | 1 -> Immediate | _ -> invalidArg "intMode" "must be 0 or 1"
        let mode1 = intToMode ((opCode % 1000) / 100)
        let mode2 = intToMode (opCode / 1000)
        let readFrom addr = function
            | Position -> mem.[mem.[addr]]
            | Immediate -> mem.[addr]
        match (opCode % 100) with
        | 1 ->
            mem.[mem.[ip + 3]] <- (readFrom (ip + 1) mode1) + (readFrom (ip + 2) mode2)
            (Running, ip + 4, outputs)
        | 2 ->
            mem.[mem.[ip + 3]] <- (readFrom (ip + 1) mode1) * (readFrom (ip + 2) mode2)
            (Running, ip + 4, outputs)
        | 3 ->
            mem.[mem.[ip + 1]] <- input
            (Running, ip + 2, outputs)
        | 4 ->
            (Running, ip + 2, (readFrom (ip + 1) mode1)::outputs)
        | 5 ->
            if (readFrom (ip + 1) mode1) <> 0 then (Running, (readFrom (ip + 2) mode2), outputs)
            else (Running, ip + 3, outputs)
        | 6 ->
            if (readFrom (ip + 1) mode1) = 0 then (Running, (readFrom (ip + 2) mode2), outputs)
            else (Running, ip + 3, outputs)
        | 7 ->
            if (readFrom (ip + 1) mode1) < (readFrom (ip + 2) mode2) then mem.[mem.[ip + 3]] <- 1
            else mem.[mem.[ip + 3]] <- 0
            (Running, ip + 4, outputs)
        | 8 ->
            if (readFrom (ip + 1) mode1) = (readFrom (ip + 2) mode2) then mem.[mem.[ip + 3]] <- 1
            else mem.[mem.[ip + 3]] <- 0
            (Running, ip + 4, outputs)
        | 99 -> (Finished, ip, outputs)
        | _ -> invalidOp "Invalid opcode"

    let solve (mem: int array) input =
        let rec loop ip outputs =
            let (nextState, nextIp, nextOutputs) = cycle ip outputs mem input
            match nextState with
            | Running -> loop nextIp nextOutputs
            | Finished -> nextOutputs
        loop 0 []

    let program =
        File.ReadAllLines("input.txt")
        |> Array.exactlyOne
        |> fun line -> line.Split(',')
        |> Array.map int

    match (solve (Array.copy program) 1) with
    | [] -> invalidOp "no outputs"
    | v::vs ->
        if List.exists (fun x -> x <> 0) vs then invalidOp "Test program failed"
        else printfn "Answer 1: %i" v

    match (solve (Array.copy program) 5) with
    | [] -> invalidOp "no outputs"
    | v::vs ->
        if vs = [] then printfn "Answer 2: %i" v
        else invalidOp "Test program failed"

    Console.ReadKey() |> ignore
    0 // return an integer exit code
