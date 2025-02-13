open System
open System.IO
open System.Globalization

let linearInterpolation (x0, y0) (x1, y1) x =
    y0 + (y1 - y0) * (x - x0) / (x1 - x0)

let newtonInterpolation points x =
    let n = List.length points
    let dividedDifferences: float list list =
        List.init n (fun i -> List.init n (fun _ -> 0.0))
        |> List.mapi (fun i row -> List.mapi (fun j _ -> if j = 0 then snd (List.item i points) else 0.0) row)
    
    let dividedDifferences =
        List.fold (fun (acc: float list list) j ->
            List.mapi (fun i row ->
                if i + j < n then
                    let xi, _ = List.item i points
                    let xj, _ = List.item (i + j) points
                    let value = (List.item (i + 1) acc).[j - 1] - (List.item i acc).[j - 1]
                    let newValue = value / (xj - xi)
                    List.mapi (fun k v -> if k = j then newValue else v) row
                else row) acc) dividedDifferences [1 .. n - 1]
    
    let rec evaluate term acc i =
        if i = n then acc
        else
            let xi, _ = List.item i points
            let term = term * (x - xi)
            evaluate term (acc + term * (List.head dividedDifferences).[i]) (i + 1)
    
    evaluate 1.0 (List.head dividedDifferences).[0] 1

let lagrangeInterpolation points x =
    let lagrangeBasis j x =
        List.fold (fun acc (xi, _) ->
            if xi = fst (List.item j points) then acc
            else acc * (x - xi) / (fst (List.item j points) - xi)) 1.0 points
    List.mapi (fun j (xj, yj) -> yj * lagrangeBasis j x) points
    |> List.sum

let generateInterpolatedPoints interpolationMethod step points =
    let xStart, _ = List.head points
    let xEnd, _ = List.last points
    [xStart .. step .. xEnd]
    |> List.map (fun x -> x, interpolationMethod points x)

let windowSize = 7

let processInput interpolationMethodName interpolationMethod step (inputStream: TextReader) =
    let rec loop lastPrintedX (points: (float * float) list) =
        match inputStream.ReadLine() with
        | null | "EOF" -> 
            if points.Length > 1 then
                let computed = 
                    match points.Length with
                    | len when len < windowSize -> generateInterpolatedPoints interpolationMethod step points 
                    | _ -> 
                        let xStart, _ = List.head points
                        let xEnd, _ = List.last points
                        let xCenter = (xStart + xEnd) / 2.0
                        [ (xCenter, interpolationMethod points xCenter) ]
                let newValues = 
                    match lastPrintedX with
                    | None -> computed
                    | Some lastX -> computed |> List.filter (fun (xi, _) -> xi > lastX)
                newValues |> List.iter (fun (xi, yi) -> printfn "%s: %f %f" interpolationMethodName xi yi)
            points
        | line ->
            match line.Split([|';'; '\t'; ' '|], StringSplitOptions.RemoveEmptyEntries) with
            | [| xStr; yStr |] ->
                let x = Double.Parse(xStr, CultureInfo.InvariantCulture)
                let y = Double.Parse(yStr, CultureInfo.InvariantCulture)
                let newPoints = 
                    match points.Length with
                    | len when len >= windowSize -> (List.tail points) @ [(x, y)]
                    | _ -> points @ [(x, y)]
                match newPoints.Length with
                | len when len > 1 ->
                    let computed =
                        match len with
                        | l when l < windowSize -> generateInterpolatedPoints interpolationMethod step newPoints 
                        | _ -> 
                            let xStart, _ = List.head newPoints
                            let xEnd, _ = List.last newPoints
                            let xCenter = (xStart + xEnd) / 2.0
                            [ (xCenter, interpolationMethod newPoints xCenter) ]
                    let newValues = 
                        match lastPrintedX with
                        | None -> computed
                        | Some lastX -> computed |> List.filter (fun (xi, _) -> xi > lastX)
                    newValues |> List.iter (fun (xi, yi) -> printfn "%s: %f %f" interpolationMethodName xi yi)
                    match newValues with
                    | [] -> loop lastPrintedX newPoints
                    | _ -> loop (Some (List.last newValues |> fst)) newPoints
                | _ -> loop lastPrintedX newPoints
            | _ -> loop lastPrintedX points
    loop None []

let interpolationMethods = 
    [ "linear", (fun points x -> linearInterpolation (List.head points) (List.last points) x)
      "newton", newtonInterpolation
      "lagrange", lagrangeInterpolation ]

[<EntryPoint>]
let main argv =
    let methodName, method, step =
        match argv with
        | [| "--linear"; "--step"; stepStr |] ->
            "linear", (fun points x -> linearInterpolation (List.head points) (List.last points) x),
            Double.Parse(stepStr, CultureInfo.InvariantCulture)
        | [| "--newton"; "--step"; stepStr |] ->
            "newton", newtonInterpolation,
            Double.Parse(stepStr, CultureInfo.InvariantCulture)
        | [| "--lagrange"; "--step"; stepStr |] ->
            "lagrange", lagrangeInterpolation,
            Double.Parse(stepStr, CultureInfo.InvariantCulture)
        | _ ->
            printfn "Использование: my_lab3 --[linear|newton|lagrange] --step [value]"
            Environment.Exit 1
            failwith "Неверные аргументы"
    
    let _ = processInput methodName method step Console.In
    0
