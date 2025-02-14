open System
open System.IO

type Point = { X: float; Y: float }

let parseLine (line: string) : Point option =
    let delims = [|' '; '\t'; ';'|]
    let parts = line.Split(delims, StringSplitOptions.RemoveEmptyEntries)
    if parts.Length >= 2 then
        match Double.TryParse(parts.[0]), Double.TryParse(parts.[1]) with
        | (true, x), (true, y) -> Some { X = x; Y = y }
        | _ -> None
    else
        None

let linearInterpolate (p0: Point) (p1: Point) (x: float) : float =
    let t = (x - p0.X) / (p1.X - p0.X)
    p0.Y + t * (p1.Y - p0.Y)

let newtonInterpolate (points: Point list) (x: float) : float =
    let n = points.Length
    let dd = 
        points
        |> List.mapi (fun i p -> (i, p.Y))
        |> List.fold (fun acc (i, y) -> Array2D.set acc i 0 y; acc) (Array2D.zeroCreate<float> n n)
    let dd = 
        [1 .. n - 1]
        |> List.fold (fun acc j ->
            [0 .. n - j - 1]
            |> List.fold (fun acc i ->
                Array2D.set acc i j ((Array2D.get acc (i+1) (j-1) - Array2D.get acc i (j-1)) / (points.[i+j].X - points.[i].X))
                acc
            ) acc
        ) dd
    let result = 
        [1 .. n - 1]
        |> List.fold (fun (res, prod) j ->
            let prod = prod * (x - points.[j-1].X)
            res + Array2D.get dd 0 j * prod, prod
        ) (Array2D.get dd 0 0, 1.0)
    fst result

let lagrangeInterpolate (points: Point list) (x: float) : float =
    points
    |> List.fold (fun acc p ->
        let l = points
                |> List.filter (fun q -> q <> p)
                |> List.fold (fun acc q -> acc * (x - q.X) / (p.X - q.X)) 1.0
        acc + l * p.Y
    ) 0.0

type LinearState = {
    PrevPoint : Point option
    NextX : float option
}

let createLinearState () = { PrevPoint = None; NextX = None }

let processLinear (step: float) (point: Point) (state: LinearState) : LinearState =
    match state.PrevPoint, state.NextX with
    | None, _ ->
        { state with PrevPoint = Some point; NextX = Some point.X }
    | Some(prev), Some(nextX) when point.X > prev.X ->
        let rec loop x acc =
            if x <= point.X then
                let y = linearInterpolate prev point x
                printfn ">linear: %g %g" x y
                loop (x + step) { state with NextX = Some x; PrevPoint = Some point }
            else
                acc
        loop nextX state
    | Some(prev), _ ->
        eprintfn "Ошибка: точки должны быть отсортированы по возрастанию X (текущая: %g, предыдущая: %g)" point.X prev.X
        state

let flushLinear (step: float) (state: LinearState) (lastPoint: Point) =
    match state.NextX with
    | Some(x) when x <= lastPoint.X ->
        let prev = state.PrevPoint.Value
        let rec loop curX =
            if curX <= lastPoint.X then
                let y = linearInterpolate prev lastPoint curX
                printfn ">linear: %g %g" curX y
                loop (curX + step)
        loop x
    | _ -> ()

type NewtonState = {
    buffer: Point list
    windowSize: int
    NextX: float option
}

let createNewtonState windowSize = 
    { buffer = []; windowSize = windowSize; NextX = None }

let processNewton (step: float) (point: Point) (state: NewtonState) : NewtonState =
    let buffer = point :: state.buffer |> List.truncate state.windowSize
    if List.length buffer = state.windowSize then
        let window = List.rev buffer
        let nextX = state.NextX |> Option.defaultValue (List.head window).X
        let rec loop x acc =
            if x <= (List.last window).X then
                let y = newtonInterpolate window x
                printfn ">newton: %g %g" x y
                loop (x + step) { state with buffer = buffer; NextX = Some(x + step) }
            else
                acc
        loop nextX state
    else
        { state with buffer = buffer }

let flushNewton (step: float) (state: NewtonState) =
    if List.length state.buffer > 0 then
        let window = List.rev state.buffer
        let rec loop curX =
            if curX <= (List.last window).X then
                let y = newtonInterpolate window curX
                printfn ">newton: %g %g" curX y
                loop (curX + step)
        match state.NextX with
        | Some(x) -> loop x
        | None -> ()

type LagrangeState = {
    buffer: Point list
    windowSize: int
    NextX: float option
}

let createLagrangeState windowSize = 
    { buffer = []; windowSize = windowSize; NextX = None }

let processLagrange (step: float) (point: Point) (state: LagrangeState) : LagrangeState =
    let buffer = point :: state.buffer |> List.truncate state.windowSize
    if List.length buffer = state.windowSize then
        let window = List.rev buffer
        let nextX = state.NextX |> Option.defaultValue (List.head window).X
        let rec loop x acc =
            if x <= (List.last window).X then
                let y = lagrangeInterpolate window x
                printfn ">lagrange: %g %g" x y
                loop (x + step) { state with buffer = buffer; NextX = Some(x + step) }
            else
                acc
        loop nextX state
    else
        { state with buffer = buffer }

let flushLagrange (step: float) (state: LagrangeState) =
    if List.length state.buffer > 0 then
        let window = List.rev state.buffer
        let rec loop curX =
            if curX <= (List.last window).X then
                let y = lagrangeInterpolate window curX
                printfn ">lagrange: %g %g" curX y
                loop (curX + step)
        match state.NextX with
        | Some(x) -> loop x
        | None -> ()

type Options = {
    useLinear: bool
    useNewton: bool
    useLagrange: bool
    step: float
    newtonWindowSize: int
    lagrangeWindowSize: int
}

let parseArgs (args: string[]) : Options =
    let rec aux i (opts: Options) =
        if i >= args.Length then opts
        else
            match args.[i] with
            | "--linear" -> aux (i+1) { opts with useLinear = true }
            | "--newton" -> aux (i+1) { opts with useNewton = true }
            | "--lagrange" -> aux (i+1) { opts with useLagrange = true }
            | "--step" ->
                if i+1 < args.Length then
                    let stepVal = float args.[i+1]
                    aux (i+2) { opts with step = stepVal }
                else failwith "! Enter value for --step"
            | "-n" ->
                if i+1 < args.Length then
                    let nVal = int args.[i+1]
                    aux (i+2) { opts with newtonWindowSize = nVal; useNewton = true }
                else failwith "! Enter value for -n"
            | "-l" ->
                if i+1 < args.Length then
                    let lVal = int args.[i+1]
                    aux (i+2) { opts with lagrangeWindowSize = lVal; useLagrange = true }
                else failwith "! Enter value for -l"
            | _ -> aux (i+1) opts
    let defaultOpts = { useLinear = false; useNewton = false; useLagrange = false; step = 1.0; newtonWindowSize = 4; lagrangeWindowSize = 4 }
    aux 0 defaultOpts

[<EntryPoint>]
let main argv =
    let opts = parseArgs argv
    match opts.useLinear, opts.useNewton, opts.useLagrange with
    | false, false, false ->
        eprintfn "! Method is not chosen --linear (-l for ), --newton (-n for interpolation point count), --lagrange, (--step for interpolation step)"
        1
    | _ ->
        let linearState = createLinearState ()
        let newtonState = createNewtonState opts.newtonWindowSize
        let lagrangeState = createLagrangeState opts.lagrangeWindowSize
        let rec processLines linearState newtonState lagrangeState lastPoint =
            match Console.In.Peek() with
            | -1 ->
                match lastPoint with
                | Some(p) ->
                    if opts.useLinear then flushLinear opts.step linearState p
                    if opts.useNewton then flushNewton opts.step newtonState
                    if opts.useLagrange then flushLagrange opts.step lagrangeState
                | None -> ()
            | _ ->
                let line = Console.In.ReadLine()
                match String.IsNullOrWhiteSpace line with
                | true -> processLines linearState newtonState lagrangeState lastPoint
                | false ->
                    match parseLine line with
                    | Some(point) ->
                        let linearState = if opts.useLinear then processLinear opts.step point linearState else linearState
                        let newtonState = if opts.useNewton then processNewton opts.step point newtonState else newtonState
                        let lagrangeState = if opts.useLagrange then processLagrange opts.step point lagrangeState else lagrangeState
                        processLines linearState newtonState lagrangeState (Some point)
                    | None ->
                        eprintfn "Input format error: %s" line
                        processLines linearState newtonState lagrangeState lastPoint
        processLines linearState newtonState lagrangeState None
        0
