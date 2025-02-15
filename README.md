### Ходжаев. P3308. 368994
## Функциональное программирование 
# Лабораторная работа №3

### Описание обработки входного потока
1. **Чтение строки**:
    ```fsharp
    match String.IsNullOrWhiteSpace line with
    | true -> (linearState, newtonState, lagrangeState, lastPoint)
    | false ->
         match parseLine line with
         | Some(point) -> ...
    ```
    Если строка пустая, завершаем обработку.

2. **Обновление состояния**:
    ```fsharp
    let linearState = if opts.useLinear then processLinear opts.step point linearState else linearState
    let newtonState = if opts.useNewton then processNewton opts.step point newtonState else newtonState
    let lagrangeState = if opts.useLagrange then processLagrange opts.step point lagrangeState else lagrangeState
    ```
    Обновляем состояния для каждого метода интерполяции.

3. **Фильтрация и вывод новых значений**:
    ```fsharp
    match state.NextX with
    | Some(x) when x <= lastPoint.X -> ...
    | _ -> ()
    ```
    Фильтруем уже напечатанные значения и выводим новые.

4. **Рекурсивный вызов**:
    ```fsharp
    loop linearState newtonState lagrangeState lastPoint
    ```
    Рекурсивно вызываем `loop`, обновляя состояния.

### Линейная интерполяция
```fsharp
let linearInterpolate (p0: Point) (p1: Point) (x: float) : float =
     let t = (x - p0.X) / (p1.X - p0.X)
     p0.Y + t * (p1.Y - p0.Y)
```

### Метод Ньютона
```fsharp
let newtonInterpolate (points: Point list) (x: float) : float =
     let dd = computeDividedDifferences points
     let n = points.Length
     let result = 
          [1 .. n - 1]
          |> List.fold (fun (res, prod) j ->
                let prod = prod * (x - points.[j-1].X)
                res + Array2D.get dd 0 j * prod, prod
          ) (Array2D.get dd 0 0, 1.0)
     fst result
```

### Метод Лагранжа
```fsharp
let lagrangeInterpolate (points: Point list) (x: float) : float =
     points
     |> List.fold (fun acc p ->
          let l = points
                     |> List.filter (fun q -> q <> p)
                     |> List.fold (fun acc q -> acc * (x - q.X) / (p.X - q.X)) 1.0
          acc + l * p.Y
     ) 0.0
```
