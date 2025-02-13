### Ходжаев. P3308. 368994
## Функциональное программирование 
# Лабораторная работа №3

### Описание обработки входного потока
1. **Чтение строки**:
   ```fsharp
   match inputStream.ReadLine() with
   | null | "EOF" -> 
   ```
   Если строка пустая или EOF, завершаем обработку.

2. **Обновление окна данных**:
   ```fsharp
   match points.Length with
   | len when len >= windowSize -> (List.tail points) @ [(x, y)]
   | _ -> points @ [(x, y)]
   ```
   Если точек больше `windowSize`, удаляем первую точку.

3. **Вычисление интерполяции**:
   ```fsharp
   match points.Length with
   | len when len < windowSize -> generateInterpolatedPoints interpolationMethod step points 
   | _ -> 
       let xStart, _ = List.head points
       let xEnd, _ = List.last points
       let xCenter = (xStart + xEnd) / 2.0
       [ (xCenter, interpolationMethod points xCenter) ]
   ```
   Интерполируем, если точек меньше `windowSize`, иначе вычисляем центральную точку.

4. **Фильтрация и вывод новых значений**:
   ```fsharp
   match lastPrintedX with
   | None -> computed
   | Some lastX -> computed |> List.filter (fun (xi, _) -> xi > lastX)
   ```
   Фильтруем уже напечатанные значения и выводим новые.

5. **Рекурсивный вызов**:
   ```fsharp
   loop (Some (List.last newValues |> fst)) newPoints
   ```
   Рекурсивно вызываем `loop`, обновляя `lastPrintedX`, если были новые значения.

### Линейная интерполяция
```fsharp
let linearInterpolation (x0, y0) (x1, y1) x =
    y0 + (y1 - y0) * (x - x0) / (x1 - x0)
```

### Метод ньютона
```fsharp
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
```

### Метод лагранжа
```fsharp
let lagrangeInterpolation points x =
    let lagrangeBasis j x =
        List.fold (fun acc (xi, _) ->
            if xi = fst (List.item j points) then acc
            else acc * (x - xi) / (fst (List.item j points) - xi)) 1.0 points
    List.mapi (fun j (xj, yj) -> yj * lagrangeBasis j x) points
    |> List.sum
```