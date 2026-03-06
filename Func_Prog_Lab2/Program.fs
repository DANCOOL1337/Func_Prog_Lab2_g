open System

//Zadanie 1
//(*
//Преобразования двоичного списка в десятичное число
let rec binaryToDecimal binaryList =
    let rec helper list index acc =
        match list with
        | [] -> acc
        | head :: tail ->
            //Убираем List.rev и считаем с начала списка
            let newAcc = if head = 1 then acc + pown 2 (List.length binaryList - 1 - index) else acc
            helper tail (index + 1) newAcc
    helper binaryList 0 0

//Вывод
let printResult binary decimal =
    printfn "Двоичное представление: %A" binary
    printfn "Десятичное представление: %d" decimal

[<EntryPoint>]
let main args =
    printfn "===Преобразование двоичных чисел в десятичные==="
    
    //Список в двоичном представлении
    let binaryNumbers = [
        [1]                     // 1
        [1; 0]                  // 2
        [1; 1]                  // 3
        [1; 0; 0]               // 4
        [1; 0; 1]               // 5
        [1; 1; 0]               // 6
        [1; 1; 1]               // 7
        [1; 0; 0; 0]            // 8
        [1; 0; 0; 1]            // 9
    ]
    
    //Запрашиваем ввод у пользователя
    printf "Введите двоичное представление числа от 1 до 9: "
    let input = Console.ReadLine()
    
    //Преобразуем строку в список
    let userBinaryList = 
        input.ToCharArray()
        |> Array.map (fun c -> if c = '1' then 1 else 0)
        |> Array.toList
    
    //Проверяем, есть ли такое число в списке
    if List.contains userBinaryList binaryNumbers then
        let decimal = binaryToDecimal userBinaryList
        printfn "\nРезультат:"
        printResult userBinaryList decimal
    else
        printfn "Ошибка. Введено некорректное число"
    
    0
//*)

// Zadanie 2
//(*

// Функция для преобразования шестнадцатеричного символа в число
let hexCharToValue hexChar =
    match hexChar with
    | '0' -> 0
    | '1' -> 1
    | '2' -> 2
    | '3' -> 3
    | '4' -> 4
    | '5' -> 5
    | '6' -> 6
    | '7' -> 7
    | '8' -> 8
    | '9' -> 9
    | 'A' | 'a' -> 10
    | 'B' | 'b' -> 11
    | 'C' | 'c' -> 12
    | 'D' | 'd' -> 13
    | 'E' | 'e' -> 14
    | 'F' | 'f' -> 15
    | _ -> failwith "Недопустимый шестнадцатеричный символ"

// Функция для ввода символа
let readHexChar () =
    printf "Введите шестнадцатеричный символ (0-9, A-F): "
    let input = Console.ReadLine()
    input.[0]  // Берём первый символ

[<EntryPoint>]
let main args =
    printfn "=Преобразование шестнадцатеричного символа в число=\n"
    
    // Вводим символ
    let hexChar = readHexChar ()
    
    try
        // Преобразуем в число
        let decimalValue = hexCharToValue hexChar
        
        // Выводим результат
        printfn "\nСимвол: %c" hexChar
        printfn "Десятичное значение: %d" decimalValue
    with
        | _ -> printfn "Ошибка: Недопустимый шестнадцатеричный символ!"
    0

//*)
