open System

// Zadanie 1
//(*

// Функция для преобразования двоичного списка в десятичное число
let rec binaryToDecimal binaryList =
    let rec helper list index acc =
        match list with
        | [] -> acc
        | head :: tail ->
            // Если встретили 1, добавляем 2^index к результату
            let newAcc = if head = 1 then acc + pown 2 index else acc
            helper tail (index - 1) newAcc
    // Запускаем с конца списка (индекс = длина - 1)
    helper (List.rev binaryList) (List.length binaryList - 1) 0

// Функция для вывода результата
let printResult binary decimal =
    printfn "Двоичное представление: %A" binary
    printfn "Десятичное представление: %d" decimal

[<EntryPoint>]
let main args =
    printfn "===Преобразование двоичных чисел в десятичные==="
    
    // Список двоичных представлений цифр от 1 до 9
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
    
    printfn "Список двоичных значений цифр от 1 до 9:"
    
    // Проходим по каждому двоичному числу и преобразуем его
    let rec processList list =
        match list with
        | [] -> ()
        | head :: tail ->
            let decimal = binaryToDecimal head
            printResult head decimal
            processList tail
    
    processList binaryNumbers
    0

//*)

// Zadanie 2
(*

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

*)