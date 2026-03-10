open System
//Zadanie 1
//(*

//Двоичный список в десятичное число
let rec binaryToDecimal binaryList =
    let rec helper list index acc =
        match list with
        | [] -> acc
        | head :: tail ->
            let newAcc =
                if head = 1 then 
                    acc + pown 2 (List.length binaryList - 1 - index) 
                else 
                    acc
            helper tail (index + 1) newAcc
    helper binaryList 0 0


//Строки в список двоичных цифр 
let stringToBinaryList (input: string) =
    let rec charsToList chars acc =
        match chars with
        | [] -> List.rev acc
        | c :: rest ->
            let digit = if c = '1' then 1 else 0
            charsToList rest (digit :: acc)
    charsToList (Seq.toList input) []

//Проверка
let isValidBinaryList userList validLists =
    List.contains userList validLists

//Изначальный список
let getValidBinaryNumbers () = [
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

//Разделение строки на список подстрок
let splitBySpace (text: string) =
    let rec collectWords currentWord words chars =
        match chars with
        | [] -> 
            if currentWord = "" then List.rev words
            else List.rev (currentWord :: words)
        | c :: rest ->
            if c = ' ' then
                if currentWord = "" then collectWords "" words rest
                else collectWords "" (currentWord :: words) rest
            else
                collectWords (currentWord + string c) words rest
    collectWords "" [] (Seq.toList text)


let processInput (input: string) =
    let binaryNumbers = getValidBinaryNumbers ()
    let rawInputs = splitBySpace input
    
    //Cоздание списка десятичных результатов
    let results = 
        rawInputs |> List.map (fun singleInput ->
            let userBinaryList = stringToBinaryList singleInput
            if isValidBinaryList userBinaryList binaryNumbers then
                string (binaryToDecimal userBinaryList)
            else
                "ошибка"
        )

    //Вывод
    printfn "\nИзначальный список : %s" (String.Join(" ", rawInputs))
    printfn "Получившийся список : %s" (String.Join(" ", results))

[<EntryPoint>]
let main args =
    printfn "===Преобразование двоичных чисел в десятичные==="
    
    printf "Введите двоичные цифры через пробел: "
    let input = Console.ReadLine()
    
    if not (String.IsNullOrWhiteSpace(input)) then
        processInput input
    
    0//*)

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
