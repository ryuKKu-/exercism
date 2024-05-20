module Wordy

open FParsec

type Operator =
    | Plus
    | Minus
    | Multiply
    | Divide

type Expr =
    | Integer of int32
    | Full of Expr * Operator * Expr

type AST =
    | Expression of Expr

let plus = pstring "plus" >>% Plus
let minus = pstring "minus" >>% Minus
let multiply = pstring "multiplied by" >>% Multiply
let divide = pstring "divided by" >>% Divide

let operatorParser =
    spaces >>. choice [
        plus
        minus
        multiply
        divide
    ]

// let integerExpression = spaces >>? pint32 |>> Integer

let integerParser = spaces >>? pint32

let operationExpression =
    let computeResult int1 operator int2 =
        match operator with
        | Plus -> int1 + int2
        | Minus -> int1 - int2
        | Multiply -> int1 * int2
        | Divide -> int1 / int2
    
    Inline.SepBy(elementParser = integerParser,
                 separatorParser = operatorParser,
                 stateFromFirstElement = id,
                 foldState = (fun acc sep e -> (acc, sep, e) |||> computeResult),
                 resultFromState = id)
    
    // equivalent to
    // pipe2 integerExpression (many (operator .>>. integerExpression))
    //     (fun i sepsAndElems ->
    //           (i, sepsAndElems)
    //           ||> List.fold (fun acc (sep, e) -> (acc, sep, e) |> Full)
    //     )
        
// let expr = spaces >>. choice [ attempt operationExpression; integerExpression ]
let expr = spaces >>. choice [ attempt operationExpression; integerParser ]
    
let final = pstringCI "What is" >>. spaces1 >>. expr .>> pstring "?"
   
let answer question =
    // let rec computeResult expr =
    //     match expr with
    //     | Integer i -> i
    //     | Full (expr1, operator, expr2) ->
    //         let a = computeResult expr1
    //         let b = computeResult expr2
    //         match operator with
    //         | Plus -> a + b
    //         | Minus -> a - b
    //         | Multiply -> a * b
    //         | Divide -> a / b
    
    match run (final .>> eof) question with
    | Success(r, _, _) -> r |> Some
    | Failure _ -> None
