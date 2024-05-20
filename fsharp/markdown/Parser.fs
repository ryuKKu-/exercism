module Parser

open System
open System.Text.RegularExpressions

type ParserResult<'T> = Result<'T, string>

type Parser<'T> = Parser of (string -> ParserResult<'T * string>)

let EOS = '\uffff'

let satisfy predicate =
    let innerFn input =
        if String.IsNullOrEmpty(input) then
            Error "No input"
        else
            let c = input[0]
            if c <> EOS && predicate c then
                Ok (c, input[1..])
            else Error (sprintf "Unexpected %c" c)
    
    Parser innerFn

/// Parse a single character
let pchar charToMatch =
    satisfy (fun c -> c = charToMatch)
    
/// Unwrap Parser type to get inner function and call it against input
let run parser input =
  let (Parser innerFn) = parser
  innerFn input

let andThen parser1 parser2 =
    let innerFn input =
        let result = run parser1 input
        
        match result with
        | Error err ->
            Error err
        | Ok (value1, remaining) ->
            let result = run parser2 remaining
            
            match result with
             | Error err ->
                Error err
             | Ok (value2, remaining2) ->
                let combined = (value1, value2)
                Ok (combined, remaining2)
                
    Parser innerFn

let orElse parser1 parser2 =
    let innerFn input =
        let result = run parser1 input
        
        match result with
        | Ok _ -> result
        | Error _ -> run parser2 input
            
    Parser innerFn

let (.>>.) = andThen
let (<|>) = orElse

let choiceP parserList = List.reduce (<|>) parserList

/// Choose first occurence of a character present in list
let anyOf charList =
    charList
    |> List.map pchar
    |> choiceP
    
let mapP f parser =
    let innerFun input =
        let result = run parser input
        
        match result with
        | Error err ->
            Error err
        | Ok (value, remaining) ->
            Ok (f value, remaining)
        
    Parser innerFun

let (<!>) = mapP

// swapping order of parameters for mapP
let (|>>) x f = mapP f x

let returnP x =
    let innerFun input =
        Ok (x, input)
        
    Parser innerFun
    
let applyP fP xP =
  // create a Parser containing a pair (f,x)
  (fP .>>. xP)
  // map the pair by applying f to x
  |> mapP (fun (f,x) -> f x)
  
let (<*>) = applyP

let lift2 f fP xP =
    returnP f <*> fP <*> xP
    
let rec sequence parserList =
    // define the "cons" function, which is a two parameter function
    let cons head tail = head :: tail

    // lift it to Parser World
    let consP = lift2 cons

    // process the list of parsers recursively
    match parserList with
    | [] -> returnP []
    | head :: tail -> consP head (sequence tail)

let rec parseZeroOrMore parser input =
    let f = run parser input
    
    match f with
    | Error _ -> ([], input)
    | Ok (value, fRemaining) ->
        let subsequentValues, remaining = parseZeroOrMore parser fRemaining
        (value :: subsequentValues, remaining)

let many parser =
    let innerFun input = Ok (parseZeroOrMore parser input)
    Parser innerFun

let many1 parser =
    let innerFun input =
        let f = run parser input
        match f with
        | Error err -> Error err
        | Ok (value, fRemaining) ->
            let subsequentValues, remaining = parseZeroOrMore parser fRemaining
            Ok (value :: subsequentValues, remaining)
            
    Parser innerFun

let manyMax max parser =
    let innerFun input =
        let f = run parser input
        match f with
        | Error err -> Error err
        | Ok (value, fRemaining) ->
            let subsequentValues, remaining = parseZeroOrMore parser fRemaining
            if subsequentValues.Length > (max - 1) then
                Error "max value"
            else
                Ok (value :: subsequentValues, remaining)
            
    Parser innerFun

let (>>%) p x = p |>> (fun _ -> x)

let skipChar char = pchar char >>% ()

let opt p =
    let some = p |>> Some
    let none = returnP None
    some <|> none

/// keep left
let (.>>) p1 p2 = (p1 .>>. p2) |>> fst
    
/// keep right
let (>>.) p1 p2 = (p1 .>>. p2) |>> snd

/// Keep only the value parsed in the middle
let between p1 p2 p3 = p1 >>. p2 .>> p3

let pstring (str: string) =
    let charListToString charList =
        charList |> List.toArray |> String
    
    str
    |> List.ofSeq
    |> List.map pchar
    |> sequence
    |>> charListToString

let private charListToString charList =
    charList |> List.toArray |> String
  
let manyChars cp =
  many cp
  |>> charListToString

let manyChars1 cp =
  many1 cp
  |>> charListToString

let pint =
    let charListToInt (sign, charList) =
        let int = charList |> List.toArray |> String |> int
        match sign with
        | Some '-' -> -int
        | _ -> int
        
    let digit = anyOf ['0'..'9']
    let minus = opt (pchar '-')
    
    minus .>>. (many1 digit)
    |>> charListToInt
    
let whitespaceChar =
    let predicate = Char.IsWhiteSpace
    satisfy predicate
    
let spaces = many whitespaceChar
let spaces1 = many1 whitespaceChar

let regex pattern =
    let regex = Regex("\\A" + pattern, RegexOptions.Multiline ||| RegexOptions.ExplicitCapture)
    
    let innerFn input =
        let m = regex.Match(input)
        
        if m.Success then
            let str = m.Value
            Ok (str, input[str.Length..])
        else
            Error ""
            
    Parser innerFn

module MarkdownParser =
    let [<Literal>] starChar = '*'
    let [<Literal>] backQuoteChar = '`'
    let [<Literal>] hashChar = '#'
    
    let newLine = opt(pchar '\r') .>>. pchar '\n'
    
    type TextToken =
        | InlineCodeBlock of string
        | Italic of string
        | PlainText of string
        | Link of Description: string * Link: string

    let plainTextParser =
        manyChars1 (satisfy (fun c -> c <> '\n' && c <> starChar && c <> backQuoteChar))
        |>> PlainText

    let inlineCodeBlockParser =       
        let tick = pchar backQuoteChar
        let chars = manyChars1 (satisfy (fun c -> c <> backQuoteChar))
        between tick chars tick
        |>> InlineCodeBlock
        
    let italicParser =
        let star = pchar '*'
        let chars = manyChars1 (satisfy (fun c -> c <> starChar))
        between star chars star
        |>> Italic
        
    let linkParser =
        let leftDesc = pchar '['
        let rightDesc = pchar ']'
        let description = manyChars1 (satisfy(fun c -> c <> '\n' && c <> ']'))
        let descriptionParser = between leftDesc description rightDesc
        
        let linkLeft = pchar '('
        let linkRight = pchar ')'
        let link = manyChars1 (satisfy(fun c -> c <> '\n' && c <> ')'))
        let linkParser = between linkLeft link linkRight
        
        descriptionParser .>>. linkParser
        |>> Link
        
    let textTokenParser =
        choiceP [ plainTextParser
                  inlineCodeBlockParser
                  italicParser
                  linkParser ]

    type MarkdownToken =
        | Title of Type: int * TextToken list
        | Paragraph of TextToken list
        | CodeBlock of string

    let headerParser =
        let hash = manyMax 6 (pchar hashChar)
        
        hash .>> whitespaceChar .>>. (many1 textTokenParser)
        |>> (fun (hash, tokens) -> Title (hash.Length, tokens))

    let anyChar = satisfy(fun c -> c <> EOS)
    
    let codeBlockParser =       
        let start = opt(newLine) >>. spaces >>. pstring "```" .>> newLine
        let ``end`` = newLine >>. pstring "```" .>> newLine .>> spaces
        
        between start (manyChars1 anyChar) ``end``
        |>> CodeBlock
    
    let paragraphParser = many1 textTokenParser