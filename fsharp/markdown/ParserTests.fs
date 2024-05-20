module ParserTests

open System
open Xunit
open Parser
open FsUnit.Xunit

// Helpers
let pcharA = pchar 'A'
let pcharB = pchar 'B'
let pcharC = pchar 'C'


[<Fact>]
let ``should parse a single char`` () =
    let text = "ABCDE"
    let expected : ParserResult<char * string> = Ok('A', "BCDE")
    run pcharA text |> should equal expected
    

[<Fact>]
let ``should not parse a single char`` () =
    let text = "ABCDE"
    let expected : ParserResult<char * string> = Error "Expecting B, got A"
    run pcharB text |> should equal expected
    
[<Fact>]
let ``andThen combinator`` () =
    let text = "ABCDE"
    let combined = pcharA .>>. pcharB
        
    let expected : ParserResult<(char * char) * string> = Ok(('A', 'B'), "CDE")
    run combined text |> should equal expected
    
[<Fact>]
let ``orElse combinator A`` () =
    let text = "ABCDE"
    let pcharAorB = pcharA <|> pcharB
        
    let expected : ParserResult<char * string> = Ok('A', "BCDE")
    run pcharAorB text |> should equal expected
    
[<Fact>]
let ``orElse combinator B`` () =
    let text = "BCDE"
    let pcharAorB = pcharA <|> pcharB
        
    let expected : ParserResult<char * string> = Ok('B', "CDE")
    run pcharAorB text |> should equal expected
    
[<Fact>]
let ``orElse combinator error`` () =
    let text = "ZBCDE"
    let pcharAorB = pcharA <|> pcharB
        
    let expected : ParserResult<char * string> = Error "Expecting B, got Z"
    run pcharAorB text |> should equal expected
    
[<Fact>]
let ``orElse & andThen combinator`` () =
    let text = "ABCDE"
    let pcharBorC = pcharB <|> pcharC
    let combined = pcharA .>>. pcharBorC 
    
    let expected : ParserResult<(char * char) * string> = Ok (('A', 'B'), "CDE")
    run combined text |> should equal expected

    
[<Fact>]
let ``orElse & andThen combinator error`` () =
    let text = "AZBCDE"
    let pcharBorC = pcharB <|> pcharC
    let combined = pcharA .>>. pcharBorC 
    
    let expected : ParserResult<(char * char) * string> = Error "Expecting C, got Z"
    run combined text |> should equal expected
    
[<Fact>]
let ``anyOf combinator`` () =
    let text = "AZBCDE"
    let characterLists = [ 'A'; 'B'; 'C' ]
    let parser = anyOf characterLists
    
    let expected : ParserResult<char * string> = Ok ('A', "ZBCDE")
    run parser text |> should equal expected
    
[<Fact>]
let ``anyOf combinator error`` () =
    let text = "AZBCDE"
    let characterLists = [ 'a' .. 'z' ]
    let parser = anyOf characterLists
    
    let expected : ParserResult<char * string> = Error "Expecting z, got A"
    run parser text |> should equal expected
    
[<Fact>]
let ``mapP combinator`` () =
    let text = "AZBCDE"
    let characterLists = [ 'A' .. 'Z' ]
    let anyOfUppercase = anyOf characterLists
    let parser = (anyOfUppercase .>>. anyOfUppercase .>>. anyOfUppercase)
                 |>> (fun ((c1, c2), c3) -> System.String [| c1; c2; c3 |])
                
    let expected : ParserResult<string * string> = Ok ("AZB", "CDE")
    run parser text |> should equal expected
    
[<Fact>]
let ``pstring combinator`` () =
    let text = "AZBCDE"
    let parser = pstring "AZB" 
    
    let expected : ParserResult<string * string> = Ok ("AZB", "CDE")
    run parser text |> should equal expected  
    
[<Fact>]
let ``pint combinator`` () =
    let text = "-12345Z"
    
    let expected : ParserResult<int * string> = Ok (-12345, "Z")
    run pint text |> should equal expected
   

open MarkdownParser

[<Fact>]
let ``test`` () =   
    let text = "123;456"
    
    // let text2 = "## the ttitle"
    // let text3 = "### the ttitle"
    // let text4 = "####### the ttitle"
    // let text5 = "#### `ttitle`"
    // let p = run headerParser
    let start = opt(newLine) >>. spaces >>. pstring "```" .>> newLine .>>. (manyChars1 anyChar)

    let a = run start text
    // let a = p text2
    // let a = p text3
    // let a = p text4
    // let a = p text5
    
    "A" |> should equal "A"