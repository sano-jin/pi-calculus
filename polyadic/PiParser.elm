module PiParser exposing (..)

import Parser exposing (..)
import Char
import Set
import Util exposing (..)

type Value = Name String
           | Int Int

type alias ProcList = List Proc                   -- P|Q
type Proc = Send String (List Value)              -- x![v1, ..., vn] 
          | Receive String (List String) ProcList -- x?[v1, ..., vn].P
          | Create String ProcList                -- \x.P
          | Replicate ProcList                    -- !P
          | Null                                  -- 0

-- Lexer
lexeme : Parser a -> Parser a
lexeme p = p |. spaces 

nameLit : Parser String
nameLit =
    lexeme
    <| variable
        { start = Char.isAlpha
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.empty
        }
                   
intLit : Parser Int
intLit =
    lexeme
    <| oneOf
        [ succeed negate
        |. lexeme (symbol "-")
        |= int
        , int
        ]

valueLit : Parser Value
valueLit =
    oneOf [ Parser.map Name nameLit
          , Parser.map Int intLit ]
    
-- Parser
sandwitched : String -> String -> Parser a -> Parser a
sandwitched left right p =
    succeed identity
        |. lexeme (symbol left)
        |= p
        |. lexeme (symbol right)

paren : Parser a -> Parser a
paren = sandwitched "(" ")"

sepBy1 : String -> Parser a -> Parser (List a)
sepBy1 sep p =
    Parser.map List.singleton p |> andThen (flip loop <| sepByHelp sep p)

sepBy : String -> Parser a -> Parser (List a)
sepBy sep p =
    oneOf [ sepBy1 sep p
          , succeed [] ]
        
sepByHelp : String -> Parser a -> List a -> Parser (Step (List a) (List a))
sepByHelp sep p as_ =
  oneOf
    [ succeed (\a -> Loop (a::as_))
    |. lexeme (symbol sep)
    |= p
    , succeed ()
    |> map (\_ -> Done (List.reverse as_))
    ]

list : Parser a -> Parser (List a)
list p =
    sandwitched "[" "]" <| sepBy "," p 
      
receive : String -> Parser Proc
receive channelName =
    succeed (Receive channelName) 
        |. lexeme (symbol "?")
        |= list nameLit
        |. lexeme (symbol ".")
        |= lazy (\_ -> process)

send : String -> Parser Proc
send channelName =
    succeed (Send channelName) 
        |. lexeme (symbol "!")
        |= list valueLit           
    
create : Parser Proc
create =
    succeed Create
        |. lexeme (symbol "\\")
        |= lexeme nameLit
        |. lexeme (symbol ".")
        |= lazy (\_ -> process)
    
replicate : Parser Proc
replicate =
    succeed Replicate
        |. lexeme (symbol "!")
        |= lazy (\_ -> process)

null : Parser Proc
null =
    succeed Null
        |. lexeme (symbol "0")

process : Parser ProcList
process =
    oneOf [ Parser.map List.singleton
                <| oneOf [ (lexeme nameLit |> andThen (\n -> oneOf [ receive n, send n ]))
                         , create
                         , replicate
                         , null
                         ]
          , paren (lazy (\_ -> parallel))
          ]

parallel : Parser ProcList
parallel =
    Parser.map List.concat 
    <| sepBy1 "|" process
                                        
parser : Parser ProcList
parser =
    succeed identity
       |. spaces
       |= parallel
       |. end 

-- show
showValue : Value -> String
showValue value =
    case value of
        Int i -> String.fromInt i
        Name n -> n

showValues vs = "[" ++ String.join "," (List.map showValue vs) ++ "]"
                
showProcList : ProcList -> String
showProcList ps = String.join "|" <| List.map show ps
  
show : Proc -> String
show proc =
    let showPL ps = case ps of
                        [p] -> show p
                        _ -> "(" ++ String.join "|" (List.map show ps) ++ ")"
    in
        case proc of
            Send x y -> x ++ "!" ++ showValues y
            Receive x y p -> x ++ "?" ++ "[" ++ String.join "," y ++ "]." ++ showPL p
            Create x p -> "new " ++ x ++ "." ++ showPL p
            Replicate p -> "!" ++ showPL p
            Null -> "0"        

problem2String : Problem -> String
problem2String problem = case problem of
                             Expecting str -> "expectiong " ++ str 
                             ExpectingVariable -> "expecting variable"
                             ExpectingSymbol str -> "expecting symbol " ++ str
                             ExpectingKeyword str -> "expecting keyword" ++ str
                             ExpectingEnd -> "execting end"
                             UnexpectedChar -> "unexpected char"
                             Problem str -> "problem " ++ str
                             other -> "error!"

