module PiParser exposing (..)

import Parser exposing (..)
import Char
import Set

type Value = Name String
           | Int Int
           
type ProcLit = Send String (List Value)            -- x![v1, ..., vn] 
             | Receive String (List Value) ProcLit -- x?[v1, ..., vn].P
             | Create String ProcLit               -- \x.P
             | Replicate ProcLit                   -- !P
             | Parallel (List ProcLit)             -- P|Q
             | Null                                -- 0

flip : (a -> b -> c) -> b -> a -> c
flip f a b = f b a
             
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
    oneOf [ Parser.map List.singleton p |> andThen (flip loop <| sepByHelp sep p)
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
      
receive : String -> Parser ProcLit
receive channelName =
    succeed (Receive channelName) 
        |. lexeme (symbol "?")
        |= list valueLit
        |. lexeme (symbol ".")
        |= lazy (\_ -> process)

send : String -> Parser ProcLit
send channelName =
    succeed (Send channelName) 
        |. lexeme (symbol "!")
        |= list valueLit           
    
create : Parser ProcLit
create =
    succeed Create
        |. lexeme (symbol "\\")
        |= lexeme nameLit
        |. lexeme (symbol ".")
        |= lazy (\_ -> process)
    
replicate : Parser ProcLit
replicate =
    succeed Replicate
        |. lexeme (symbol "!")
        |= lazy (\_ -> process)

null : Parser ProcLit
null =
    succeed Null
        |. lexeme (symbol "0")

process : Parser ProcLit
process =
    oneOf [ (lexeme nameLit |> andThen (\n -> oneOf [ receive n, send n ]))
          , create
          , replicate
          , null
          , paren (lazy (\_ -> parallel))
          ]

parallel : Parser ProcLit
parallel =
    Parser.map Parallel 
    <| sepBy1 "|" process
                                        
parser : Parser ProcLit
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

show : ProcLit -> String
show proc =
    let showValues vs = "[" ++ String.join "," (List.map showValue vs) ++ "]"
        showWithParen proc_ = case proc_ of
                                  Parallel ps -> case ps of
                                                     [p] -> showWithParen p
                                                     _ -> "(" ++ show (Parallel ps) ++ ")"
                                  _ -> show proc_
    in
    case proc of
        Send x y -> x ++ "!" ++ showValues y
        Receive x y p -> x ++ "?" ++ showValues y ++ "." ++ showWithParen p
        Create x p -> "new " ++ x ++ "." ++ showWithParen p
        Replicate p -> "!" ++ showWithParen p
        Parallel ps -> String.join "|" <| List.map showWithParen ps
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

