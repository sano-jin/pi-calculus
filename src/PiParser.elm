module PiParser exposing (..)

import Parser exposing (..)
import Char
import Set

type ProcLit = Receive String String ProcLit -- x?y.P
             | Send String String ProcLit    -- x!y.P
             | Parallel ProcLit ProcLit      -- P|Q
             | Create String ProcLit         -- \x.P
             | Replicate ProcLit                -- !P
             | Null                          -- 0

-- Lexer
lexeme : Parser a -> Parser a
lexeme p = p |. spaces 

varLit : Parser String
varLit = variable
         { start = Char.isAlpha
         , inner = \c -> Char.isAlphaNum c || c == '_'
         , reserved = Set.empty
         }
          
-- Parser
flip : (a -> b -> c) -> b -> a -> c
flip f a b = f b a

paren : Parser a -> Parser a
paren p =
    succeed identity
        |. symbol "("
        |. spaces
        |= p
        |. symbol ")"
        |. spaces

receive : String -> Parser ProcLit
receive channelName =
    succeed (Receive channelName) 
        |. lexeme (symbol "?")
        |= lexeme varLit
        |= lazy (\_ -> process)

send : String -> Parser ProcLit
send channelName =
    succeed (Send channelName) 
        |. lexeme (symbol "!")
        |= lexeme varLit
        |= lazy (\_ -> process)

parallel : Parser ProcLit
parallel =
    succeed Parallel
        |= lazy (\_ -> process)
        |. lexeme (symbol "|")
        |= lazy (\_ -> process)

create : Parser ProcLit
create =
    succeed Create
        |. lexeme (symbol "\\")
        |= lexeme varLit
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
    oneOf [ (lexeme varLit |> andThen (\n -> oneOf [ receive n, send n ]))
          , parallel
          , create
          , replicate
          , null
          , paren (lazy (\_ -> process))
          ]
         
       
parser : Parser ProcLit
parser =
    succeed identity
       |. spaces
       |= process
       |. end 

-- show

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

