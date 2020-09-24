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
        |. lexeme (symbol ".")
        |= lazy (\_ -> process)

send : String -> Parser ProcLit
send channelName =
    succeed (Send channelName) 
        |. lexeme (symbol "!")
        |= lexeme varLit
        |. lexeme (symbol ".")
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
show : ProcLit -> String
show proc =
    case proc of
        Receive x y p -> x ++ "?" ++ y ++ "." ++ show p
        Send x y p -> x ++ "!" ++ y ++ "." ++ show p
        Parallel p q -> "(" ++ show p ++ "|" ++ show q ++ ")"
        Create x p -> "new " ++ x ++ "." ++ show p
        Replicate p -> "!" ++ show p
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

