module Pi exposing (..)

import PiParser as PP exposing ( ProcList, Proc (..), Value )
import Set as S
import Dict as D
import Util exposing (..)

type Chan = Senders (List String)
          | Receivers (List (String, ProcList))
            
type alias Env = List (String, Chan)
type alias OutputString = List String
type alias State = (Env, OutputString)
    
lookupMap : (Chan -> (Chan, a)) -> String -> Env -> (Env, a)
lookupMap f x env =
    case env of
        [] -> let (c, a) = f <| Senders [] in ([(x, c)], a)
        (y, c)::cs ->
            if x == y then
                let (c_, a) = f c in ((y, c_)::cs, a)
            else
                let (cs_, a) = lookupMap f x cs in ((y, c)::cs_, a)

freeNamesPL : ProcList -> S.Set String
freeNamesPL procList =
    List.foldl S.union S.empty <| List.map freeNames procList
              
freeNames : Proc -> S.Set String
freeNames proc =    
    case proc of
        Send x y -> S.fromList [ x, y ]
        Receive x y p -> S.insert x <| S.remove y <| freeNamesPL p
        Create x p -> S.remove x <| freeNamesPL p
        Replicate p -> freeNamesPL p
        Null -> S.empty                         

substitutePL : String -> String -> ProcList -> ProcList
substitutePL var val procList =
    List.map (substitute var val) procList
    
substitute : String -> String -> Proc -> Proc
substitute var val proc =
    case proc of
        Send x y -> Send
                    (if x == var then val else x)
                    (if y == var then val else y)
        Receive x y p ->
            let x_ = if x == var then val else x in
            if y == var then Receive x_ y p
            else if y /= val then Receive x_ y <| substitutePL var val p 
                 else let y_ = newVar y <| S.insert val <| freeNamesPL p
                          p_ = substitutePL y y_ p
                      in Receive x_ y_ <| substitutePL var val p_
        Create x p ->
            if x == var then Create x p
            else if x /= val then Create x <| substitutePL var val p
                 else let x_ = newVar x <| S.insert val <| freeNamesPL p
                          p_ = substitutePL x x_ p
                      in Create x_ <| substitutePL var val p_
        Replicate p ->
            Replicate <| substitutePL var val p
        Null -> Null
                
send : String -> String -> State -> State
send channel value (env, outputs) =
    let message = "sent '" ++ value ++ "' over channel '" ++ channel ++ "'"
        send_ ch =
            case ch of
                Senders ss -> ( Senders <| ss ++ [ value ]
                              , ( identity
                                , message ++ ". waiting to be received." )
                              )
                Receivers [] -> ( Senders [ value ]
                                , (identity, message ++ ". waiting to be received." )
                                )
                Receivers ((var, procList) :: rs) ->
                    let procList_ = List.map (substitute var value) procList in
                    ( Receivers rs
                    , (evalPL procList_ 
                      , message ++ " and received with '" ++ var ++ "'.")
                    )    
        (env_, (f, output)) = lookupMap send_ channel env in
   f (env_, output::outputs)

receive : String -> String -> ProcList -> State -> State
receive channel var procList (env, outputs) =
    let messageWaiting = "channel '" ++ channel ++ "' is waiting to receive a value."
        messageReceived value =
            "channel '" ++ channel ++ "' received '" ++ value ++ "', and bound it to '" ++ var ++ "'."
        recv_ ch =
            case ch of
                Receivers ss -> ( Receivers <| ss ++ [ (var, procList) ]
                              , ( identity, messageWaiting )
                              )
                Senders [] -> ( Receivers [ (var, procList) ]
                                , (identity, messageWaiting )
                                )
                Senders (value :: ss) ->
                    let procList_ = List.map (substitute var value) procList in
                    ( Senders ss
                    , (evalPL procList_
                      , messageReceived value)
                    )    
        (env_, (f, output)) = lookupMap recv_ channel env in
   f (env_, output::outputs)

create : String -> ProcList -> State -> State
create channel procList (env, outputs) =
    let message = "created new channel '" ++ channel ++ "'."
        state = ((channel, Senders [])::env, message::outputs) in
    Tuple.mapFirst (List.drop 1) <| evalPL procList state

evalPL : ProcList -> State -> State
evalPL procList state = List.foldl eval state procList
         
eval : Proc -> State -> State
eval proc state =
    case proc of
        Send ch values -> send ch values state
        Receive ch var p -> receive ch var p state
        Create ch p -> create ch p state 
        _ -> state

run : ProcList -> List String
run procList =
    List.reverse
        <| Tuple.second
            <| evalPL procList ([], [])
                         
newVar : String -> S.Set String -> String
newVar var fv =
    case S.member var fv of
        False -> var
        True ->
            case var of
                "z" -> newVar "A" fv
                "Z" -> newVar "a" fv
                _ ->  newVar (String.map
                                  (Char.fromCode << (+) 1 << Char.toCode) var) fv
                      
