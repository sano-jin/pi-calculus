module Pi exposing (..)

import PiParser as PP exposing ( ProcList, Proc (..), Value )
import Set as S
import Dict as D
import Util exposing (..)

type Chan = Senders (List Value)
          | Receivers (List (List String, ProcList))
            
type alias Env = List (String, Chan)
type alias OutputString = List String
type alias State = (Env, OutputString)
    
lookupMap : (Chan -> (Env, String)) -> String -> State -> State
lookupMap f x (env, outputString) =
    case env of
        [] -> let (c_, a) = f <| Senders [] in ([(x, c_)], a::outputString)
        (y, c)::cs ->
            if x == y then 
                let (c_, a) = f c in ((y, c_)::cs, a::outputString)
            else
                let (cs_, a) = lookupMap f x cs in ((y, c)::cs_, a::outputString)

freeNames : Proc -> S.Set String
freeNames proc =
    case proc of
        Send x y ->
            let getNameMaybe val = case val of
                                    Name n -> Just n
                                    _ -> Nothing
                filterNames = List.filterMap getNameMaybe in
            S.insert x <| S.fromList <| filterNames y
        Receive x y p ->
            S.insert x <| S.diff (freeNames p) <| S.fromList y
        Create x p ->
            S.remove x <| freeNames p
        Replicate p ->
            freeNames p
        Null -> S.empty
                
substitute : List String -> List Value -> Proc -> Result String Proc
substitute var val proc =
    case proc of
        Send x y ->
            Send
            <| if x == var then val else x
            <| List.map (\v -> if v == Name var then val else v) y
        Receive x y p ->
            let x_ == var then val else x in
            if var == y then Receive x_ y p
            else 
                case val of
                       Int i -> substitute var val p
                       Name n ->
                           if not <| S.member n y then substitute var val p
                           else let z = newVar n <| S.union (S.fromList y) <| freeNames p
                                    
        Create x y ->
            Send
            <| if x == var then val else x
            <| if y == var then val else y
        Send x y ->
            Send
            <| if x == var then val else x
            <| if y == var then val else y
                
        
             
send : String -> List Value -> State -> Result String State
send channel values state =
    let send_ ch =
            case ch of
                Senders ss ->
                    Ok ( Senders ss ++ [ value ]
                       , "sent " ++ PP.showValues values ++ ". waiting to be received." )
                Receivers [] ->
                    Ok ( [ value ]
                       , "sent " ++ PP.showValues values ++ ". waiting to be received." )
                Receivers ((vars, proc) :: rs) ->
                    if List.length vars /= List.length values then
                        Err "Unmatched length of values. "
                            ++ "Vars are [" ++ String.join "," vars ++ "] but the values are"
                            ++ PP.showValues values
                    else 
                        let state_ = ( Receivers rs,
                                 , "sent " ++ PP.showValues values ++ " and eceived." )
                            procResult = substitute var vales proc in
                        Result.map (flip eval state_) procResult
    in
    lookupMap send_ values state

    
eval : Proc -> State -> (Output, Env)
eval proc state =
    case proc of
        Send x values -> send x values state
        _ -> state

                         
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
                      
