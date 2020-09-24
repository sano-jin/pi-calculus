module VM exposing (..)

import PiParser as PP
import Set as S
import Dict as D

type alias ProcAndFN = { proc : ProcVal, fn : S.Set String }
type ProcVal = Receive String String ProcAndFN -- x?y.P
             | Send String String ProcAndFN    -- x!y.P
             | Parallel ProcAndFN ProcAndFN      -- P|Q
             | Create String ProcAndFN         -- \x.P
             | Replicate ProcAndFN                -- !P
             | Null                         -- 0

lit2PFN : PP.ProcLit -> ProcAndFN
lit2PFN procLit =
    case procLit of
        PP.Receive x y p ->
            let p_ = lit2PFN p in 
            { proc = Receive x y p_, fn = S.insert x <| S.remove y p_.fn }
        PP.Send x y p ->
            let p_ = lit2PFN p in 
            { proc = Receive x y p_, fn = S.insert x <| S.insert y p_.fn }
        PP.Parallel p q ->
            let p_ = lit2PFN p
                q_ = lit2PFN q in 
            { proc = Parallel p_ q_, fn = S.union p_.fn q_.fn }
        PP.Create x p ->
            let p_ = lit2PFN p in 
            { proc = Create x p_, fn = S.remove x p_.fn }
        PP.Replicate p ->
            let p_ = lit2PFN p in 
            { proc = Replicate p_, fn = p_.fn }
        PP.Null -> { proc = Null, fn = S.empty }

leftPara : ProcAndFN -> ProcAndFN
leftPara pfn =
    case pfn.proc of
        Parallel p q -> case q.proc of
                            Parallel ql qr -> leftPara { proc = Parallel { proc = Parallel p ql
                                                                          , fn = S.union p.fn ql.fn
                                                                          } qr
                                                        , fn = pfn.fn }
                            _ -> { proc = Parallel (leftPara p) (leftPara q)
                                 , fn = pfn.fn }
        _ -> pfn

show : ProcAndFN -> String
show pfn =
    case pfn.proc of
        Receive x y p -> x ++ "?" ++ y ++ "." ++ show p
        Send x y p -> x ++ "!" ++ y ++ "." ++ show p
        Parallel p q -> "(" ++ show p ++ "|" ++ show q ++ ")"
        Create x p -> "new " ++ x ++ "." ++ "(" ++ show p ++ ")"
        Replicate p -> "!" ++ show p
        Null -> "0"


{--
substitute : String -> ProcAndFN -> ProcAndFN -> ProcAndFN
substitute var termAndFV1 termAndFV2 =
    case termAndFV1.term of
        VarVal x -> if x == var then termAndFV2
                    else termAndFV1
        AppVal fun val ->
            let (fun_, val_) =  (substitute var fun termAndFV2, substitute var val termAndFV2) in
            { term = AppVal fun_ val_
            , fv = S.union fun_.fv val_.fv
            }
        LamVal x body ->
            if x == var then termAndFV1
            else let (x_, body_) =
                         if S.member x termAndFV2.fv then
                             let z = newVar x
                                         <| S.union termAndFV2.fv body.fv in
                             (z, substitute x body {term = VarVal z, fv = S.singleton z })
                         else (x, body)
                     body__ = substitute var body_ termAndFV2
                 in { term = LamVal x_ body__
                    , fv = S.remove x_ body__.fv
                    }
--}
-- convert to postfix notation
getIndex : a -> List a -> Maybe Int
getIndex x list =
    let getIndexHelp l i =
            case l of
                [] -> Nothing
                h::t -> if h == x then Just i
                        else getIndexHelp t (i + 1)
    in getIndexHelp list 0

                      {--
toPostfixNotation : ProcAndFN -> List String -> String
toPostfixNotation tFV env =
    case tFV.term of
        VarVal x -> case getIndex x env of
                        Nothing -> x
                        Just i -> String.fromInt i
        AppVal m n -> toPostfixNotation m env ++ toPostfixNotation n env ++ "@"
        LamVal x body -> toPostfixNotation body (x::env) ++ "\\"

-- show
showT : ProcAndFN -> String
showT tFV =
    case tFV.term of
        VarVal x -> x
        AppVal tFV1 tFV2 -> showAppFun tFV1 ++ showAppVal tFV2
        LamVal var body -> "\\" ++ var ++ showCurriedAbs body 

showCurriedAbs tFV =
    case tFV.term of
        LamVal var body -> var ++ showCurriedAbs body
        _ -> "." ++ showT tFV

showAppFun tFV =
    case tFV.term of
        LamVal var body ->
            "(" ++ showT tFV ++ ")"
        _ -> showT tFV

showAppVal tFV =
    case tFV.term of
        VarVal _ -> showT tFV
        AppVal tFV1 tFV2 -> "(" ++ showT tFV ++ ")"
        LamVal var body -> showAppFun tFV
               
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
                      
                      --}
