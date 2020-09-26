module Pi exposing (..)

import PiParser as PP
import Set as S
import Dict as D

type ProcList =
    ProcList
    { send : List { x : String
                  , y : String
                  , p : ProcAndFN
                  , fn : S.Set String }
    , receive : List { x : String
                     , y : String
                     , p : ProcAndFN
                     , fn : S.Set String }
    , create : List { x : String
                    , p : ProcAndFN
                    , fn : S.Set String }
    , replicate : List { p : ProcAndFN
                       , fn : S.Set String }
    }

type alias ProcAndFN =
    { ps : ProcList
    , fn : S.Set String
    } -- Null if all lists (send, ... , replicate) are empty

null = { send = [], receive = [], create = [], replicate = [] }    
    
lit2PFN : PP.ProcLit -> ProcAndFN
lit2PFN procLit =
    case procLit of
        PP.Send x y p ->
            let p_ = lit2PFN p
                fn = S.insert x <| S.insert y p_.fn
            in
            { ps = ProcList
                  { null | send = [ { x = x, y = y, p = p_, fn = fn } ] }
            , fn = fn
            }
        PP.Receive x y p ->
            let p_ = lit2PFN p
                fn = S.insert x <| S.remove y p_.fn in
            { ps = ProcList
                  { null | receive = [ { x = x, y = y, p = p_, fn = fn } ] }
            , fn = fn
            }
        PP.Parallel p q ->
            let p_ = lit2PFN p
                q_ = lit2PFN q 
                (ProcList p_ps, ProcList q_ps) =  (p_.ps, q_.ps) in
                { ps = ProcList
                      { send = p_ps.send ++ q_ps.send
                      , receive = p_ps.receive ++ q_ps.receive
                      , create = p_ps.create ++ q_ps.create
                      , replicate = p_ps.replicate ++ q_ps.replicate }
                , fn = S.union p_.fn q_.fn
                } 
        PP.Create x p ->
            let p_ = lit2PFN p 
                fn = S.remove x p_.fn in
            { ps = ProcList
                  { null | create = [ { x = x, p = p_, fn = fn } ] }
            , fn = fn
            }
        PP.Replicate p ->
            let p_ = lit2PFN p in 
            { ps = ProcList
                  { null | replicate = [ { p = p_, fn = p_.fn } ] }
            , fn = p_.fn
            }
        PP.Null ->
            { ps = ProcList null
            , fn = S.empty  }               

-- show            
procList2StringList : ProcList -> List String
procList2StringList (ProcList ps) = 
    let sends = List.map (\p -> p.x ++ "!" ++ p.y ++ "." ++ showWithParen p.p) ps.send
        receives = List.map (\p -> p.x ++ "?" ++ p.y ++ "." ++ showWithParen p.p) ps.receive
        creates = List.map (\p -> "\\" ++ p.x ++ "." ++ showWithParen p.p) ps.create
        replicates = List.map (\p -> "!" ++ showWithParen p.p) ps.replicate
    in sends ++ receives ++ creates ++ replicates
            
show : ProcAndFN -> String
show pfn =
    if isNull pfn then "0"
    else String.join "|" <| procList2StringList pfn.ps
        
showWithParen : ProcAndFN -> String
showWithParen pfn =
    let psStrList = procList2StringList pfn.ps in
    case psStrList of
        [] -> "0"
        [p] -> p
        _ -> "(" ++ String.join "|" psStrList ++ ")"

-- canonicalize             
isNull : ProcAndFN -> Bool
isNull pfn =
    pfn.ps == ProcList null

classify : (a -> Bool) -> List a -> (List a, List a)
classify f list =
    case list of
        [] -> ([], [])
        h::t -> if f h then Tuple.mapFirst ((::) h) <| classify f t
                else Tuple.mapSecond ((::) h) <| classify f t

tupleMapThird : (c -> x) -> (a, b, c) -> (a, b, x)
tupleMapThird f (a, b, c) = (a, b, f c)
    
normalize : ProcAndFN -> ProcAndFN
normalize pfn =
    let (ProcList ps) = pfn.ps
        mapNormalizeChild = List.map (\p -> { p | p = normalize p.p })
        filterNull = mapNormalizeChild << List.filter (\p -> not <| isNull p.p )
        creates = filterNull ps.create
        ps_ = 
          { null | send = mapNormalizeChild ps.send
          , receive = mapNormalizeChild ps.receive
          , replicate = filterNull ps.replicate
          }
        (creates_, unbound) = scopeExtension creates
    in
        { ps = unionProcList (ProcList { ps_ | create = filterNull creates_ }) unbound
        , fn = pfn.fn }

scopeExtension : List { x : String, p : ProcAndFN, fn : S.Set String }
               -> ( List { x : String, p : ProcAndFN, fn : S.Set String }, ProcList)
scopeExtension =
    Tuple.mapSecond (List.foldl unionProcList <| ProcList null)
    << List.unzip 
    << List.map (\create ->
                     Tuple.mapFirst (\p -> { create | p = p, fn = S.remove create.x p.fn })
                     <| classifyBound create.x create.p
                )
             
unionProcList : ProcList -> ProcList -> ProcList
unionProcList p1 p2 =
    let (ProcList p1_, ProcList p2_) = (p1, p2) in
    ProcList { send = p1_.send ++ p2_.send
             , receive = p1_.receive ++ p2_.receive
             , create = p1_.create ++ p2_.create
             , replicate = p1_.replicate ++ p2_.replicate
             }            
    
classifyBound : String -> ProcAndFN -> (ProcAndFN, ProcList)
classifyBound x pfn =
    let (ProcList ps) = pfn.ps
        classifyBound_ procs =
            classify (\proc -> S.member x proc.fn) procs
        (boundSend, unBoundSend) = classifyBound_ ps.send
        (boundReceive, unBoundReceive) = classifyBound_ ps.receive
        (boundCreate, unBoundCreate) = classifyBound_ ps.create
        (boundReplicate, unBoundReplicate) = classifyBound_ ps.replicate
    in
        ({ ps = ProcList
               { send = boundSend
               , receive = boundReceive
               , create = boundCreate
               , replicate = boundReplicate
               }
         , fn = pfn.fn }
        , ProcList { send = unBoundSend
                   , receive = unBoundReceive
                   , create = unBoundCreate
                   , replicate = unBoundReplicate
                   }
        )

-- substitution        
substituteSend : String -> String
               -> { x : String, y : String, p : ProcAndFN, fn : S.Set String }
               -> { x : String, y : String, p : ProcAndFN, fn : S.Set String }
substituteSend y z q =
    let x_ = if q.x == y then z else q.x
        y_ = if q.y == y then z else q.y
        p_ = substitute y z q.p
    in
    { x = x_
    , y = y_
    , p = p_
    , fn = S.insert x_ <| S.insert y_ p_.fn
    }
                 
substituteReceive : String -> String
                  -> { x : String, y : String, p : ProcAndFN, fn : S.Set String }
                  -> { x : String, y : String, p : ProcAndFN, fn : S.Set String }
substituteReceive y z q =
    let x_ = if q.x == y then z else q.x
        p_ = if q.y == y then q.p
             else
                 let p__ = 
                         if q.y /= z then q.p
                         else substitute z (newVar z <| S.insert z q.p.fn) q.p
                 in substitute y z p__
    in
    { q | x = x_
    , p = p_
    , fn = S.insert x_ <| S.remove q.y p_.fn
    }

substituteCreate : String -> String
                 -> { x : String, p : ProcAndFN, fn : S.Set String }
                 -> { x : String, p : ProcAndFN, fn : S.Set String }
substituteCreate y z q =
    let
        p_ = if q.x == y then q.p
             else
                 let p__ = 
                         if q.x /= z then q.p
                         else substitute z (newVar z <| S.insert z q.p.fn) q.p
                 in substitute y z p__
    in
    { q | p = p_
    , fn = S.remove q.x p_.fn
    }

substituteReplicate : String -> String
                      -> { p : ProcAndFN, fn : S.Set String }
                      -> { p : ProcAndFN, fn : S.Set String }
substituteReplicate y z q =
    let p_ = substitute y z q.p in
    { p = p_
    , fn = p_.fn
    }            

substitute : String -> String -> ProcAndFN -> ProcAndFN        
substitute y z q =
    let (ProcList ps) = q.ps
        send_ = List.map (substituteSend y z) ps.send
        receive_ = List.map (substituteReceive y z) ps.receive
        create_ = List.map (substituteCreate y z) ps.create
        replicate_ = List.map (substituteReplicate y z) ps.replicate
    in { q | ps = ProcList { ps | send = send_
                                 , receive = receive_
                                 , create = create_
                                 , replicate = replicate_
                           } }       
             
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
                      
eval : ProcAndFN -> Maybe ProcAndFN 
eval pfn =
    let (ProcList ps) = pfn.ps in
    
