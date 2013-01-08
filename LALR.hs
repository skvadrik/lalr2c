module LALR
    ( lalr1_table
    ) where


import qualified Data.Set            as S
import qualified Data.HashMap.Strict as M
import           Data.List                (foldl')

import Control.DeepSeq

import           Types
import           Grammar


first1 :: [Symbol] -> Context
first1 []       = S.singleton Tlambda
first1 (s : ss) =
    let ts = M.lookupDefault (error "yikes") s symbol_to_first1
    in  if S.member Tlambda ts
            then S.union ts (first1 ss)
            else ts


symbol_to_first1 :: M.HashMap Symbol Context
symbol_to_first1 =
    let terminal_headed ss = case ss of
            T _ : _ -> True
            _       -> False
        init_terminal m t = M.insert (T t) (S.singleton t) m
        init_nonterminal m n =
            let rs  = rules n
                ts1 = (S.fromList . map ((\ (T t) -> t) . head) . filter terminal_headed) rs
                ts2 = if elem [] rs then S.insert Tlambda ts1 else ts1
            in  M.insert (N n) ts2 m
        f _  ys []               = ys
        f xs ys (T Tlambda : ss) = f xs ys ss
        f _  ys (T t : _)        = S.insert t ys
        f xs ys (N n : ss)       =
            let zs = M.lookupDefault (error "aargh") (N n) xs
            in  if S.member Tlambda zs
                    then f xs (S.union ys zs) ss
                    else S.union ys zs
        update xs n =
            let rs = filter (not . terminal_headed) (rules n)
                ys = foldl' (f xs) S.empty rs
            in  M.adjust (S.union ys) (N n) xs
        iterate xs =
            let xs' = S.foldl' update xs nonterminals
            in  if xs' == xs then xs else iterate xs'
        xs1 = S.foldl' init_terminal M.empty terminals
        xs2 = S.foldl' init_nonterminal xs1 nonterminals
    in  iterate xs2


lalr1_table :: LALRTable
lalr1_table =
    let sid2st_s2sid = lr1_to_lalr1 state_table
        sids         = M.keys sid2st_s2sid
        f tbl sid =
            let (st, s, s2sid) = M.lookupDefault (error "missing state in state table") sid sid2st_s2sid
                t2action       = action_table st s2sid
                n2sid          = goto_table s2sid
            in  M.insert sid (s, t2action, n2sid) tbl
    in  foldl' f M.empty sids


raise_conflict :: Terminal -> Action -> Action -> a
raise_conflict t act1 act2 = error $ concat
    [ "LALR(1) conflict trying to define ACTION function"
    , "\n\tat symbol "
    , show t
    , "\n\ttwo actions possible: "
    , show act1
    , " vs "
    , show act2
    ]


action_table :: State -> M.HashMap Symbol SID -> ActionTable
action_table state s2sid =
-- проверь эту хрень с точки зрения логики. шифты особенно. не должны ли они быть уникальны по построению и т.д.
    let try_insert act t2act t = case (M.lookup t t2act, act) of
            (Nothing,             _                  ) -> M.insert t act t2act
            (Just Error,          _                  ) -> M.insert t act t2act
            (Just (Shift i),      Shift j            ) | i == j -> t2act
            (Just (Shift i),      Shift j            ) | i /= j -> error "Shifting to different states"
            (Just (Shift _),      Reduce _ _         ) -> t2act
            (Just (Reduce _ _),   Shift _            ) -> M.insert t act t2act
--            (Just (Reduce _ ss1), act'@(Reduce _ ss2)) -> if length ss1 < length ss2 then M.insert t act' t2act else t2act
            (Just act',           _                  ) -> raise_conflict t act' act
        f t2act cr ctx =
            let v = case cr of
                    (_, _, T t : _)                  -> case M.lookup (T t) s2sid of
                        Just sid -> Just (S.singleton t, Shift sid)
                        Nothing  -> error $ "can't find goto for terminal " ++ show t
                    (n, ls, []) | n /= axiom         -> Just (ctx, Reduce n ls)
                    _ | (cr, ctx) == final_situation -> Just (ctx, Accept)
                    _                                -> Nothing
            in  case v of
                    Just (ts, act) -> S.foldl' (try_insert act) t2act ts
                    Nothing        -> t2act
        t2act = S.foldl' (\ t2act' t -> M.insert t Error t2act') M.empty terminals
    in  M.foldlWithKey' f t2act state


goto_table :: M.HashMap Symbol SID -> GotoTable
goto_table s2sid =
    let f m (N n) v = M.insert n v m
        f m _     _ = m
    in  M.foldlWithKey' f M.empty s2sid


lr1_to_lalr1 :: StateTable -> StateTable
lr1_to_lalr1 =
    let unite (sid1, (st1, s1, s2sid1)) (sid2, (st2, s2, s2sid2)) =
            let sid   = min sid1 sid2
                st    = M.foldlWithKey' (\ st cr ctx -> M.insertWith S.union cr ctx st) st1 st2
                s2sid = M.foldlWithKey' (\ s2sid s sid -> M.insertWith min s sid s2sid) s2sid1 s2sid2
                s     = if s1 == s2 then s1 else error $ "symbols associated with sids don't match"
            in  (sid, (st, s, s2sid))
        f cr2sid_st_s_s2sid sid (st, s, s2sid) =
            let cr = (S.fromList . M.keys) st
            in  M.insertWith unite cr (sid, (st, s, s2sid)) cr2sid_st_s_s2sid
    in  M.fromList . M.elems . M.foldlWithKey' f M.empty


state_table :: StateTable
state_table =
    let g (open, closed, result, max) st (sid, s) =
            let (s2sid, open', max') = goto_function open closed st max
                closed'              = M.insert st (sid, s) closed
                result'              = M.insert sid (st, s, s2sid) result
            in  (open', closed', result', max')
        f (open, _,      result, _  ) | open == M.empty = result
        f (open, closed, result, sid)                   = f $ M.foldlWithKey' g (M.empty, closed, result, sid) open
    in  f (M.insert init_state (0, T Tlambda) M.empty, M.empty, M.empty, 1)


init_situation :: (Core, Context)
init_situation =
    let r   = case rules axiom of
            [[ax']] -> [ax']
            _       -> error "bad axiom rule in grammar"
        ctx = S.singleton (Tlambda)
    in  ((axiom, [], r), ctx)


final_situation :: (Core, Context)
final_situation =
    let r   = case rules axiom of
            [[ax']] -> [ax']
            _       -> error "bad axiom rule in grammar"
        ctx = S.singleton (Tlambda)
    in  ((axiom, r, []), ctx)


init_state :: State
init_state =
    let (cr, ctx) = init_situation
        open = M.insert cr ctx M.empty
    in  closing open


goto_function :: M.HashMap State (SID, Symbol) -> M.HashMap State (SID, Symbol) -> State -> SID -> (M.HashMap Symbol SID, M.HashMap State (SID, Symbol), SID)
goto_function open  _      st sid | st == M.empty = (M.empty, open, sid)
goto_function open  closed st sid =
    let f (s2sid, open, max) s =
            let st' = closing $ shift st s
            in  case M.lookup st' closed of
                    Just (sid, s') | s' == s || st' == M.empty -> (M.insert s sid s2sid, open, max)
                    Just (_,   s')                             -> error $ "transitions to the same state on multiple symbols: " ++ show s ++ " and " ++ show s'
                    Nothing                                    -> (M.insert s max s2sid, M.insert st' (max, s) open, max + 1)
    in  S.foldl' f (M.empty, open, sid) symbols


shift :: State -> Symbol -> State
shift st s =
    let f xs (n, ls, rs) ctx = case (s, rs) of
            (N n1, r@(N n2) : rs') | n1 == n2 -> M.insert (n, ls ++ [r], rs') ctx xs
            (T t1, r@(T t2) : rs') | t1 == t2 -> M.insert (n, ls ++ [r], rs') ctx xs
            _                                 -> xs
    in  M.foldlWithKey' f M.empty st


closing :: State -> State
closing open = closing_ open M.empty


closing_ :: State -> State -> State
closing_ open closed | open == M.empty = closed
closing_ open closed                   =
    let close_state (open, closed) cr@(_, _, [])       ctx = (open, M.insertWith S.union cr ctx closed)
        close_state (open, closed) cr@(_, _, T _ : _)  ctx = (open, M.insertWith S.union cr ctx closed)
        close_state (open, closed) cr@(_, _, N n : ss) ctx =
            let ctx' =
                    let ctx1 = first1 ss
                    in  if S.member Tlambda ctx1 then S.union ctx1 ctx else ctx1
                f (xs, ys) r = case r of
                    N _ : _ ->
                        let xs' = case M.lookup (n, [], r) ys of
                                Just ctx'' ->
                                    let ctx''' = S.difference ctx' ctx''
                                    in  if ctx''' == S.empty then xs else M.insertWith S.union (n, [], r) ctx''' xs
                                Nothing    -> M.insertWith S.union (n, [], r) ctx' xs
                        in  (xs', ys)
                    _       ->
                        let ys' = M.insertWith S.union (n, [], r) ctx' ys
                        in  (xs, ys')
                closed' = M.insertWith S.union cr ctx closed
            in  foldl' f (open, closed') (rules n)
        (open', closed') = M.foldlWithKey' close_state (M.empty, closed) open
    in  closing_ open' closed'


{-
                        let xs' = case M.lookup (n, [], r) ys of
                                Just ctx'' | S.isSubsetOf ctx' ctx'' -> xs
                                _                                    -> M.insertWith S.union (n, [], r) ctx' xs
                        in  (xs', ys)
-}



