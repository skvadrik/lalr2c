module LALR
    ( lalr1_table
    ) where


import qualified Data.Set            as S
import qualified Data.HashMap.Strict as M
import           Data.List                (foldl')

import           Types
import           Grammar


first1 :: [Symbol] -> S.Set Terminal
first1 []       = S.singleton Tlambda
first1 (s : ss) =
    let ts = M.lookupDefault (error "yikes") s symbol_to_first1
    in  if S.member Tlambda ts
            then S.union ts (first1 ss)
            else ts


symbol_to_first1 :: M.HashMap Symbol (S.Set Terminal)
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
--lalr1_table = lr1_goto_table
lalr1_table = lr1_to_lalr1 lr1_goto_table


lr1_to_lalr1 :: GotoTable -> GotoTable
lr1_to_lalr1 goto_tbl =
    let unite_states st1 st2 = M.foldlWithKey' (\ st cr ctx -> M.insertWith S.union cr ctx st) st1 st2
        f1 cr2st st =
            let cr = (S.fromList . M.keys) st
            in  M.insertWith unite_states cr st cr2st
        states     = M.keys goto_tbl
        core2state = foldl' f1 M.empty states
        f2 st2st st =
            let cr  = (S.fromList . M.keys) st
                st' = M.lookupDefault (error "cant't find state for core") cr core2state
            in  M.insert st st' st2st
        state2state = foldl' f2 M.empty states
        f3 tbl st s2st =
            let lookup st = M.lookupDefault (error "cant't find state for state") st state2state
                unite_goto s2st1 s2st2 = M.foldlWithKey' (\ s2st s st -> M.insertWith unite_states s st s2st) s2st1 s2st2
                st'   = lookup st
                s2st' = M.map lookup s2st
            in  M.insertWith unite_goto st' s2st' tbl
        goto_tbl' = M.foldlWithKey' f3 M.empty goto_tbl
    in  goto_tbl'


lr1_goto_table :: GotoTable
lr1_goto_table =
    let g (open, closed) st =
            let s2st    = goto_function st
                open'   = (S.union open . S.filter (\ st -> M.lookup st closed == Nothing) . S.fromList . M.elems) s2st
                closed' = M.insert st s2st closed
            in  (open', closed')
        f open closed | open == S.empty = closed
        f open closed =
            let (open', closed') = S.foldl' g (S.empty, closed) open
            in  f open' closed'
    in  f (S.singleton init_state) M.empty


init_state :: State
init_state =
    let r    = case rules axiom of
            [[ax']] -> [ax']
            _       -> error "bad axiom rule in grammar"
        ctx  = S.singleton (Tlambda)
        open = M.insert (axiom, [], r) ctx M.empty
    in  closing open


goto_function :: State -> M.HashMap Symbol State
goto_function state | state == M.empty = M.empty
goto_function state =
    let f s2st s =
            let st = closing $ shift state s
            in  M.insert s st s2st
    in  S.foldl' f M.empty symbols


shift :: State -> Symbol -> State
shift state s =
    let f xs (n, ls, rs) ctx = case (s, rs) of
            (N n1, r@(N n2) : rs') | n1 == n2 -> M.insert (n, ls ++ [r], rs') ctx xs
            (T t1, r@(T t2) : rs') | t1 == t2 -> M.insert (n, ls ++ [r], rs') ctx xs
            _                                 -> xs
    in  M.foldlWithKey' f M.empty state


closing :: State -> State
closing open = closing_ open M.empty


closing_ :: State -> State -> State
closing_ open closed | open == M.empty = closed
closing_ open closed                   =
    let close_state (open, closed) cr@(_, _, [])       ctx = (open, M.insertWith S.union cr ctx closed)
        close_state (open, closed) cr@(_, _, T _ : _)  ctx = (open, M.insertWith S.union cr ctx closed)
        close_state (open, closed) cr@(_, _, N n : ss) ctx =
            let ctx' = (S.unions . S.toList . S.map (first1 . (\ t -> ss ++ [T t]))) ctx
                f (xs, ys) r = case r of
                    N _ : _ ->
                        let xs' = case M.lookup (n, [], r) ys of
                                Just ctx'' | S.isSubsetOf ctx' ctx'' -> xs
                                _                                    -> M.insertWith S.union (n, [], r) ctx' xs
                        in  (xs', ys)
                    _       ->
                        let ys' = M.insertWith S.union (n, [], r) ctx' ys
                        in  (xs, ys')
                closed' = M.insertWith S.union cr ctx closed
            in  foldl' f (open, closed') (rules n)
        (open', closed') = M.foldlWithKey' close_state (M.empty, closed) open
    in  closing_ open' closed'



