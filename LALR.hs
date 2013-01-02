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


{-
n2context :: M.HashMap NonTerminal (S.Set Terminal)
n2context = S.foldl' (\ m n -> M.insert n (first1 [N n]) m) M.empty nonterminals
-}


lalr1_table :: LALRTable
lalr1_table = lr1_to_lalr1 lr1_states


lr1_to_lalr1 :: S.Set State -> S.Set State
lr1_to_lalr1 states =
--    let unite_states st1 st2 = M.foldlWithKey' (\ st cr ctx -> M.insertWith S.union cr ctx st) st1 st2
    let unite_states st1 st2 = M.foldlWithKey'
            (\ st cr ctx -> case M.lookup cr st of
                Just ctx' -> M.insert cr (S.union ctx ctx') st
                Nothing   -> error "no appropriate cores found in merged states"
            ) st1 st2
        f cr2st st =
            let cr = (S.fromList . M.keys) st
            in  M.insertWith unite_states cr st cr2st
        core2state = S.foldl' f M.empty states
    in  (S.fromList . M.elems) core2state


lr1_states :: S.Set State
lr1_states =
    let f open closed | open == S.empty = closed
        f open closed =
            let closed' = S.union closed open
                open'   = (S.filter (\ st -> S.notMember st closed') . S.unions . map expand_state . S.toList) open
            in  f open' closed'
    in  f (S.singleton init_state) S.empty


init_state :: State
init_state =
    let r    = case rules axiom of
            [[ax']] -> [ax']
            _       -> error "bad axiom rule in grammar"
        ctx  = S.singleton (Tlambda)
        open = M.insert (axiom, [], r) ctx M.empty
    in  closing open M.empty


expand_state :: State -> S.Set State
expand_state state =
    let ss = (map T . S.toList) terminals ++ (map N . S.toList) nonterminals
        f states s =
            let st1 = shift state s
                st2 = closing st1 M.empty
            in  S.insert st2 states
    in  foldl' f S.empty ss


shift :: State -> Symbol -> State
shift state s =
    let f xs (n, ls, rs) ctx = case (s, rs) of
            (N n1, r@(N n2) : rs') | n1 == n2 -> M.insert (n, ls ++ [r], rs') ctx xs
            (T t1, r@(T t2) : rs') | t1 == t2 -> M.insert (n, ls ++ [r], rs') ctx xs
            _                                 -> xs
    in  M.foldlWithKey' f M.empty state


closing :: State -> State -> State
closing open closed | open == M.empty = closed
closing open closed                   =
    let close_state (open, closed) cr@(_, _, [])       ctx = (open, M.insertWith S.union cr ctx closed)
        close_state (open, closed) cr@(_, _, T _ : _)  ctx = (open, M.insertWith S.union cr ctx closed)
        close_state (open, closed) cr@(_, _, N n : ss) ctx =
            let ctx' = (S.unions . S.toList . S.map (first1 . (\ t -> ss ++ [T t]))) ctx
                f (xs, ys) r = case r of
                    N _ : _ ->
                        let xs' = case M.lookup (n, [], r) ys of
                                Nothing -> M.insertWith S.union (n, [], r) ctx' xs
                                _       -> xs
                        in  (xs', ys)
                    _       ->
                        let ys' = M.insertWith S.union (n, [], r) ctx' ys
                        in  (xs, ys')
                closed' = M.insertWith S.union cr ctx closed
            in  foldl' f (open, closed') (rules n)
        (open', closed') = M.foldlWithKey' close_state (M.empty, closed) open
    in  closing open' closed'



