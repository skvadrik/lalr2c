module LALR
    ( lalr1_table
    ) where


import qualified Data.Set            as S
import qualified Data.HashMap.Strict as M
import           Data.List                (foldl', partition)
import           Control.DeepSeq

import           Types
import           Grammar


first1 :: [Symbol] -> S.Set Terminal
first1 ss = first1_ (S.singleton ss) S.empty

{-
-- кэш прикрути
first1_ :: [[Symbol]] -> S.Set Terminal -> S.Set Terminal
first1_ []                  result = result
first1_ ([] : open)         result = first1_ open (S.insert (T "lambda") result)
first1_ ((N n : ss) : open) result =
    let terminal_headed ss = case ss of
            T _ : _ -> True
            _       -> False

        cut_after_terminal []             = []
        cut_after_terminal (t@(T _) : _)  = [t]
        cut_after_terminal (n@(N _) : ss) = n : cut_after_terminal ss

        (xs, ys) = (partition terminal_headed . map (++ ss)) (rules n)
        result1  = foldl' (\ ts (t : _) -> S.insert t ts) result xs
        open1    = (S.toList . S.fromList) $ open' ++ map cut_after_terminal ys
    in  first1_ open1 result1
first1_ _                   _      = error "terminal-headed sequence in open set"
-}

first1_ :: S.Set [Symbol] -> S.Set Terminal -> S.Set Terminal
first1_ open result | open == S.empty = result
first1_ open result                   = trace' open `seq`
    let terminal_headed ss = case ss of
            T _ : _ -> True
            _       -> False

        cut_after_terminal []             = []
        cut_after_terminal (t@(T _) : _)  = [t]
        cut_after_terminal (n@(N _) : ss) = n : cut_after_terminal ss

        f (open, result) []         = (open, S.insert (Tlambda) result)
        f (open, result) (T t : _)  = (open, S.insert t result)
        f (open, result) (N n : ss) =
            let (xs, ys) = (partition terminal_headed . map (++ ss)) (rules n)
                result'  = foldl' (\ ts (T t : _) -> S.insert t ts) result xs
                open'    = (S.fromList . map cut_after_terminal) ys
            in  (S.union open open', result')

        (open', result') = S.foldl' f (S.empty, result) open
    in  first1_ open' result'


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
    let close_state (open, closed) cr@(_, _, N n : ss) ctx =
            let ctx' = (S.unions . S.toList . S.map (first1 . (\ t -> ss ++ [T t]))) ctx
                f (xs, ys) r = case r of
                    N _ : _ -> (M.insertWith S.union (n, [], r) ctx' xs, ys)
                    _       -> (xs, M.insertWith S.union (n, [], r) ctx' ys)
                closed' = M.insertWith S.union cr ctx closed
            in  foldl' f (open, closed') (rules n)
        close_state _ (_, _, _) _ = error "terminal-headed or empty rhs sequence "
        (open', closed') = M.foldlWithKey' close_state (M.empty, closed) open
    in  closing open' closed'



