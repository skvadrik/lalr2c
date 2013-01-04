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
lalr1_table =
--    let states = state_table
    let states = lr1_to_lalr1 state_table
        f tbl st =
            let s2st    = M.lookupDefault (error "missing state in state table") st states
                actions = action_table st s2st
                gotos   = goto_table s2st
            in  M.insert st (actions, gotos) tbl
    in  foldl' f M.empty (M.keys states)


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


action_table :: State -> M.HashMap Symbol State -> ActionTable
action_table st s2st =
    let try_insert act t2act t = case M.lookup t t2act of
            Nothing    -> M.insert t act t2act
            Just Error -> M.insert t act t2act
            Just Shift | (case act of { Shift -> True; _ -> False }) -> t2act
            Just act'  -> raise_conflict t act' act
        f t2act cr ctx =
            let v = case cr of
                    (_, _, T t : _)                  -> Just (S.singleton t, Shift)
                    (n, ls, []) | n /= axiom         -> Just (ctx, Reduce n ls)
                    _ | (cr, ctx) == final_situation -> Just (ctx, Accept)
                    _                                -> Nothing
            in  case v of
                    Just (ts, act) -> S.foldl' (try_insert act) t2act ts
                    Nothing        -> t2act
        t2act = S.foldl' (\ t2act' t -> M.insert t Error t2act') M.empty terminals
    in  M.foldlWithKey' f t2act st
--                    (n, ls, T t : rs)               -> Just (S.singleton t, Shift)
--            let st' = M.lookupDefault (error "can't find state in goto map") (T t) s2st


goto_table :: M.HashMap Symbol State -> GotoTable
goto_table s2st =
    let f m (N n) v = M.insert n v m
        f m _     _ = m
    in  M.foldlWithKey' f M.empty s2st


lr1_to_lalr1 :: StateTable -> StateTable
lr1_to_lalr1 state_tbl =
    let unite_states st1 st2 = M.foldlWithKey' (\ st cr ctx -> M.insertWith S.union cr ctx st) st1 st2
        f1 cr2st st =
            let cr = (S.fromList . M.keys) st
            in  M.insertWith unite_states cr st cr2st
        states     = M.keys state_tbl
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
        state_tbl' = M.foldlWithKey' f3 M.empty state_tbl
    in  state_tbl'


state_table :: StateTable
state_table =
    let g (open, closed) st =
            let s2st    = state_function st
                open'   = (S.union open . S.filter (\ st -> M.lookup st closed == Nothing) . S.fromList . M.elems) s2st
                closed' = M.insert st s2st closed
            in  (open', closed')
        f open closed | open == S.empty = closed
        f open closed =
            let (open', closed') = S.foldl' g (S.empty, closed) open
            in  f open' closed'
    in  f (S.singleton init_state) M.empty


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


state_function :: State -> M.HashMap Symbol State
state_function state | state == M.empty = M.empty
state_function state =
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



