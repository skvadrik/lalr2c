module LALR
    ( lalr1_table
    ) where


import qualified Data.Set            as S
import qualified Data.HashMap.Strict as M
import           Data.List                (foldl')
import           Text.Printf

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
            let rs  = n2rules n
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
            let rs = filter (not . terminal_headed) (n2rules n)
                ys = foldl' (f xs) S.empty rs
            in  M.adjust (S.union ys) (N n) xs
        iterate xs =
            let xs' = S.foldl' update xs nonterminals
            in  if xs' == xs then xs else iterate xs'
        xs1 = S.foldl' init_terminal M.empty terminals
        xs2 = S.foldl' init_nonterminal xs1 nonterminals
    in  iterate xs2


lalr1_table :: LALR1Table
lalr1_table =
    let sid2st_s2sid = lalr1_state_table
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


report_sr_conflict :: RID -> RID -> String
report_sr_conflict i j = trace' $ printf "shift/reduce:  state %d vs (%d) %s" i j ((show . rid2rule) j)


report_rr_conflict :: RID -> RID -> String
report_rr_conflict i j = trace' $ printf "reduce/reduce: (%d) %s vs (%d) %s" i ((show . rid2rule) i) j ((show . rid2rule) j)


action_table :: LALR1State -> M.HashMap Symbol SID -> ActionTable
action_table state s2sid =
-- проверь эту хрень с точки зрения логики. шифты особенно. не должны ли они быть уникальны по построению и т.д.
    let try_insert act t2act t = case (M.lookup t t2act, act) of
            (Nothing,         _              ) -> M.insert t act t2act
            (Just Error,      _              ) -> M.insert t act t2act
            (Just (Shift i),  Shift j        ) | i == j -> t2act
            (Just (Shift i),  Shift j        ) | i /= j -> error "Shifting to different states"
            (Just (Shift i),  Reduce j       ) -> report_sr_conflict i j `seq` t2act
            (Just (Reduce i), Shift j        ) -> report_sr_conflict j i `seq` M.insert t act t2act
            (Just (Reduce i), act'@(Reduce j)) -> report_rr_conflict i j `seq`
                if (length . rid2rule) i < (length . rid2rule) j
                    then M.insert t act' t2act
                    else t2act
            (Just act',           _          ) -> raise_conflict t act' act
        f t2act cr@(rid, _) ctx =
            let v = case core2rhs cr of
                    _ | (cr, ctx) == lalr1_final_item -> Just (ctx, Accept)
                    []                                -> Just (ctx, Reduce rid)
                    T t : _                           -> case M.lookup (T t) s2sid of
                        Just sid -> Just (S.singleton t, Shift sid)
                        Nothing  -> error "missing goto for terminal"
                    _                                 -> Nothing
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


lalr1_final_item :: (Core, Context)
lalr1_final_item =
    let rid = case n2rids axiom of
            [rid'] -> rid'
            _      -> error "bad axiom rule in grammar"
        ctx = S.singleton (Tlambda)
    in  ((rid, 1), ctx)


lalr1_state_table :: LALR1StateTable
lalr1_state_table =
    let sids         = M.keys lr0_state_table
        (ss, s2sids) = ((\ (_, xs, ys) -> (xs, ys)) . unzip3 . M.elems) lr0_state_table
    in  ( M.fromList
        . zip sids
        . (\ xs -> zip3 xs ss s2sids)
        . M.elems
        . M.map
            ( lalr1_closing
            . M.map fst
            )
        ) lookahead_table


propagate :: LookaheadTable -> LookaheadTable
propagate m =
    let f3 ctx (m, end) (sid, cr) =
            let ctx' = (fst . M.lookupDefault undefined cr . M.lookupDefault undefined sid) m
            in  if S.isSubsetOf ctx ctx'
                    then (m, end)
                    else (M.adjust (M.adjust (\ (ctx', v) -> (S.union ctx' ctx, v)) cr) sid m, False)
        f2 (m, end) (ctx, sidcrs) = S.foldl' (f3 ctx) (m, end) sidcrs
        f1 (m, end) cr2ctx_sidcrs = M.foldl' f2 (m, end) cr2ctx_sidcrs
        f0 m = M.foldl' f1 (m, True) m
    in  case f0 m of
            (_,  True ) -> m
            (m', False) -> propagate m'


lookahead_table :: LookaheadTable
lookahead_table =
    let fold_context sid s2sid cr m cr'@(rid, pos) ctx = case core2rhs cr' of
            []    -> m
            s : _ ->
                let sid' = M.lookupDefault (error "missing sid for symbol") s s2sid
                    cr'' = (rid, pos + 1)

                    set_propagate m =
                        let update1 (v, propagates) = (v, S.insert (sid', cr'') propagates)
                            update2 m = M.adjust update1 cr m
                        in  M.adjust update2 sid m

                    set_lookahead m t =
                        let update1 (lookaheads, v) = (S.insert t lookaheads, v)
                            update2 m = M.adjust update1 cr'' m
                        in  M.adjust update2 sid' m

                    dispatch m Tnew = set_propagate m
                    dispatch m t    = set_lookahead m t

                in  S.foldl' dispatch m ctx

        fold_closing sid s2sid m cr st = M.foldlWithKey' (fold_context sid s2sid cr) m st

        fold_kernel (m, cr2st) sid (st, _, s2sid) =
            let st'               = lr0_kernel st
                (cr2st', cr2st'') = S.foldl' update_closings (cr2st, M.empty) st'
                m'                = M.foldlWithKey' (fold_closing sid s2sid) m cr2st''
            in  (m', cr2st')

        fold_table =
            let init_table = M.foldlWithKey'
                    (\ m sid (st, _, _) -> M.insert sid (S.foldl' (\ m cr -> M.insert cr (S.empty, S.empty) m) M.empty st) m
                    ) M.empty lr0_state_table
                init_table' = M.adjust (M.map (\ (lookahead, v) -> (S.insert Tlambda lookahead, v))) 1 init_table
            in  M.foldlWithKey' fold_kernel (init_table', M.empty)

    in  (propagate . fst . fold_table) lr0_state_table


update_closings :: (M.HashMap Core LALR1State, M.HashMap Core LALR1State) -> Core -> (M.HashMap Core LALR1State, M.HashMap Core LALR1State)
update_closings (cr2st, cr2st') cr = case M.lookup cr cr2st of
    Just st -> (cr2st, M.insert cr st cr2st')
    Nothing ->
        let st = lalr1_closing $ M.insert cr (S.singleton Tnew) M.empty
        in  (M.insert cr st cr2st, M.insert cr st cr2st')


lalr1_closing :: LALR1State -> LALR1State
lalr1_closing open = lalr1_closing_ open M.empty


lalr1_closing_ :: LALR1State -> LALR1State -> LALR1State
lalr1_closing_ open closed | open == M.empty = closed
lalr1_closing_ open closed                   =
    let close_state (open, closed) cr ctx = case core2rhs cr of
            []       -> (open, M.insertWith S.union cr ctx closed)
            T _ : _  -> (open, M.insertWith S.union cr ctx closed)
            N n : ss ->
                let ctx' =
                        let ctx1 = first1 ss
                        in  if S.member Tlambda ctx1 then S.union ctx1 ctx else ctx1
                    f (open, closed) rid =
                        let cr' = (rid, 0)
                        in  case (rid2rule rid, M.lookup cr' closed) of
                                (N _ : _, Just ctx'') | S.isSubsetOf ctx' ctx'' -> (open, closed)
                                (N _ : _, _         )                           -> (M.insertWith S.union cr' ctx' open, closed)
                                (_,       _         )                           -> (open, M.insertWith S.union cr' ctx' closed)
                    closed' = M.insertWith S.union cr ctx closed
                in  foldl' f (open, closed') (n2rids n)
        (open', closed') = M.foldlWithKey' close_state (M.empty, closed) open
    in  lalr1_closing_ open' closed'


lr0_kernel :: LR0State -> LR0State
lr0_kernel st =
    let is_kernel_item item = case item of
             i | i == lr0_init_item -> True
             (_, 0)                 -> False
             _                      -> True
    in  S.filter is_kernel_item st


lr0_init_item :: Core
lr0_init_item  = case n2rids axiom of
    [rid] -> (rid, 0)
    _     -> error "bad axiom rules in grammar"


lr0_empty_state :: LR0State
lr0_empty_state = S.empty


lr0_state_table :: LR0StateTable
lr0_state_table =
    let init_state  = (lr0_closing . S.singleton) lr0_init_item
        open        = M.insert init_state (1, N axiom) M.empty
        closed      = M.insert lr0_empty_state (0, T Tlambda) open
        result      = M.insert 0 (lr0_empty_state, T Tlambda, fgoto_empty) M.empty
        max         = 2
    in  lr0_state_table_ (open, closed, result, max)


fgoto_empty :: M.HashMap Symbol SID
fgoto_empty = S.foldl' (\ m s -> M.insert s 0 m) M.empty symbols


lr0_state_table_ :: (M.HashMap LR0State (SID, Symbol), M.HashMap LR0State (SID, Symbol), LR0StateTable, SID) -> LR0StateTable
lr0_state_table_ (open, _,      result, _  ) | open == M.empty = result
lr0_state_table_ (open, closed, result, max) =
    let fgoto (open, closed, result, max) st (sid, s) | st == S.empty =
            let result' = M.insert sid (st, s, fgoto_empty) result
            in  (open, closed, result', max)
        fgoto (open, closed, result, max) st (sid, s) =
            let f (s2sid, open, closed, max) s =
                    let st' = lr0_closing $ lr0_shift st s
                    in  case M.lookup st' closed of
                            Just (sid', s') | s' == s || st' == lr0_empty_state ->
                                ( M.insert s sid' s2sid
                                , open
                                , closed
                                , max
                                ) -- at start lambda leads to empty state
                            Nothing ->
                                ( M.insert s max s2sid
                                , M.insert st' (max, s) open
                                , M.insert st' (max, s) closed
                                , max + 1
                                )
                            _ -> error "transitions to the same state on multiple symbols"
                (s2sid, open', closed', max') = S.foldl' f (M.empty, open, closed, max) symbols
                result'                       = M.insert sid (st, s, s2sid) result
            in  (open', closed', result', max')
    in  lr0_state_table_ $ M.foldlWithKey' fgoto (M.empty, closed, result, max) open


lr0_shift :: LR0State -> Symbol -> LR0State
lr0_shift st s =
    let f xs cr@(rid, pos) = case (s, core2rhs cr) of
            (N n1, N n2 : _) | n1 == n2 -> S.insert (rid, pos + 1) xs
            (T t1, T t2 : _) | t1 == t2 -> S.insert (rid, pos + 1) xs
            _                           -> xs
    in  S.foldl' f S.empty st


lr0_closing :: LR0State -> LR0State
lr0_closing open = lr0_closing_ open S.empty


lr0_closing_ :: LR0State -> LR0State -> LR0State
lr0_closing_ open closed | open == S.empty = closed
lr0_closing_ open closed                   =
    let close_state (open, closed) cr = case core2rhs cr of
            []      -> (open, S.insert cr closed)
            T _ : _ -> (open, S.insert cr closed)
            N n : _ ->
                let f (open, closed) rid =
                        let cr' = (rid, 0)
                        in  case (rid2rule rid, S.member cr' closed) of
                                (_,       True ) -> (open, closed)
                                (N _ : _, False) -> (S.insert cr' open, closed)
                                (_,       False) -> (open, S.insert cr' closed)
                    closed' = S.insert cr closed
                in  foldl' f (open, closed') (n2rids n)
        (open', closed') = S.foldl' close_state (S.empty, closed) open
    in  lr0_closing_ open' closed'


core2rhs :: Core -> [Symbol]
core2rhs (rid, pos) = (drop pos . rid2rule) rid



