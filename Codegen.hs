module Codegen
    ( codegen
    ) where


import           Data.Char                        (toUpper, isAlphaNum, isDigit)
import qualified Data.HashMap.Strict as M  hiding (lookupDefault)
import qualified Data.HashMap.Lazy   as M         (lookupDefault)
import qualified Data.Set            as S
import           Data.List                        (delete, sort, sortBy)
import           Data.Function                    (on)
import           Data.Maybe                       (isJust)
import qualified Text.PrettyPrint    as PP
import           Text.PrettyPrint                 (Doc, ($$), (<>), ($+$))

import Grammar
import Types


codegen :: LALR1Table -> Verbosity -> FilePath -> FilePath -> IO ()
codegen tbl v fsource fheader = do
    writeFile fheader $ PP.render $
        wrap_in_ifndefs fheader doc_tokens
    writeFile fsource $ PP.render $
        maybe_doc_code entry
        $$$ PP.nest 4 (doc_parser tbl v)
        $$$ maybe_doc_code ending


wrap_in_ifndefs :: FilePath -> Doc -> Doc
wrap_in_ifndefs hdr d =
    let doc_hdr =
           ( (PP.text "__" <>)
           . (<> PP.text "__")
           . PP.text
           . map toUpper
           . takeWhile isAlphaNum
           ) hdr
    in  PP.text "#ifndef " <> doc_hdr
        $$ PP.text "#define " <> doc_hdr
        $$$ d
        $$$ PP.text "#endif // " <> doc_hdr


doc_tokens :: Doc
doc_tokens =
    let doc_macro_name_token  = PP.text "LALR2C_TOKEN"
        doc_macro_name_tokens = PP.text "LALR2C_TOKENS"
        doc_define            = PP.text "#define "
        doc_carry             = PP.text " \\"
        doc_wrap_one_terminal =
            ( (<> doc_carry)
            . (doc_macro_name_token <>)
            . PP.parens
            . doc_terminal
            )
        doc_wrap_terminals =
            ( PP.vcat
            . map doc_wrap_one_terminal
            . S.toList
            . S.filter (/= New)
            ) terminals
    in  doc_define <> doc_macro_name_tokens <> doc_carry
        $$ PP.nest 4 doc_wrap_terminals


doc_parser :: LALR1Table -> Verbosity -> Doc
doc_parser tbl v =
    let sids = (delete 0 . sort . M.keys) tbl
        doc_dispatch_states =
            ( PP.vcat
            . map (\ (sid, (s, acts, _)) -> doc_dispatch_state v sid s acts)
            . filter ((/= 0) . fst)
            . sortBy (compare `on` fst)
            . M.toList
            ) tbl
        doc_dispatch_rules =
            ( PP.vcat
            . map (doc_dispatch_rule v)
            ) rules
        doc_dispatch_nonterminals =
            ( PP.vcat
            . map (doc_dispatch_nonterminal v tbl)
            . S.toList
            ) nonterminals
    in  doc_parser_entry sids
        $$$ doc_dispatch_states
        $$$ doc_dispatch_rules
        $$$ doc_dispatch_nonterminals
        $$$ maybe_doc_fin


doc_parser_entry :: [SID] -> Doc
doc_parser_entry sids = case freeze of
    Nothing -> doc_goto_state 1
    Just _  ->
        let di = doc_is_1st_call
            dt = doc_assign doc_is_1st_call (PP.text "false") $$ doc_goto_state 1
            de =
                let doc_guard = doc_stack_state 0
                    doc_cases =
                        ( PP.vcat
                        . map (\ sid -> doc_case (PP.int sid) (doc_goto_state sid))
                        ) sids
                in  doc_switch doc_guard doc_cases
        in  PP.text "static bool " <> doc_assign doc_is_1st_call (PP.text "true")
            $$ doc_ifthenelse di dt de


doc_dispatch_state :: Verbosity -> SID -> Symbol -> ActionTable -> Doc
doc_dispatch_state _ sid s t2act =
    let act2ts =
            let group_by_act m t act = M.insertWith S.union act (S.singleton t) m
            in  M.foldlWithKey' group_by_act M.empty t2act
        doc_normal_case (act, ts) =
            let doc_guards = (map doc_terminal . S.toList) ts
            in  case act of
                    Shift sid' _ -> doc_multicase doc_guards (doc_goto_state sid')
                    Reduce rid _ -> doc_multicase doc_guards (doc_goto_reduce rid)
                    Accept       -> doc_multicase doc_guards doc_success_action
                    Error        -> PP.empty {- these go to default case -}
        doc_normal_cases =
            ( PP.vcat
            . map doc_normal_case
            . M.toList
            ) act2ts
        doc_freeze_case = case freeze of
            Just (t, c) -> doc_case (PP.text t) (PP.text c)
            Nothing     -> doc_goto_fin
        doc_dispatch = doc_switch
            doc_token_type
            ( doc_normal_cases
            $$ doc_freeze_case
            $$ doc_default doc_failure_action
            )
        maybe_doc_shift = if is_terminal s {- true for shift states only -}
            then
                doc_assign (doc_stack_semantics 0) doc_token_semantics
                $$ doc_shift_token
            else PP.empty
    in  doc_decl_state sid
        $$ PP.nest 4
            ( doc_assign (doc_stack_state 0) (doc_sid sid)
            $$ maybe_doc_shift
            $$ doc_reserve_stack
            $$ doc_dispatch
            )


doc_dispatch_rule :: Verbosity -> (RID, NonTerminal, [Symbol], Maybe Code) -> Doc
doc_dispatch_rule _ (rid, n, ss, c) =
    doc_decl_reduce rid
    $$ PP.nest 4
        ( doc_user_code (length ss) c
        $$ doc_goto_nonterminal n
        )


doc_user_code :: Int -> Maybe Code -> Doc
doc_user_code n Nothing     = doc_pop_stack n
doc_user_code n (Just code) =
    let subst ""               = PP.empty
        subst ('$' : '$' : xs) = doc_tmp_semantics <> subst xs
        subst ('$' : xs)       = case break (not . isDigit) xs of
            ("",  _  ) -> PP.char '$' <> subst xs
            (xs1, xs2) -> doc_stack_semantics (n + 1 - read xs1) <> subst xs2
        subst (x : xs)         = PP.char x <> subst xs
    in  subst code
        $$ doc_pop_stack n
        $$ doc_assign (doc_stack_semantics 0) doc_tmp_semantics


doc_dispatch_nonterminal :: Verbosity -> LALR1Table -> NonTerminal -> Doc
doc_dispatch_nonterminal _ tbl n =
    let f m sid (_, _, gotos) =
            let sid' = M.lookupDefault (error "can't find nonterminal") n gotos
            in  M.insertWith S.union sid' (S.singleton sid) m
        sid2sids = M.foldlWithKey' f M.empty tbl
        doc_one_case (sid, sids) =
            let doc_guards = (map doc_sid . S.toList) sids
            in  if sid == 0 {- zero state is the empty state -}
                    then PP.empty
                    else doc_multicase doc_guards (doc_goto_state sid)
        doc_cases =
            ( PP.vcat
            . map doc_one_case
            . M.toList
            ) sid2sids
        doc_dispatch = doc_switch
            (doc_stack_state 1)
            (doc_cases $$ doc_default doc_failure_action)
    in  doc_decl_nonterminal n
        $$ PP.nest 4 doc_dispatch


maybe_doc_code :: Maybe Code -> Doc
maybe_doc_code Nothing  = PP.empty
maybe_doc_code (Just c) = PP.text c


maybe_doc_fin :: Doc
maybe_doc_fin =
    if isJust success || isJust failure || isJust freeze
        then doc_decl_fin
        else PP.empty


-- User-defined symbols
doc_token_type :: Doc
doc_token_type = PP.text "LALR2C_R_TOKEN_TYPE"

doc_token_semantics :: Doc
doc_token_semantics = PP.text "LALR2C_R_TOKEN_SEMANTICS"

doc_stack_state :: Int -> Doc
doc_stack_state n = PP.text "LALR2C_RW_STACK_STATE " <> PP.parens (PP.int n)

doc_stack_semantics :: Int -> Doc
doc_stack_semantics n = PP.text "LALR2C_RW_STACK_SEMANTICS " <> PP.parens (PP.int n)

doc_shift_token :: Doc
doc_shift_token = PP.text "LALR2C_E_SHIFT_TOKEN ();"

doc_reserve_stack :: Doc
doc_reserve_stack = PP.text "LALR2C_E_RESERVE_STACK ();"

doc_pop_stack :: Int -> Doc
doc_pop_stack n = PP.text "LALR2C_E_STACK_POP " <> PP.parens (PP.int n) <> PP.semi

doc_tmp_semantics :: Doc
doc_tmp_semantics = PP.text "LALR2C_RW_TMP_SEMANTICS"


-- Internal symbol
doc_is_1st_call :: Doc
doc_is_1st_call = PP.text "LALR2C_IS_1ST_CALL"


-- Internal types
doc_sid :: SID -> Doc
doc_sid = PP.int

doc_rid :: RID -> Doc
doc_rid = PP.int

doc_nonterminal :: NonTerminal -> Doc
doc_nonterminal = PP.text . show

doc_terminal :: Terminal -> Doc
doc_terminal = PP.text . tail . show


-- Labels
doc_label :: Doc -> Doc
doc_label d = PP.text "lalr2c_" <> d

doc_label_state :: SID -> Doc
doc_label_state sid = doc_label (PP.text "state_" <> doc_sid sid)

doc_label_reduce :: RID -> Doc
doc_label_reduce rid = doc_label (PP.text "reduce_" <> doc_rid rid)

doc_label_nonterminal :: NonTerminal -> Doc
doc_label_nonterminal n = doc_label (PP.text "nonterminal_" <> doc_nonterminal n)

doc_label_fin :: Doc
doc_label_fin = doc_label (PP.text "fin")


-- Gotos
doc_goto :: Doc -> Doc
doc_goto d = PP.text "goto " <> d <> PP.semi

doc_goto_state :: SID -> Doc
doc_goto_state sid = doc_goto $ doc_label_state sid

doc_goto_reduce :: RID -> Doc
doc_goto_reduce rid = doc_goto $ doc_label_reduce rid

doc_goto_nonterminal :: NonTerminal -> Doc
doc_goto_nonterminal n = doc_goto $ doc_label_nonterminal n

doc_goto_fin :: Doc
doc_goto_fin = doc_goto doc_label_fin


-- Label declarations
doc_decl :: Doc -> Doc
doc_decl d = d <> PP.colon

doc_decl_state :: SID -> Doc
doc_decl_state sid = doc_decl $ doc_label_state sid

doc_decl_reduce :: RID -> Doc
doc_decl_reduce rid = doc_decl $ doc_label_reduce rid

doc_decl_nonterminal :: NonTerminal -> Doc
doc_decl_nonterminal n = doc_decl $ doc_label_nonterminal n

doc_decl_fin :: Doc
doc_decl_fin = doc_decl doc_label_fin


doc_assign :: Doc -> Doc -> Doc
doc_assign d1 d2 = d1 <> PP.text " = " <> d2 <> PP.semi


doc_success_action :: Doc
doc_success_action = case success of
    Just sc -> PP.text sc
    Nothing -> doc_goto_fin


doc_failure_action :: Doc
doc_failure_action = case failure of
    Just fl -> PP.text fl
    Nothing -> doc_goto_fin


is_terminal :: Symbol -> Bool
is_terminal (T _) = True
is_terminal _     = False


($$$) :: Doc -> Doc -> Doc
($$$) d1 d2 | PP.isEmpty d1 = d2
($$$) d1 d2 | PP.isEmpty d2 = d1
($$$) d1 d2                 = d1 $$ PP.text "" $$ d2
infixl 5 $$$

{-
verbose :: Verbosity -> Doc -> Doc
verbose v d = case v of
    V0 -> PP.empty
    V1 -> d


doc_verbose :: Verbosity -> Doc -> Doc
doc_verbose v d = verbose v $ doc_ifthen (PP.text "VERBOSE") d
-}

wrap_in_braces :: Doc -> Doc
wrap_in_braces d =
    PP.text "{"
    $+$ PP.nest 4 d
    $$ PP.text "}"

{-
doc_ifthen :: Doc -> Doc -> Doc
doc_ifthen d1 d2 =
    PP.text "if " <> PP.parens d1
    $$ wrap_in_braces d2
-}

doc_ifthenelse :: Doc -> Doc -> Doc -> Doc
doc_ifthenelse d1 d2 d3 =
    PP.text "if " <> PP.parens d1
    $$ wrap_in_braces d2
    $$ PP.text "else"
    $$ wrap_in_braces d3

{-
doc_while :: Doc -> Doc -> Doc
doc_while d1 d2 =
    PP.text "while " <> (PP.parens d1)
    $$ (wrap_in_braces d2)
-}

doc_switch :: Doc -> Doc -> Doc
doc_switch d1 d2 =
    PP.text "switch " <> (PP.parens d1)
    $$ (wrap_in_braces d2)


doc_case :: Doc -> Doc -> Doc
doc_case d1 d2 =
    PP.text "case " <> d1 <> PP.colon
    $$ PP.nest 4 d2

{-
doc_casebreak :: Doc -> Doc -> Doc
doc_casebreak d1 d2 =
    PP.text "case " <> d1 <> PP.colon
    $$ PP.nest 4 (d2 $$ PP.text "break;")
-}

doc_multicase :: [Doc] -> Doc -> Doc
doc_multicase ds d =
    (PP.vcat $ map (\ d -> PP.text "case " <> d <> PP.colon) ds)
    $$ PP.nest 4 d

{-
doc_multicasebreak :: [Doc] -> Doc -> Doc
doc_multicasebreak ds d =
    (PP.vcat $ map (\ d -> PP.text "case " <> d <> PP.colon) ds)
    $$ PP.nest 4 (d $$ PP.text "break;")
-}

doc_default :: Doc -> Doc
doc_default d = PP.text "default:" $$ PP.nest 4 d


