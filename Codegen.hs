module Codegen
    ( codegen
    ) where


import           Data.Char                 (toUpper, isAlphaNum, isDigit)
import qualified Data.HashMap.Strict as M
import qualified Data.Set            as S
import           Data.List                 (foldl')
import qualified Text.PrettyPrint    as PP
import           Text.PrettyPrint          (Doc, ($$), (<>), ($+$))

import Grammar
import Types


codegen :: LALR1Table -> Verbosity -> FilePath -> FilePath -> IO ()
codegen tbl v fcpp fh = do
    writeFile fh $ PP.render $ wrap_in_define fh $
        doc_defines
        $$$ doc_tokens
        $$$ doc_protos
    writeFile fcpp $ PP.render $
        doc_includes fh
        $$$ (verbose v $ doc_print_stack $$$ doc_print_buffer)
        $$$ doc_parse tbl v


wrap_in_define :: FilePath -> Doc -> Doc
wrap_in_define fp d =
    let d' = PP.text $ '_' : map (\ c -> if isAlphaNum c then toUpper c else '_') fp
    in  PP.text "#ifndef " <> d'
        $$ PP.text "#define " <> d'
        $$$ d
        $$$ PP.text "#endif // " <> d'


doc_includes :: FilePath -> Doc
doc_includes fp =
    PP.text "#include <stdio.h>"
    $$$ PP.text "#include \"" <> PP.text fp <> PP.char '"'


doc_defines :: Doc
doc_defines =
    PP.text "#define STACK_SIZE     1024"
    $$ PP.text "#define VERBOSE        false"
    $$ PP.text "#define N_BUFFER_DEBUG 1"


doc_tokens :: Doc
doc_tokens =
    let tokens =
            ( PP.vcat
            . map
                ( (\ t -> PP.text "TOKEN(" <> PP.text t <> PP.text ") \\") 
                . show
                )
            . S.toList
            ) terminals
    in  PP.text "#define TOKENS \\"
        $$ PP.nest 4 tokens
        $$$ PP.text "#define TOKEN(x) #x,"
        $$ PP.text "static const char * token_names [] ="
        $$ ( wrap_in_braces $ PP.text "TOKENS") <> PP.semi
        $$ PP.text "#undef TOKEN"
        $$$ PP.text "#define TOKEN(x) x,"
        $$ PP.text "enum Token"
        $$ wrap_in_braces
            ( PP.text "TOKENS"
            $$ PP.text "TOKEN_NUMBER"
            ) <> PP.semi
        $$ PP.text "#undef TOKEN"
        $$$ PP.text "struct StackType"
        $$ wrap_in_braces
            ( PP.text "int state;"
            $$ PP.text "Semantics semantics;"
            ) <> PP.semi


doc_protos :: Doc
doc_protos =
    doc_parse_signature <> PP.semi
    $$ doc_print_buffer_signature <> PP.semi
    $$ doc_print_stack_signature <> PP.semi


doc_print_stack_signature :: Doc
doc_print_stack_signature =
    PP.text "void print_stack (const StackType * bottom, StackType * top)"


doc_print_stack :: Doc
doc_print_stack =
    doc_print_stack_signature
    $$ wrap_in_braces
        ( PP.text "StackType * p = top;"
        $$ PP.text "printf (\"stack:\\n\");"
        $$ doc_while (PP.text "p - bottom >= 0") (PP.text "printf (\"\\t%d\\t%d\\n\", p->state, p->semantics);" $$ PP.text "--p;")
        )


doc_print_buffer_signature :: Doc
doc_print_buffer_signature =
    PP.text "void print_buffer (Token * begin, Token * end)"


doc_print_buffer :: Doc
doc_print_buffer =
    doc_print_buffer_signature
    $$ wrap_in_braces
        ( PP.text "Token * p = begin;"
        $$ PP.text "printf (\"buffer:\");"
        $$ doc_while (PP.text "end - p > 0") (PP.text "printf (\"\\t%s\\n\", token_names [p++->type]);")
        )


doc_parse :: LALR1Table -> Verbosity -> Doc
doc_parse tbl v =
    doc_parse_signature
    $$ wrap_in_braces
        ( doc_init_stack
        $$$ doc_goto (PP.text "state_" <> PP.int 1)
        $$$ M.foldlWithKey' (\ doc sid (s, actions, gotos) -> doc $$ (doc_state v) sid s actions gotos) PP.empty tbl
        $$$ foldl' (\ doc (rid, n, r, c) -> doc $$ (doc_rule v tbl) rid n r c) PP.empty rules
        )

-- Maybe implement codegen modes as cmd-key controlled features (save-space, save-time, normal)

doc_parse_signature :: Doc
doc_parse_signature = PP.text "bool parse (Token * p)"


doc_init_stack :: Doc
doc_init_stack =
    PP.text "StackType * stack = new StackType "
    <> PP.brackets (PP.text "STACK_SIZE")
    <> PP.semi
    $$ PP.text "const StackType * stack_bottom = &stack[0];"
    $$ PP.text "U32B semantics;"


doc_goto :: Doc -> Doc
doc_goto d = PP.text "goto " <> d <> PP.semi


is_terminal :: Symbol -> Bool
is_terminal (T _) = True
is_terminal _     = False


doc_state :: Verbosity -> SID -> Symbol -> ActionTable -> GotoTable -> Doc
doc_state v sid s t2act _ =
    let d0 = PP.text "p->type"
        f ts act =
            let d = (map (PP.text . show) . S.toList) ts
            in  case act of
                    Shift sid' -> doc_multicase d $ doc_goto (PP.text "state_" <> PP.int sid')
                    Reduce rid -> doc_multicase d $ doc_goto (PP.text "reduce_" <> PP.int rid)
                    Accept     -> doc_multicase d $
                        (doc_verbose v $ PP.text "printf (\"SUCCESS\\n\");")
                        $$ PP.text "return true;"
                    Error       -> PP.empty
        act2ts = M.foldlWithKey' (\ m t act -> M.insertWith S.union act (S.singleton t) m) M.empty t2act
        d1 = M.foldlWithKey' (\ doc act ts -> doc $$ f ts act) PP.empty act2ts
        d2 = doc_default $
            (doc_verbose v $ PP.text "printf (\"FAIL\\n\");")
            $$ PP.text "return false;"
    in  PP.text "state_" <> PP.int sid <> PP.colon
        $$ PP.nest 4
            ( PP.text "stack->state = " <> PP.int sid <> PP.semi
            $$ (doc_verbose v $ PP.text "printf (\"pushed %d\\n\", stack->state);")
            $$
                ( if is_terminal s
                    then
                        PP.text "stack->semantics = p->semantics;"
                        $$ (doc_verbose v $ PP.text "printf (\"pushed semantics %d\\n\", p->semantics);")
                        $$ PP.text "p++;"
                        $$ (doc_verbose v $ PP.text "printf (\"shifting %s\\n\", token_names[p->type]);")
                    else PP.empty
                )
            $$ PP.text "stack++;"
            $$ (doc_verbose v $ PP.text "print_buffer (p, p + N_BUFFER_DEBUG);")
            $$ doc_switch d0 (d1 $$ d2)
            )


doc_rule :: Verbosity -> LALR1Table -> RID -> NonTerminal -> [Symbol] -> Code -> Doc
doc_rule v tbl rid n ss c =
    PP.text "reduce_" <> PP.int rid <> PP.colon
    $$ PP.nest 4
        ( (doc_verbose v $ PP.text "print_stack (stack_bottom, stack - 1);")
        $$ doc_user_code (length ss) c
        $$ (doc_verbose v $ PP.text "printf (\"" <> PP.text (show n) <> PP.text " ----> " <> PP.text (concatMap show ss) <> PP.text "\\n\");")
        $$ (doc_verbose v $ PP.text "printf (\"popped " <> PP.int (length ss) <> PP.text " symbols\\n\");")
        $$ doc_nonterminal v tbl n
        )


doc_user_code :: Int -> Code -> Doc
doc_user_code n "{}" = PP.text "stack -= " <> PP.int n <> PP.semi
doc_user_code n code =
    let subst ""               = ""
        subst ('$' : '$' : xs) = "semantics" ++ subst xs
        subst ('$' : xs)       = case break (not . isDigit) xs of
            ("",  _  ) -> '$' : subst xs
            (xs1, xs2) -> "(stack - " ++ show (n + 1 - read xs1) ++ ")->semantics" ++ subst xs2
        subst (x : xs)         = x : subst xs
    in  (PP.text . subst) code
        $$ PP.text "stack -= " <> PP.int n <> PP.semi
        $$ PP.text "stack->semantics = semantics;"


doc_nonterminal :: Verbosity -> LALR1Table -> NonTerminal -> Doc
doc_nonterminal v tbl n =
    let d0 = PP.text "(stack - 1)->state"
        f1 m sid (_, _, gotos) =
            let sid' = M.lookupDefault (error "can't find nonterminal") n gotos
            in  M.insertWith S.union sid' (S.singleton sid) m
        sid2sids = M.foldlWithKey' f1 M.empty tbl
        f2 0   _    = PP.empty
        f2 sid sids =
            let d = (map (PP.text . show) . S.toList) sids
            in  doc_multicasebreak d $ doc_goto (PP.text "state_" <> PP.int sid)
        d1 = M.foldlWithKey' (\ doc sid sids -> doc $$ f2 sid sids) PP.empty sid2sids
        d2 = doc_default $ doc_goto (PP.text "state_0")
    in  (doc_verbose v $ PP.text "print_stack (stack_bottom, stack - 1);")
        $$ doc_switch d0 (d1 $$ d2)


----------------------------------------------------------------------


($$$) :: Doc -> Doc -> Doc
($$$) d1 d2 | PP.isEmpty d1 = d2
($$$) d1 d2 | PP.isEmpty d2 = d1
($$$) d1 d2                 = d1 $$ PP.text "" $$ d2
infixl 5 $$$


verbose :: Verbosity -> Doc -> Doc
verbose v d = case v of
    V0 -> PP.empty
    V1 -> d


doc_verbose :: Verbosity -> Doc -> Doc
doc_verbose v d = verbose v $ doc_ifthen (PP.text "VERBOSE") d


wrap_in_braces :: Doc -> Doc
wrap_in_braces d =
    PP.text "{"
    $+$ PP.nest 4 d
    $$ PP.text "}"


doc_ifthen :: Doc -> Doc -> Doc
doc_ifthen d1 d2 =
    PP.text "if " <> PP.parens d1
    $$ wrap_in_braces d2

{-
doc_ifthenelse :: Doc -> Doc -> Doc -> Doc
doc_ifthenelse d1 d2 d3 =
    PP.text "if " <> PP.parens d1
    $$ wrap_in_braces d2
    $$ PP.text "else"
    $$ wrap_in_braces d3
-}

doc_while :: Doc -> Doc -> Doc
doc_while d1 d2 =
    PP.text "while " <> (PP.parens d1)
    $$ (wrap_in_braces d2)


doc_switch :: Doc -> Doc -> Doc
doc_switch d1 d2 =
    PP.text "switch " <> (PP.parens d1)
    $$ (wrap_in_braces d2)

{-
doc_case :: Doc -> Doc -> Doc
doc_case d1 d2 =
    PP.text "case " <> d1 <> PP.colon
    $$ PP.nest 4 d2


doc_casebreak :: Doc -> Doc -> Doc
doc_casebreak d1 d2 =
    PP.text "case " <> d1 <> PP.colon
    $$ PP.nest 4 (d2 $$ PP.text "break;")
-}

doc_multicase :: [Doc] -> Doc -> Doc
doc_multicase ds d =
    (PP.vcat $ map (\ d -> PP.text "case " <> d <> PP.colon) ds)
    $$ PP.nest 4 d


doc_multicasebreak :: [Doc] -> Doc -> Doc
doc_multicasebreak ds d =
    (PP.vcat $ map (\ d -> PP.text "case " <> d <> PP.colon) ds)
    $$ PP.nest 4 (d $$ PP.text "break;")


doc_default :: Doc -> Doc
doc_default d = PP.text "default:" $$ PP.nest 4 d


