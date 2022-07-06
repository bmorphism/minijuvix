module MiniJuvix.Syntax.MicroJuvix.Pretty.Base
  ( module MiniJuvix.Syntax.MicroJuvix.Pretty.Base,
    module MiniJuvix.Syntax.MicroJuvix.Pretty.Ann,
    module MiniJuvix.Syntax.MicroJuvix.Pretty.Options,
  )
where

import Data.HashMap.Strict qualified as HashMap
import MiniJuvix.Internal.Strings qualified as Str
import MiniJuvix.Prelude
import MiniJuvix.Prelude.Pretty
import MiniJuvix.Syntax.Backends
import MiniJuvix.Syntax.Fixity
import MiniJuvix.Syntax.ForeignBlock
import MiniJuvix.Syntax.MicroJuvix.Language.Extra
import MiniJuvix.Syntax.MicroJuvix.Pretty.Ann
import MiniJuvix.Syntax.MicroJuvix.Pretty.Options

doc :: PrettyCode c => Options -> c -> Doc Ann
doc opts =
  run
    . runReader opts
    . ppCode

class PrettyCode c where
  ppCode :: Member (Reader Options) r => c -> Sem r (Doc Ann)

runPrettyCode :: PrettyCode c => Options -> c -> Doc Ann
runPrettyCode opts = run . runReader opts . ppCode

instance PrettyCode NameId where
  ppCode (NameId k) = return (pretty k)

instance PrettyCode Name where
  ppCode n = do
    showNameId <- asks (^. optShowNameIds)
    uid <-
      if
          | showNameId -> Just . ("@" <>) <$> ppCode (n ^. nameId)
          | otherwise -> return Nothing
    return
      $ annotate (AnnKind (n ^. nameKind))
      $ pretty (n ^. nameText)
      <?> uid

instance PrettyCode Iden where
  ppCode :: Member (Reader Options) r => Iden -> Sem r (Doc Ann)
  ppCode i = case i of
    IdenFunction na -> ppCode na
    IdenConstructor na -> ppCode na
    IdenVar na -> ppCode na
    IdenAxiom a -> ppCode a
    IdenInductive a -> ppCode a

instance PrettyCode Application where
  ppCode a = do
    l' <- ppLeftExpression appFixity (a ^. appLeft)
    r' <- case a ^. appImplicit of
      Explicit -> ppRightExpression appFixity (a ^. appRight)
      Implicit -> braces <$> ppCode (a ^. appRight)
    return $ l' <+> r'

instance PrettyCode TypedExpression where
  ppCode e = ppCode (e ^. typedExpression)

instance PrettyCode SmallUniverse where
  ppCode _ = return kwType

instance PrettyCode Expression where
  ppCode = \case
    ExpressionIden i -> ppCode i
    ExpressionHole h -> ppCode h
    ExpressionApplication a -> ppCode a
    ExpressionFunction f -> ppCode f
    ExpressionUniverse u -> ppCode u
    ExpressionLiteral l -> return (pretty l)

keyword :: Text -> Doc Ann
keyword = annotate AnnKeyword . pretty

kwInclude :: Doc Ann
kwInclude = keyword Str.include

kwArrow :: Doc Ann
kwArrow = keyword Str.toUnicode

kwMapsto :: Doc Ann
kwMapsto = keyword Str.mapstoUnicode

kwForeign :: Doc Ann
kwForeign = keyword Str.foreign_

kwCompile :: Doc Ann
kwCompile = keyword Str.compile

kwC :: Doc Ann
kwC = keyword Str.cBackend

kwGhc :: Doc Ann
kwGhc = keyword Str.ghc

kwColon :: Doc Ann
kwColon = keyword Str.colon

kwData :: Doc Ann
kwData = keyword Str.data_

kwAssign :: Doc Ann
kwAssign = keyword Str.assignUnicode

kwEquals :: Doc Ann
kwEquals = keyword Str.equal

kwColonColon :: Doc Ann
kwColonColon = keyword (Str.colon <> Str.colon)

kwPipe :: Doc Ann
kwPipe = keyword Str.pipe

kwHole :: Doc Ann
kwHole = keyword Str.underscore

kwAxiom :: Doc Ann
kwAxiom = keyword Str.axiom

kwWhere :: Doc Ann
kwWhere = keyword Str.where_

kwModule :: Doc Ann
kwModule = keyword Str.module_

kwType :: Doc Ann
kwType = keyword Str.type_

kwWildcard :: Doc Ann
kwWildcard = keyword Str.underscore

instance PrettyCode BackendItem where
  ppCode BackendItem {..} = do
    backend <- ppCode _backendItemBackend
    return $
      backend <+> kwMapsto <+> pretty _backendItemCode

instance PrettyCode FunctionParameter where
  ppCode FunctionParameter {..} = do
    case _paramName of
      Nothing -> ppLeftExpression funFixity _paramType
      Just n -> do
        paramName' <- ppCode n
        paramType' <- ppCode _paramType
        return $ implicitDelim _paramImplicit (paramName' <+> paramType')

instance PrettyCode Function where
  ppCode (Function l r) = do
    funParameter' <- ppCode l
    funReturn' <- ppRightExpression funFixity r
    return $ funParameter' <+> kwArrow <+> funReturn'

instance PrettyCode Hole where
  ppCode _ = return kwHole

instance PrettyCode InductiveConstructorDef where
  ppCode c = do
    constructorName' <- ppCode (c ^. inductiveConstructorName)
    constructorParameters' <- mapM ppCodeAtom (c ^. inductiveConstructorParameters)
    return (hsep $ constructorName' : constructorParameters')

indent' :: Member (Reader Options) r => Doc a -> Sem r (Doc a)
indent' d = do
  i <- asks (^. optIndent)
  return $ indent i d

ppBlock ::
  (PrettyCode a, Members '[Reader Options] r, Traversable t) =>
  t a ->
  Sem r (Doc Ann)
ppBlock items = mapM ppCode items >>= bracesIndent . vsep . toList

implicitDelim :: IsImplicit -> Doc Ann -> Doc Ann
implicitDelim = \case
  Implicit -> braces
  Explicit -> parens

bracesIndent :: Members '[Reader Options] r => Doc Ann -> Sem r (Doc Ann)
bracesIndent d = do
  d' <- indent' d
  return $ braces (line <> d' <> line)

instance PrettyCode InductiveParameter where
  ppCode (InductiveParameter v) = do
    v' <- ppCode v
    return $ parens (v' <+> kwColon <+> kwType)

instance PrettyCode InductiveDef where
  ppCode d = do
    inductiveName' <- ppCode (d ^. inductiveName)
    params <- hsepMaybe <$> mapM ppCode (d ^. inductiveParameters)
    inductiveConstructors' <- mapM ppCode (d ^. inductiveConstructors)
    rhs <- indent' $ align $ concatWith (\a b -> a <> line <> kwPipe <+> b) inductiveConstructors'
    return $ kwData <+> inductiveName' <+?> params <+> kwEquals <> line <> rhs

instance PrettyCode ConstructorApp where
  ppCode c = do
    constr' <- ppCode (c ^. constrAppConstructor)
    params' <- mapM ppCodeAtom (c ^. constrAppParameters)
    return $ hsep $ constr' : params'

instance PrettyCode Pattern where
  ppCode p = case p of
    PatternVariable v -> ppCode v
    PatternConstructorApp a -> ppCode a
    PatternWildcard {} -> return kwWildcard
    PatternBraces b -> braces <$> ppCode b

instance PrettyCode FunctionDef where
  ppCode f = do
    funDefName' <- ppCode (f ^. funDefName)
    funDefType' <- ppCode (f ^. funDefType)
    clauses' <- mapM ppCode (f ^. funDefClauses)
    return $
      funDefName'
        <+> kwColonColon
        <+> funDefType'
          <> line
          <> vsep (toList clauses')

instance PrettyCode FunctionClause where
  ppCode c = do
    funName <- ppCode (c ^. clauseName)
    clausePatterns' <- mapM ppCodeAtom (c ^. clausePatterns)
    clauseBody' <- ppCode (c ^. clauseBody)
    return $ funName <+> hsep clausePatterns' <+> kwAssign <+> clauseBody'

instance PrettyCode Backend where
  ppCode = \case
    BackendGhc -> return kwGhc
    BackendC -> return kwC

instance PrettyCode ForeignBlock where
  ppCode ForeignBlock {..} = do
    _foreignBackend' <- ppCode _foreignBackend
    return $
      kwForeign
        <+> _foreignBackend'
        <+> lbrace
          <> line
          <> pretty _foreignCode
          <> line
          <> rbrace

instance PrettyCode Include where
  ppCode i = do
    name' <- ppCode (i ^. includeModule . moduleName)
    return $ kwInclude <+> name'

instance PrettyCode AxiomDef where
  ppCode AxiomDef {..} = do
    axiomName' <- ppCode _axiomName
    axiomType' <- ppCode _axiomType
    return $ kwAxiom <+> axiomName' <+> kwColon <+> axiomType'

instance PrettyCode Statement where
  ppCode = \case
    StatementForeign f -> ppCode f
    StatementFunction f -> ppCode f
    StatementInductive f -> ppCode f
    StatementAxiom f -> ppCode f
    StatementInclude i -> ppCode i

instance PrettyCode ModuleBody where
  ppCode m = do
    everything <- mapM ppCode (m ^. moduleStatements)
    return $ vsep2 everything

instance PrettyCode Module where
  ppCode m = do
    name' <- ppCode (m ^. moduleName)
    body' <- ppCode (m ^. moduleBody)
    return $
      kwModule
        <+> name'
        <+> kwWhere
          <> line
          <> line
          <> body'
          <> line

instance PrettyCode TypeCallIden where
  ppCode = \case
    InductiveIden n -> ppCode n
    FunctionIden n -> ppCode n

instance PrettyCode Caller where
  ppCode = \case
    CallerInductive n -> ppCode n
    CallerAxiom n -> ppCode n
    CallerFunction n -> ppCode n

instance PrettyCode ConcreteTypeCall where
  ppCode = ppCode . fmap (^. unconcreteType)

instance PrettyCode TypeCall where
  ppCode (TypeCall' f args) = do
    f' <- ppCode f
    args' <- mapM ppCodeAtom args
    return $ f' <+> hsep args'

instance PrettyCode TypeCallsMap where
  ppCode m = do
    let title = keyword "Type Calls Map:"
        elems :: [(Caller, TypeCall)]
        elems =
          [(caller, t) | (caller, l) <- HashMap.toList (m ^. typeCallsMap), t <- toList l]
    elems' <- mapM ppCodeElem (sortOn fst elems)
    return $ title <> line <> vsep elems' <> line
    where
      ppCodeElem :: Member (Reader Options) r => (Caller, TypeCall) -> Sem r (Doc Ann)
      ppCodeElem (caller, t) = do
        caller' <- ppCode caller
        t' <- ppCode t
        return $ caller' <+> kwMapsto <+> t'

instance PrettyCode TypeCalls where
  ppCode m = do
    let title = keyword "Propagated Type Calls:"
        elems = sortOn (^. typeCallIden) (concatMap HashMap.keys (toList (m ^. typeCallSet)))
    elems' <- mapM ppCode elems
    return $ title <> line <> vsep elems' <> line

parensCond :: Bool -> Doc Ann -> Doc Ann
parensCond t d = if t then parens d else d

ppPostExpression ::
  (PrettyCode a, HasAtomicity a, Member (Reader Options) r) =>
  Fixity ->
  a ->
  Sem r (Doc Ann)
ppPostExpression = ppLRExpression isPostfixAssoc

ppRightExpression ::
  (PrettyCode a, HasAtomicity a, Member (Reader Options) r) =>
  Fixity ->
  a ->
  Sem r (Doc Ann)
ppRightExpression = ppLRExpression isRightAssoc

ppLeftExpression ::
  (PrettyCode a, HasAtomicity a, Member (Reader Options) r) =>
  Fixity ->
  a ->
  Sem r (Doc Ann)
ppLeftExpression = ppLRExpression isLeftAssoc

ppLRExpression ::
  (HasAtomicity a, PrettyCode a, Member (Reader Options) r) =>
  (Fixity -> Bool) ->
  Fixity ->
  a ->
  Sem r (Doc Ann)
ppLRExpression associates fixlr e =
  parensCond (atomParens associates (atomicity e) fixlr)
    <$> ppCode e

ppCodeAtom :: (HasAtomicity c, PrettyCode c, Members '[Reader Options] r) => c -> Sem r (Doc Ann)
ppCodeAtom c = do
  p' <- ppCode c
  return $ if isAtomic c then p' else parens p'

instance PrettyCode a => PrettyCode (NonEmpty a) where
  ppCode x = do
    cs <- mapM ppCode (toList x)
    return $ encloseSep "(" ")" ", " cs

instance PrettyCode ConcreteType where
  ppCode ConcreteType {..} = ppCode _unconcreteType
