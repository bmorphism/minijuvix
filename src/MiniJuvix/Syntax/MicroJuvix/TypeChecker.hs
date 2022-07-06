module MiniJuvix.Syntax.MicroJuvix.TypeChecker
  ( module MiniJuvix.Syntax.MicroJuvix.TypeChecker,
    module MiniJuvix.Syntax.MicroJuvix.MicroJuvixTypedResult,
    module MiniJuvix.Syntax.MicroJuvix.Error,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import MiniJuvix.Internal.NameIdGen
import MiniJuvix.Prelude hiding (fromEither)
import MiniJuvix.Syntax.MicroJuvix.Error
import MiniJuvix.Syntax.MicroJuvix.InfoTable
import MiniJuvix.Syntax.MicroJuvix.Language.Extra
import MiniJuvix.Syntax.MicroJuvix.LocalVars
import MiniJuvix.Syntax.MicroJuvix.MicroJuvixArityResult
import MiniJuvix.Syntax.MicroJuvix.MicroJuvixTypedResult
import MiniJuvix.Syntax.MicroJuvix.TypeChecker.Inference

entryMicroJuvixTyped ::
  Members '[Error TypeCheckerError, NameIdGen] r =>
  MicroJuvixArityResult ->
  Sem r MicroJuvixTypedResult
entryMicroJuvixTyped res@MicroJuvixArityResult {..} = do
  r <- runReader table (mapM checkModule _resultModules)
  return
    MicroJuvixTypedResult
      { _resultMicroJuvixArityResult = res,
        _resultModules = r
      }
  where
    table :: InfoTable
    table = buildTable _resultModules

checkModule ::
  Members '[Reader InfoTable, Error TypeCheckerError, NameIdGen] r =>
  Module ->
  Sem r Module
checkModule Module {..} = do
  _moduleBody' <- checkModuleBody _moduleBody
  return
    Module
      { _moduleBody = _moduleBody',
        ..
      }

checkModuleBody ::
  Members '[Reader InfoTable, Error TypeCheckerError, NameIdGen] r =>
  ModuleBody ->
  Sem r ModuleBody
checkModuleBody ModuleBody {..} = do
  _moduleStatements' <- mapM checkStatement _moduleStatements
  return
    ModuleBody
      { _moduleStatements = _moduleStatements'
      }

checkInclude ::
  Members '[Reader InfoTable, Error TypeCheckerError, NameIdGen] r =>
  Include ->
  Sem r Include
checkInclude = traverseOf includeModule checkModule

checkStatement ::
  Members '[Reader InfoTable, Error TypeCheckerError, NameIdGen] r =>
  Statement ->
  Sem r Statement
checkStatement s = case s of
  StatementFunction fun -> StatementFunction <$> checkFunctionDef fun
  StatementForeign {} -> return s
  StatementInductive d -> StatementInductive <$> checkInductiveDef d
  StatementInclude i -> StatementInclude <$> checkInclude i
  StatementAxiom {} -> return s

checkFunctionDef ::
  Members '[Reader InfoTable, Error TypeCheckerError, NameIdGen] r =>
  FunctionDef ->
  Sem r FunctionDef
checkFunctionDef FunctionDef {..} = runInferenceDef $ do
  info <- lookupFunction _funDefName
  checkFunctionDefType _funDefType
  _funDefClauses' <- mapM (checkFunctionClause info) _funDefClauses
  return
    FunctionDef
      { _funDefClauses = _funDefClauses',
        ..
      }

checkFunctionDefType :: forall r. Members '[Inference] r => Expression -> Sem r ()
checkFunctionDefType = traverseOf_ (leafExpressions . _ExpressionHole) go
  where
    go :: Hole -> Sem r ()
    go h = void (freshMetavar h)

checkExpression ::
  Members '[Reader InfoTable, Error TypeCheckerError, NameIdGen, Reader LocalVars, Inference] r =>
  Expression ->
  Expression ->
  Sem r Expression
checkExpression expectedTy e = do
  e' <- inferExpression' e
  let inferredType = e' ^. typedType
  unlessM (matchTypes expectedTy inferredType) (throw (err inferredType))
  return (e' ^. typedExpression)
  where
    err infTy =
      ErrWrongType
        ( WrongType
            { _wrongTypeExpression = e,
              _wrongTypeInferredType = infTy,
              _wrongTypeExpectedType = expectedTy
            }
        )

checkFunctionParameter ::
  Members '[Reader InfoTable, Error TypeCheckerError, NameIdGen, Reader LocalVars, Inference] r =>
  FunctionParameter ->
  Sem r FunctionParameter
checkFunctionParameter (FunctionParameter mv i e) = do
  e' <- checkExpression (smallUniverse (getLoc e)) e
  return (FunctionParameter mv i e')

-------------------------------------------------------------------------------
-- Typechecking of data types
-------------------------------------------------------------------------------

type ErrorReference = Maybe Expression

type RecursionLimit = Int

checkStrictlyPositiveOccurrences ::
  Members '[Reader InfoTable, Error TypeCheckerError] r =>
  InductiveName ->
  ConstrName ->
  Name ->
  RecursionLimit ->
  ErrorReference ->
  Expression ->
  Sem r ()
checkStrictlyPositiveOccurrences indName ctorName name recLimit ref = helper False
  where
    -- In the func. below, we want to determine if there is a
    -- negative occurence of `name` in the expression `expr`
    -- The `inside` flag indicates whether the current search happens
    -- in the left of an inner arrow.
    helper ::
      Members '[Reader InfoTable, Error TypeCheckerError] r =>
      Bool ->
      Expression ->
      Sem r ()

    helper inside expr = case expr of
      ExpressionIden (IdenInductive ty') -> when (inside && name == ty') (strictlyPositivityError expr)
      ExpressionIden (IdenVar name') -> when (inside && name == name') (strictlyPositivityError expr)
      ExpressionFunction (Function l r) -> helper True (l ^. paramType) >> helper False r
      ExpressionApplication
        tyApp@(Application (ExpressionIden (IdenInductive ty')) r _) -> do
          if
              | inside && name == ty' -> strictlyPositivityError expr
              | name /= ty' -> do
                  -- Here `name` may show up as a subexpr of ty'. Therefore, we need to check
                  -- if the type ty' preserves the str. positivity condition.
                  -- The type ty', by assumption, has to be strictly positive. It is already in scope.
                  -- Then, it remains to check that the ty' type constructor parameters in which `name`
                  --  is, they are all strictly positive. TODO: This last check is done on demand, but it
                  -- could be cached, if the infotable becomes stateful.
                  InductiveInfo indTy' <- lookupInductive ty'
                  let (_, args) = unfoldApplication tyApp
                      paramsTy' = indTy' ^. inductiveParameters
                      go ::
                        Members '[Reader InfoTable, Error TypeCheckerError] r =>
                        [(InductiveParameter, Expression)] ->
                        Sem r ()
                      go = \case
                        ((InductiveParameter pName _, arg) : ps) ->
                          if
                              | nameInExpression name arg -> do
                                  unless
                                    (indTy' ^. inductiveNoPositivity || recLimit == 0)
                                    ( forM_ (indTy' ^. inductiveConstructors) $ \ctor' -> do
                                        -- check if pName occurs strictly positive in indTy'.
                                        -- note that we use the info of the datatype that is being checked
                                        mapM_
                                          ( checkStrictlyPositiveOccurrences
                                              indName
                                              ctorName
                                              pName
                                              (recLimit - 1)
                                              (Just (fromMaybe arg ref))
                                              -- (Just arg)
                                          )
                                          (ctor' ^. inductiveConstructorParameters)
                                    )
                                  go ps
                              | otherwise -> go ps
                        [] -> return ()
                  go (zip paramsTy' (toList args))
              | otherwise -> helper inside r
      _ -> return ()

    strictlyPositivityError :: Members '[Error TypeCheckerError] r => Expression -> Sem r ()
    strictlyPositivityError expr = do
      let errLoc = fromMaybe expr ref
      throw
        ( ErrNoStrictPositivity $
            NoStrictPositivity
              { _noStrictPositivityType = indName,
                _noStrictPositivityConstructor = ctorName,
                _noStrictPositivityArgument = errLoc
              }
        )

checkInductiveDef ::
  Members '[Reader InfoTable, Error TypeCheckerError, NameIdGen] r =>
  InductiveDef ->
  Sem r InductiveDef
checkInductiveDef ty@InductiveDef {..} = do
  checkInductiveParameterNames _inductiveName _inductiveParameters
  mapM_ (checkConstructorDef ty) _inductiveConstructors
  return ty

checkInductiveParameterNames ::
  Members '[Reader InfoTable, Error TypeCheckerError] r =>
  InductiveName ->
  [InductiveParameter] ->
  Sem r ()
checkInductiveParameterNames tyName = helper mempty
  where
    helper ::
      Members '[Reader InfoTable, Error TypeCheckerError] r =>
      HashSet Text ->
      [InductiveParameter] ->
      Sem r ()
    helper _ [] = return ()
    helper nset (p : parms) = do
      let pName = p ^. inductiveParamName
          pText = pName ^. nameText
      if
          | HashSet.member pText nset ->
              throw
                ( ErrWrongInductiveParameterName
                    ( WrongInductiveParameterName
                        { _wrongInductiveParameterName = pName,
                          _wrongInductiveParameterType = tyName
                        }
                    )
                )
          | otherwise -> helper (HashSet.insert pText nset) parms

checkConstructorDef ::
  Members '[Reader InfoTable, Error TypeCheckerError] r =>
  InductiveDef ->
  InductiveConstructorDef ->
  Sem r InductiveConstructorDef
checkConstructorDef ty ctor = do
  let indName = ty ^. inductiveName
      ctorName = ctor ^. inductiveConstructorName
  checkConstructorReturnType indName ctor
  numInductives <- HashMap.size <$> asks (^. infoInductives)
  unless
    (ty ^. inductiveNoPositivity)
    (mapM_ (checkStrictlyPositiveOccurrences indName ctorName indName numInductives Nothing) (ctor ^. inductiveConstructorParameters))
  return ctor

checkConstructorReturnType ::
  Members '[Reader InfoTable, Error TypeCheckerError] r =>
  InductiveName ->
  InductiveConstructorDef ->
  Sem r ()
checkConstructorReturnType indName ctor = do
  InductiveInfo indType <- lookupInductive indName
  let ctorName = ctor ^. inductiveConstructorName
      ctorReturnType = ctor ^. inductiveConstructorReturnType
      tyName = indType ^. inductiveName
      indParams = map (^. inductiveParamName) (indType ^. inductiveParameters)
      expectedReturnType =
        foldExplicitApplication
          (ExpressionIden (IdenInductive tyName))
          (map (ExpressionIden . IdenVar) indParams)
  when
    (ctorReturnType /= expectedReturnType)
    ( throw
        ( ErrWrongReturnType
            ( WrongReturnType
                { _wrongReturnTypeConstructorName = ctorName,
                  _wrongReturnTypeExpected = expectedReturnType,
                  _wrongReturnTypeActual = ctorReturnType
                }
            )
        )
    )

-------------------------------------------------------------------------------

inferExpression ::
  Members '[Reader InfoTable, Error TypeCheckerError, NameIdGen, Reader LocalVars, Inference] r =>
  Expression ->
  Sem r Expression
inferExpression = fmap (^. typedExpression) . inferExpression'

lookupVar :: Member (Reader LocalVars) r => Name -> Sem r Expression
lookupVar v = HashMap.lookupDefault impossible v <$> asks (^. localTypes)

checkFunctionClauseBody ::
  Members '[Reader InfoTable, Error TypeCheckerError, NameIdGen, Inference] r =>
  LocalVars ->
  Expression ->
  Expression ->
  Sem r Expression
checkFunctionClauseBody locals expectedTy body =
  runReader locals (checkExpression expectedTy body)

checkFunctionClause ::
  Members '[Reader InfoTable, Error TypeCheckerError, NameIdGen, Inference] r =>
  FunctionInfo ->
  FunctionClause ->
  Sem r FunctionClause
checkFunctionClause info FunctionClause {..} = do
  let (argTys, rty) = unfoldFunType (info ^. functionInfoDef . funDefType)
      (patTys, restTys) = splitAt (length _clausePatterns) argTys
      bodyTy = foldFunType restTys rty
  if
      | length patTys /= length _clausePatterns -> impossible
      | otherwise -> do
          locals <- checkPatterns _clauseName (zipExact patTys _clausePatterns)
          let bodyTy' =
                substitutionE
                  ( fmap
                      (ExpressionIden . IdenVar)
                      (locals ^. localTyMap)
                  )
                  bodyTy
          _clauseBody' <- checkFunctionClauseBody locals bodyTy' _clauseBody
          return
            FunctionClause
              { _clauseBody = _clauseBody',
                ..
              }

checkPatterns ::
  Members '[Reader InfoTable, Error TypeCheckerError] r =>
  FunctionName ->
  [(FunctionParameter, Pattern)] ->
  Sem r LocalVars
checkPatterns name = execState emptyLocalVars . mapM_ (uncurry (checkPattern name))

typeOfArg :: FunctionParameter -> Expression
typeOfArg = (^. paramType)

checkPattern ::
  forall r.
  Members '[Reader InfoTable, Error TypeCheckerError, State LocalVars] r =>
  FunctionName ->
  FunctionParameter ->
  Pattern ->
  Sem r ()
checkPattern funName = go
  where
    go :: FunctionParameter -> Pattern -> Sem r ()
    go argTy p = do
      tyVarMap <- fmap (ExpressionIden . IdenVar) . (^. localTyMap) <$> get
      let ty = substitutionE tyVarMap (typeOfArg argTy)
          unbrace = \case
            PatternBraces b -> b
            x -> x
      case unbrace p of
        PatternWildcard {} -> return ()
        PatternBraces {} -> impossible
        PatternVariable v -> do
          modify (addType v ty)
          case argTy ^. paramName of
            Just v' -> do
              modify (over localTyMap (HashMap.insert v' v))
            _ -> return ()
        PatternConstructorApp a -> do
          (ind, tyArgs) <- checkSaturatedInductive ty
          info <- lookupConstructor (a ^. constrAppConstructor)
          let constrInd = info ^. constructorInfoInductive
          when
            (ind /= constrInd)
            ( throw
                ( ErrWrongConstructorType
                    (WrongConstructorType (a ^. constrAppConstructor) ind constrInd funName)
                )
            )
          goConstr a tyArgs
      where
        goConstr :: ConstructorApp -> [(InductiveParameter, Expression)] -> Sem r ()
        goConstr app@(ConstructorApp c ps) ctx = do
          (_, psTys) <- constructorArgTypes <$> lookupConstructor c
          let psTys' = map (substituteIndParams ctx) psTys
              expectedNum = length psTys
          let w = map unnamedParameter psTys'
          when (expectedNum /= length ps) (throw (appErr app expectedNum))
          zipWithM_ go w ps
        appErr :: ConstructorApp -> Int -> TypeCheckerError
        appErr app expected =
          ErrArity
            ( ErrWrongConstructorAppLength
                ( WrongConstructorAppLength
                    { _wrongConstructorAppLength = app,
                      _wrongConstructorAppLengthExpected = expected
                    }
                )
            )
    checkSaturatedInductive :: Expression -> Sem r (InductiveName, [(InductiveParameter, Expression)])
    checkSaturatedInductive ty = do
      (ind, args) <- viewInductiveApp ty
      params <-
        (^. inductiveInfoDef . inductiveParameters)
          <$> lookupInductive ind
      let numArgs = length args
          numParams = length params
      when
        (numArgs < numParams)
        ( throw
            ( ErrTooFewArgumentsIndType
                ( WrongNumberArgumentsIndType
                    { _wrongNumberArgumentsIndTypeActualType = ty,
                      _wrongNumberArgumentsIndTypeActualNumArgs = numArgs,
                      _wrongNumberArgumentsIndTypeExpectedNumArgs = numParams
                    }
                )
            )
        )
      when
        (numArgs > numParams)
        ( throw
            ( ErrTooManyArgumentsIndType
                ( WrongNumberArgumentsIndType
                    { _wrongNumberArgumentsIndTypeActualType = ty,
                      _wrongNumberArgumentsIndTypeActualNumArgs = numArgs,
                      _wrongNumberArgumentsIndTypeExpectedNumArgs = numParams
                    }
                )
            )
        )
      return (ind, zip params args)

freshHole :: Members '[Inference, NameIdGen] r => Interval -> Sem r Hole
freshHole l = do
  uid <- freshNameId
  let h = Hole uid l
  void (freshMetavar h)
  return h

-- | Returns {A : Expression} → A
literalType :: Members '[NameIdGen] r => LiteralLoc -> Sem r TypedExpression
literalType l = do
  uid <- freshNameId
  let typeVar =
        Name
          { _nameText = "A",
            _nameId = uid,
            _nameKind = KNameLocal,
            _nameLoc = getLoc l
          }
      param =
        FunctionParameter
          { _paramName = Just typeVar,
            _paramImplicit = Implicit,
            _paramType = smallUniverse (getLoc l)
          }
      type_ =
        ExpressionFunction
          Function
            { _functionLeft = param,
              _functionRight = ExpressionIden (IdenVar typeVar)
            }
  return
    TypedExpression
      { _typedType = type_,
        _typedExpression = ExpressionLiteral l
      }

inferExpression' ::
  forall r.
  Members '[Reader InfoTable, Reader LocalVars, Error TypeCheckerError, NameIdGen, Inference] r =>
  Expression ->
  Sem r TypedExpression
inferExpression' e = case e of
  ExpressionIden i -> inferIden i
  ExpressionApplication a -> inferApplication a
  ExpressionLiteral l -> goLiteral l
  ExpressionFunction f -> goExpressionFunction f
  ExpressionHole h -> freshMetavar h
  ExpressionUniverse u -> goUniverse u
  where
    goUniverse :: SmallUniverse -> Sem r TypedExpression
    goUniverse u =
      return
        TypedExpression
          { _typedType = ExpressionUniverse u,
            _typedExpression = ExpressionUniverse u
          }
    goExpressionFunction :: Function -> Sem r TypedExpression
    goExpressionFunction (Function l r) = do
      let uni = smallUniverse (getLoc l)
      l' <- checkFunctionParameter l
      r' <- checkExpression uni r
      return (TypedExpression uni (ExpressionFunction (Function l' r')))
    goLiteral :: LiteralLoc -> Sem r TypedExpression
    goLiteral = literalType

    inferIden :: Iden -> Sem r TypedExpression
    inferIden i = case i of
      IdenFunction fun -> do
        info <- lookupFunction fun
        return (TypedExpression (info ^. functionInfoDef . funDefType) (ExpressionIden i))
      IdenConstructor c -> do
        ty <- constructorType c
        return (TypedExpression ty (ExpressionIden i))
      IdenVar v -> do
        ty <- lookupVar v
        return (TypedExpression ty (ExpressionIden i))
      IdenAxiom v -> do
        info <- lookupAxiom v
        return (TypedExpression (info ^. axiomInfoType) (ExpressionIden i))
      IdenInductive v -> do
        kind <- inductiveType v
        return (TypedExpression kind (ExpressionIden i))
    inferApplication :: Application -> Sem r TypedExpression
    inferApplication (Application l r i) = inferExpression' l >>= helper
      where
        helper :: TypedExpression -> Sem r TypedExpression
        helper l' = case l' ^. typedType of
          ExpressionFunction (Function (FunctionParameter mv _ funL) funR) -> do
            r' <- checkExpression funL r
            return
              TypedExpression
                { _typedExpression =
                    ExpressionApplication
                      Application
                        { _appLeft = l' ^. typedExpression,
                          _appRight = r,
                          _appImplicit = i
                        },
                  _typedType = substitutionApp (mv, r') funR
                }
          -- When we have have an application with a hole on the left: '_@1 x'
          -- We assume that it is a type application and thus 'x' must be a type.
          -- Not sure if this is always desirable.
          ExpressionHole h -> do
            q <- queryMetavar h
            case q of
              Just ty -> helper (set typedType ty l')
              Nothing -> do
                r' <- checkExpression (smallUniverse (getLoc h)) r
                h' <- freshHole (getLoc h)
                let fun = Function (unnamedParameter r') (ExpressionHole h')
                unlessM (matchTypes (ExpressionHole h) (ExpressionFunction fun)) impossible
                return
                  TypedExpression
                    { _typedType = ExpressionHole h',
                      _typedExpression =
                        ExpressionApplication
                          Application
                            { _appLeft = l' ^. typedExpression,
                              _appRight = r',
                              _appImplicit = i
                            }
                    }
          _ -> throw tyErr
            where
              tyErr :: TypeCheckerError
              tyErr =
                ErrExpectedFunctionType
                  ( ExpectedFunctionType
                      { _expectedFunctionTypeExpression = e,
                        _expectedFunctionTypeApp = l,
                        _expectedFunctionTypeType = l' ^. typedType
                      }
                  )

viewInductiveApp ::
  Member (Error TypeCheckerError) r =>
  Expression ->
  Sem r (InductiveName, [Expression])
viewInductiveApp ty = case t of
  ExpressionIden (IdenInductive n) -> return (n, as)
  _ -> throw (ErrImpracticalPatternMatching (ImpracticalPatternMatching ty))
  where
    (t, as) = viewTypeApp ty

viewTypeApp :: Expression -> (Expression, [Expression])
viewTypeApp t = case t of
  ExpressionApplication (Application l r _) ->
    second (`snoc` r) (viewTypeApp l)
  _ -> (t, [])
