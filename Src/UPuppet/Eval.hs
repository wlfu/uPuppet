{------------------------------------------------------------------------------
    uPuppet: Evaluation
------------------------------------------------------------------------------}

module UPuppet.Eval ( evalPuppet ) where

import Data.List
import Debug.Trace

import UPuppet.CState
import UPuppet.AST
import UPuppet.Catalog
import UPuppet.Options

{------------------------------------------------------------------------------
    Types private to the evaluation
------------------------------------------------------------------------------}

type Env = [(Scope, Name, Value)]

lookupEnv :: Env -> Scope -> Name -> Maybe Value
lookupEnv [] _ _                                      = Nothing
lookupEnv ((s, n, v):es) sco x | (x == n && sco == s) = (Just v)
                               | otherwise = (lookupEnv es sco x) 


clearScope :: Scope -> Env -> Env
clearScope sco [] = []
clearScope sco ((s,n,v):es) | s == sco = clearScope sco es
                            | otherwise = (s,n,v):clearScope sco es

data Def = ClassDef (Maybe Name) OptParameterList Statements 
         | DeclaredClass (Scope)
         | ResTypeDef Name OptParameterList Statements
         deriving (Show)
type DefEnv = [(Name, Def)]

parentof :: DefEnv -> Scope -> Scope
parentof defEnv sco =  case sco of 
                          SClass b -> (lookupDefEnv defEnv b)
                          SNode    -> STop
                          STop     -> error "Top scope: No higher scope"
                          SDef b   -> baseof defEnv b

-- the base scope (toplevel or node) in effect in a given scope
baseof :: DefEnv -> Scope -> Scope
baseof defEnv STop        = STop
baseof defEnv SNode       = SNode
baseof defEnv (SDef sco)  = baseof defEnv sco
baseof defEnv (SClass a)  = baseof defEnv (lookupDefEnv defEnv a)

lookforVar :: Env -> DefEnv -> Scope -> Variable -> Value
lookforVar es defEnv sco (LocalVar x) = case (lookupEnv es sco x) of 
                                        (Just b) -> b
                                        Nothing  -> case sco of STop -> (error ("unqualified variable not found in any scope: " ++ show x))
                                                                sco -> (lookforVar es defEnv (parentof defEnv sco) (LocalVar x))
lookforVar es defEnv sco (ScopeVar sco' x) = case (lookupEnv es sco' x) of 
                    (Just b) -> b
                    Nothing  -> error ("lookForVar: " ++ (show sco') ++ " :: " ++ (show x))


extendEnv :: Scope -> [(String, ValueExp)] -> Env
extendEnv _ [] = []
extendEnv sco ((x, (DeRef (Values y))):ys) = (sco, x, y):(extendEnv sco ys)

changeDef :: DefEnv -> String -> Scope -> DefEnv
changeDef ((n, def):ds) a sco | n /= a = (n, def):(changeDef ds a sco) 
                              | n == a = (n, (DeclaredClass sco)):ds


lookupDefEnv :: DefEnv -> Name -> Scope
lookupDefEnv [] b                     = STop 
lookupDefEnv ((a, def):ds) b | a == b = case def of 
                                        DeclaredClass sco -> sco
                             | a /= b = (lookupDefEnv ds b) 

lookupDef :: (Eq a, Show a) => a -> [(a, b)] -> b
lookupDef a []                           = error ("lookupDef: cannot find " ++ show a)
lookupDef a ((name, v):ds) | (a == name) = v 
                           | a /= name   = (lookupDef a ds)

isDef :: DefEnv -> String -> Bool
isDef [] _ = False
isDef ((x, def):ds) n = if x == n then True else isDef ds n 



type States a = (Env, DefEnv, Catalog, a)  

evalExp :: States ValueExp -> Scope -> ValueExp
evalExp (env, defEnv, cv, (DeRef (Var x))) sco                                                                   = (DeRef (Values (lookforVar env defEnv sco x)))
--evalExp (env, defEnv, cv, (DeRef (Values (ValueInt x)))) sco                                                     = (DeRef (Values (ValueInt x)))
--evalExp (env, defEnv, cv, (DeRef (Values (ValueFloat x)))) sco                                                   = (DeRef (Values (ValueFloat x)))
--evalExp (env, defEnv, cv, (DeRef (Values (ValueString x)))) sco                                                  = (DeRef (Values (ValueString x)))
--evalExp (env, defEnv, cv, (DeRef (Values (ValueBool x)))) sco                                                    = (DeRef (Values (ValueBool x)))
evalExp (env, defEnv, cv, (BinOps AddOp (DeRef (Values (ValueInt x))) (DeRef (Values (ValueInt y))))) sco        = (DeRef (Values (ValueInt (x + y))))
evalExp (env, defEnv, cv, (BinOps AddOp (DeRef (Values (ValueFloat x))) (DeRef (Values (ValueFloat y))))) sco    = (DeRef (Values (ValueFloat (x + y))))
evalExp (env, defEnv, cv, (BinOps MinOp (DeRef (Values (ValueInt x))) (DeRef (Values (ValueInt y))))) sco        = (DeRef (Values (ValueInt (x - y))))
evalExp (env, defEnv, cv, (BinOps MinOp (DeRef (Values (ValueFloat x))) (DeRef (Values (ValueFloat y))))) sco    = (DeRef (Values (ValueFloat (x - y))))
evalExp (env, defEnv, cv, (BinOps TimOp (DeRef (Values (ValueInt x))) (DeRef (Values (ValueInt y))))) sco        = (DeRef (Values (ValueInt (x * y))))
evalExp (env, defEnv, cv, (BinOps TimOp (DeRef (Values (ValueFloat x))) (DeRef (Values (ValueFloat y))))) sco    = (DeRef (Values (ValueFloat (x * y))))
evalExp (env, defEnv, cv, (BinOps DivOp (DeRef (Values (ValueInt x))) (DeRef (Values (ValueInt y))))) sco        = (DeRef (Values (ValueInt (x `div` y))))
evalExp (env, defEnv, cv, (BinOps DivOp (DeRef (Values (ValueFloat x))) (DeRef (Values (ValueFloat y))))) sco    = (DeRef (Values (ValueFloat (x / y))))
evalExp (env, defEnv, cv, (BinOps AndOp (DeRef (Values (ValueBool x))) (DeRef (Values (ValueBool y))))) sco      = (DeRef (Values (ValueBool (x && y))))
evalExp (env, defEnv, cv, (BinOps OrOp  (DeRef (Values (ValueBool x))) (DeRef (Values (ValueBool y))))) sco      = (DeRef (Values (ValueBool (x || y))))
evalExp (env, defEnv, cv, (Not (DeRef (Values (ValueBool True))))) sco                                           = (DeRef (Values (ValueBool False)))
evalExp (env, defEnv, cv, (Not (DeRef (Values (ValueBool False))))) sco                                          = (DeRef (Values (ValueBool True)))
evalExp (env, defEnv, cv, (BinOps EqOp (DeRef (Values (ValueInt x))) (DeRef (Values (ValueInt y))))) sco         = (DeRef (Values (ValueBool (x == y))))
evalExp (env, defEnv, cv, (BinOps EqOp (DeRef (Values (ValueFloat x))) (DeRef (Values (ValueFloat y))))) sco     = (DeRef (Values (ValueBool (x == y))))
evalExp (env, defEnv, cv, (BinOps EqOp (DeRef (Values (ValueString x))) (DeRef (Values (ValueString y))))) sco   = (DeRef (Values (ValueBool (x == y))))
evalExp (env, defEnv, cv, (BinOps EqOp (DeRef (Values (ValueBool x))) (DeRef (Values (ValueBool y))))) sco       = (DeRef (Values (ValueBool (x == y))))
evalExp (env, defEnv, cv, (BinOps UneqOp (DeRef (Values (ValueInt x))) (DeRef (Values (ValueInt y))))) sco       = (DeRef (Values (ValueBool (x /= y))))
evalExp (env, defEnv, cv, (BinOps UneqOp (DeRef (Values (ValueFloat x))) (DeRef (Values (ValueFloat y))))) sco   = (DeRef (Values (ValueBool (x /= y))))
evalExp (env, defEnv, cv, (BinOps UneqOp (DeRef (Values (ValueString x))) (DeRef (Values (ValueString y))))) sco = (DeRef (Values (ValueBool (x /= y))))
evalExp (env, defEnv, cv, (BinOps UneqOp (DeRef (Values (ValueBool x))) (DeRef (Values (ValueBool y))))) sco     = (DeRef (Values (ValueBool (x /= y))))
evalExp (env, defEnv, cv, (BinOps GrtOp (DeRef (Values (ValueInt x))) (DeRef (Values (ValueInt y))))) sco        = (DeRef (Values (ValueBool (x > y))))
evalExp (env, defEnv, cv, (BinOps GrtOp (DeRef (Values (ValueFloat x))) (DeRef (Values (ValueFloat y))))) sco    = (DeRef (Values (ValueBool (x > y))))
evalExp (env, defEnv, cv, (BinOps LessOp (DeRef (Values (ValueInt x))) (DeRef (Values (ValueInt y))))) sco       = (DeRef (Values (ValueBool (x < y))))
evalExp (env, defEnv, cv, (BinOps LessOp (DeRef (Values (ValueFloat x))) (DeRef (Values (ValueFloat y))))) sco   = (DeRef (Values (ValueBool (x < y))))
evalExp (env, defEnv, cv, (BinOps LeqOp (DeRef (Values (ValueInt x))) (DeRef (Values (ValueInt y))))) sco        = (DeRef (Values (ValueBool (x <= y))))
evalExp (env, defEnv, cv, (BinOps LeqOp (DeRef (Values (ValueFloat x))) (DeRef (Values (ValueFloat y))))) sco    = (DeRef (Values (ValueBool (x <= y))))
evalExp (env, defEnv, cv, (BinOps GeqOp (DeRef (Values (ValueInt x))) (DeRef (Values (ValueInt y))))) sco        = (DeRef (Values (ValueBool (x >= y))))
evalExp (env, defEnv, cv, (BinOps GeqOp (DeRef (Values (ValueFloat x))) (DeRef (Values (ValueFloat y))))) sco    = (DeRef (Values (ValueBool (x >= y))))
evalExp (env, defEnv, cv, (Not exp)) sco                                                                         = (Not (evalExp (env, defEnv, cv, exp) sco))
evalExp (env, defEnv, cv, (BinOps op (DeRef (Values v)) exp')) sco                                               = BinOps op (DeRef (Values v)) (evalExp (env, defEnv, cv, exp') sco)
evalExp (env, defEnv, cv, (BinOps op exp exp')) sco                                                              = BinOps op (evalExp (env, defEnv, cv, exp) sco) exp'
evalExp (env, defEnv, cv, (Selector s sbody)) sco | not(isVal s)                                                 = Selector (evalExp (env,defEnv, cv, s) sco) sbody
evalExp (env, defEnv, cv, (Selector _  [])) sco                                                                  = error "No value returned by selector"
evalExp (env, defEnv, cv, (Selector s@(DeRef (Values v)) ((x,z):xs))) sco = 
  case x of (DeRef (Values (ValueString "default"))) -> z   
            (DeRef (Values w))                       -> if v == w then z else (Selector s xs)
            _                                        -> Selector s ((e,z):xs)
                                                        where e = evalExp (env, defEnv, cv, x) sco
evalExp (env, defEnv, cv, (Selector _ _)) sco                                                                    = error "Selector"
evalExp (env, defEnv, cv, (Array [])) sco   = error "empty array"
evalExp (env, defEnv, cv, (Array (as))) sco = if (valueArray as) then (DeRef (Values (ValueArray (toValueArray as)))) 
                                              else (Array (evalArray (env, defEnv, cv, as) sco))
evalExp (env, defEnv, cv, (Hash [])) sco    = error "empty hash"
evalExp (env, defEnv, cv, (Hash hs)) sco    = if (valueHash hs) then (DeRef (Values (ValueHash (toValueHash hs))))
                                              else (Hash (evalHash (env, defEnv, cv, hs) sco))
evalExp (env, defEnv, cv, (DeRef (DeRefItem x r))) sco = case x of 
    (Var var)               -> DeRef (DeRefItem (Values (lookforVar env defEnv sco var)) r)
    (Values (ValueArray s)) -> case r of 
                               (DeRef (Values (ValueInt x)))    -> (DeRef (deRefArray s x))
                               DeRef (Values x)                 -> error "evalDeRefArray"
                               _                                -> DeRef (DeRefItem x (evalExp (env, defEnv, cv, r) sco))
    (Values (ValueHash s))  -> case r of 
                               DeRef (Values x)        -> DeRef (deRefHash s x)
                               _                       -> DeRef (DeRefItem x (evalExp (env, defEnv, cv, r) sco))
    (Values (ValueRef a b)) -> case r of 
                               (DeRef (Values (ValueString x))) -> (lookupCat cv a b x) 
                               _                                -> error "evalValueRef"             
    (ResRef a b)            -> case b of 
                               (DeRef (Values (ValueString x))) -> (DeRef (DeRefItem (Values (ValueRef a x)) r))
                               (DeRef (Values _ ))              -> error "evalResRef"
                               _                                -> (DeRef (DeRefItem (ResRef a (evalExp (env, defEnv, cv, b) sco)) r))
    (DeRefItem a b)         -> case (evalExp (env, defEnv, cv, (DeRef x)) sco) of
                               (DeRef y) -> (DeRef (DeRefItem y r))
    _                       -> error "evalExp1"

evalExp (env, defEnv, cv, (DeRef (ResRef r n))) sco = case n of 
    (DeRef (Values (ValueString x))) -> (DeRef (Values (ValueRef r x)))
    (DeRef (Values _ ))              -> error "ResRef1"
    _                                -> (DeRef (ResRef r (evalExp (env, defEnv, cv, n) sco)))

valueArray :: [ValueExp] -> Bool
valueArray []                    = True
valueArray ((DeRef (Values a)):as) = (valueArray as) 
valueArray _                     = False

toValueArray :: [ValueExp] -> [Value]
toValueArray []                    = []
toValueArray ((DeRef (Values a)):as) = (a:(toValueArray as)) 

valueHash :: [(Value, ValueExp)] -> Bool
valueHash []                    = True
valueHash ((a, (DeRef (Values b))):hs) = valueHash hs 
valueHash _                     = False

toValueHash :: [(Value, ValueExp)] -> [(Value, Value)]
toValueHash []                    = []
toValueHash ((a, (DeRef (Values b))) : hs) = ((a, b):(toValueHash hs)) 
            
evalArray :: States [ValueExp] -> Scope -> [ValueExp]
evalArray (_, _, _, []) sco                              = [] 
evalArray (env, defEnv, cv, ((DeRef (Values a)):as)) sco = ((DeRef (Values a)):(evalArray (env, defEnv, cv, as) sco))
evalArray (env, defEnv, cv, (a:as)) sco                  = (evalExp (env, defEnv, cv, a) sco):as 

evalHash :: States [(Value, ValueExp)] -> Scope  -> [(Value, ValueExp)]
evalHash (_, _, _, []) sco                                   = []
evalHash (env, defEnv, cv, ((x, (DeRef (Values h))):hs)) sco = ((x, (DeRef (Values h))):(evalHash (env, defEnv, cv, hs) sco))
evalHash (env, defEnv, cv, ((x,h):hs)) sco                   = (x, (evalExp (env, defEnv, cv, h) sco)):hs

evalStat :: States Statements -> Scope -> States Statements
evalStat (env, defEnv, cv, Skip) sco                                 = error "evalStat1"
evalStat (env, defEnv, cv, (Assignment x y)) sco  = case y of 
    (DeRef (Values v)) -> if lookupEnv env sco x /= Nothing then error ("Variable " ++ show x ++ " already defined in scope " ++ show sco)
                          else ((env ++ [(sco, x, v)]), defEnv, cv, Skip)
    _                  -> (env, defEnv, cv, (Assignment x (evalExp (env, defEnv, cv, y) sco)))
evalStat (env, defEnv, cv, (If (DeRef (Values (ValueBool True))) y k)) sco = (env, defEnv, cv, y)
evalStat (env, defEnv, cv, (If (DeRef (Values (ValueBool False))) y k)) sco =
    case k of
       Nothing -> (env, defEnv, cv, Skip)
       Just (Elseif e s k) -> (env, defEnv, cv, If e s k)
       Just (Else s) -> (env, defEnv, cv, s)
evalStat (env, defEnv, cv, (If x y k)) sco =
  let e = evalExp (env, defEnv, cv, x) sco in (env, defEnv, cv, (If e y k))
--evalStat (env, defEnv, cv, (Elseif x y)) sco = case x of 
--    (DeRef (Values (ValueBool True)))  -> (env, defEnv, cv, y)
--    (DeRef (Values (ValueBool False))) -> (env, defEnv, cv, Skip)
--    _                                  -> let e = evalExp (env, defEnv, cv, x) sco in (env, defEnv, cv, (Elseif e y))
--evalStat (env, defEnv, cv, (Else x)) sco = (env, defEnv, cv, x)
evalStat (env, defEnv, cv, (Unless (DeRef (Values (ValueBool True))) s k)) sco =
    case k of
      Nothing -> (env, defEnv, cv, Skip)
      Just (Else s) -> (env, defEnv, cv, s)
      Just (Elseif _ _ _) -> error "evalStat: 'elsif' not allowed with 'unless'"
evalStat (env, defEnv, cv, (Unless (DeRef (Values (ValueBool False))) s k)) sco = (env, defEnv, cv, s)
evalStat (env, defEnv, cv, (Unless (DeRef (Values v)) s k)) sco = error "evalStat: Test component of 'unless' is not a Boolean value!"
evalStat (env, defEnv, cv, (Unless e s k)) sco = (env, defEnv, cv, (Unless e2 s k))
    where e2 = evalExp (env, defEnv, cv, e) sco
evalStat (env, defEnv, cv, (Case x [])) sco          = (env, defEnv, cv, Skip)
evalStat (env, defEnv, cv, (Case x ((v, s):xs))) sco = case x of 
    (DeRef (Values y)) -> case v of 
                          (DeRef (Values (ValueString "default"))) -> (env, defEnv, cv, s)
                          (DeRef (Values n)) -> if (y==n) then (env, defEnv, cv, s) else (env, defEnv, cv, (Case x xs))
                          _                  -> (env, defEnv, cv, (Case x ((e, s):xs))) 
                                                where e = evalExp (env, defEnv, cv, v) sco
    _                  -> (env, defEnv, cv, (Case e ((v, s):xs))) 
                          where e = evalExp (env, defEnv, cv, x) sco
evalStat (env, defEnv, cv, (Resource x y rs)) sco = case y of 
    (DeRef (Values (ValueString n))) -> if (valueRes rs) then (env, defEnv, (extendCat cv (x,n,toValueRes rs) ), Skip)
                                        else (env, defEnv, cv, (Resource x y (evaltoListValue env defEnv cv sco rs)))
    (DeRef (Values _ ))              -> error "wrong type of resource name"
    _                                -> (env, defEnv, cv, (Resource x e rs)) 
                                         where e = evalExp (env, defEnv, cv, y) sco
evalStat (env, defEnv, cv, (Include a)) sco = 
    case (lookupDef a defEnv) of
      (DeclaredClass _ )       -> (env, defEnv, cv, Skip) 
      (ClassDef Nothing p s)   -> (env, (changeDef defEnv a (baseof defEnv sco)), cv, (ScopeStat (SClass a) (ClassCont (mergeParams [] p) s)))
      (ClassDef (Just b) p s)  -> case (lookupDef b defEnv) of 
                                    (DeclaredClass _ ) -> (env, (changeDef defEnv a (SClass b)), cv, (ScopeStat (SClass a) (ClassCont (mergeParams [] p) s)))
                                    (ClassDef _ p s)   -> (env, defEnv, cv, (StatementsList [(Include b), (Include a)]))
evalStat (env, defEnv, cv, (ClassCont p s)) sco = 
    ((env ++ (extendEnv sco (evaltoListValue env defEnv cv sco p))), defEnv, cv, s)
evalStat (env, defEnv, cv, (ClassDecl a as)) sco =  
    case (lookupDef a defEnv) of 
    (DeclaredClass _ ) -> error ("Duplicate declaration of class '" ++ a ++ "'")
    (ClassDef Nothing ps s) ->
       (env, (changeDef defEnv a (baseof defEnv sco)), cv,
        (ScopeStat (SClass a) (ClassCont (mergeParams as ps) s)))
    (ClassDef (Just b) ps s) ->
        case (lookupDef b defEnv) of 
          (DeclaredClass _) -> (env, (changeDef defEnv a (SClass b)), cv, (ScopeStat (SClass a) (ClassCont (mergeParams as ps) s)))
          (ClassDef _ _ _ ) -> (env, defEnv, cv, (StatementsList [(Include b), (ClassDecl a as)]))
evalStat (env, defEnv, cv, (ResTypeDecl t title as)) sco
   | isVal title = 
    case (lookupDef t defEnv) of 
      (ResTypeDef t p s) -> if (valueRes as) 
                            then (env, defEnv, cv, (ScopeStat (SDef sco) (ResTypeCont t (("title",title):("name",title):mergeParams as p) s)))
                            else (env, defEnv, cv, (ResTypeDecl t title (evaltoListValue env defEnv cv sco as)))
      _                  -> error "resource is not defined"
   | otherwise = (env, defEnv, cv, (ResTypeDecl t (evalExp (env, defEnv, cv, title) sco) as))
evalStat (env, defEnv, cv, (ResTypeCont t p s)) sco = 
    if (valueRes p) 
    then ((env ++ (extendEnv sco p)), defEnv, cv, s)
    else (env, defEnv, cv, (ResTypeCont t (evaltoListValue env defEnv cv sco p) s))
evalStat (env, defEnv, cv, (ScopeStat (SDef a) Skip)) sco = (clearScope (SDef a) env, defEnv, cv, Skip)
evalStat (env, defEnv, cv, (ScopeStat a Skip)) sco = (env, defEnv, cv, Skip)
evalStat (env, defEnv, cv, (ScopeStat sco' s)) sco =
  let (env', defEnv', cv', s') = (evalStat (env, defEnv, cv, s) sco') in (env', defEnv', cv', (ScopeStat sco' s'))
evalStat (env, defEnv, cv, (StatementsList [])) sco = (env, defEnv, cv, Skip)
evalStat (env, defEnv, cv, (StatementsList (Skip:xs))) sco = (env, defEnv, cv, (StatementsList xs))         
evalStat (env, defEnv, cv, (StatementsList (s:xs)))  sco = 
    let (env', defEnv', cv', s') = (evalStat (env, defEnv, cv, s) sco) in
    (env', defEnv', cv', (StatementsList (s':xs))) 


evaltoListValue :: Env -> DefEnv -> Catalog -> Scope ->  [(String, ValueExp)] -> [(String, ValueExp)]
evaltoListValue _ _ _ _ [] = [] 
evaltoListValue env defEnv cv sco ((x,(DeRef (Values y))):ys) = ((x, (DeRef (Values y))):(evaltoListValue env defEnv cv sco ys))  
evaltoListValue env defEnv cv sco ((x,y):ys) = ((x, (evalExp (env, defEnv, cv, y) sco)):ys)

-- variables with referred scope is assignable from upper scope? 

isVal :: ValueExp -> Bool
isVal (DeRef (Values v)) = True
isVal _ = False

valueRes :: [(String, ValueExp)] -> Bool
valueRes []                           = True
valueRes ((x, (DeRef (Values v))):as) = valueRes as
valueRes _                            = False

toValueRes :: [(String, ValueExp)] -> [(String, Value)]
toValueRes []                           = []
toValueRes ((x, (DeRef (Values v))):as) = ((x, v):(toValueRes as))
toValueRes _                            = error "not all element is a value"


evalProgEle :: States ProgramEle -> Name -> States ProgramEle
-- evalProgEle (env, defEnv, cv, (Node n s)) name | name == n = let (env', defEnv', cv', s') = (evalStat (env, defEnv, cv, s) SNode) in (env', defEnv', cv', (Node n s'))
--                                                  | otherwise = error $ "node name " ++ (show n) ++ " does not match " ++ (show name)
evalProgEle (env, defEnv, cv, (Node n s)) name | name == n = (env, defEnv, cv, ProStatement (ScopeStat SNode s))
                                               | otherwise = (env, defEnv, cv, ProSkip)
evalProgEle (env, defEnv, cv, (Class a p b s)) name = 
    case (isDef defEnv a) of 
      False -> (env, (defEnv ++ [(a, (ClassDef b p s))]), cv, ProSkip)
      True  -> error "Class is defined"
evalProgEle (env, defEnv, cv, (DefResType t p s)) name = 
    case (isDef defEnv t) of 
      False -> (env, (defEnv ++ [(t, (ResTypeDef t p s))]), cv, ProSkip)
      True  -> error "Resource type is defined"
evalProgEle (env, defEnv, cv, (ProStatement Skip)) name = 
    (env, defEnv, cv, ProSkip)
evalProgEle (env, defEnv, cv, (ProStatement s)) name = 
    let (env', defEnv', cv', s') = (evalStat (env, defEnv, cv, s) STop)
    in (env', defEnv', cv', (ProStatement s'))

evalProg :: States Program -> Name -> States Program
evalProg (env, defEnv, cv, []) n = (env, defEnv, cv, [])
evalProg (env, defEnv, cv, (ProSkip:ps)) n = (env, defEnv, cv, ps)
evalProg (env, defEnv, cv, (p:ps)) n = let (env', defEnv', cv', p') = (evalProgEle (env, defEnv, cv, p) n) in (env', defEnv', cv', (p':ps))




{------------------------------------------------------------------------------
    Evaluate AST for Puppet program & return the catalog
------------------------------------------------------------------------------}


evalPuppet :: CState -> AST -> IO (Either [String] Catalog)
evalPuppet st raw_ast = do { return (Right catalog') } where

	-- evaluate in steps
	(env', defEnv', catalog', ast') = 
		evalNSteps steps (env, defEnv, catalog, ast)

	-- initial values
	env = []
	defEnv = []
	catalog = []
	name = nodeName $ sOpts st
	steps = stepLimit $ sOpts st
	showTrace = (verbosity $ sOpts st) /= Normal
        ast = case mainClass $ sOpts st of
                Nothing -> raw_ast
                Just main -> raw_ast ++ [ProStatement (Include main)]

	-- evaluate steps with a trace of each step (if showTrace true)
	evalNSteps limit states@(_,_,_,ast) =
		if showTrace
			then trace (show ast) states' 
			else states'
		where states' = evalNSteps' limit states 

	-- evaluate steps
	-- stop when we program is reduced to a skip
	-- or stop when steplimit is reached
	evalNSteps' limit states@(_,_,_,ast) =
		case ast of
			-- reduced to a skip
			[] -> states
			-- check the limit
			otherwise -> case limit of
				Just 0 -> error ("Evaluation incomplete with result " ++ show states)
				Just n ->  evalNSteps (Just (n-1)) states'
				Nothing -> evalNSteps Nothing states'
		where
			states' = evalProg states name 
