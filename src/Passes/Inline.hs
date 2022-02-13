{-  This file is part of emu.
    emu is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    any later version.
    emu is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
    You should have received a copy of the GNU General Public License
    along with emu. If not, see <https://www.gnu.org/licenses/>.  -}

module Passes.Inline
    (

     inlinePass

    ) where

import Control.Monad.State

import Data.List
import qualified Data.Text as T

import Debug.Trace

import Parser.AST (Modifier (Inline), Type (Void))
    
import Semantics.SAST

data DependsTree = DependsTree T.Text [DependsTree] deriving (Show)

inlinePass :: SAST -> SAST
inlinePass (SAST decls) = let inlineFuncs = map (\(FuncDecl f) -> fName f) $ 
                                            filter (\d -> case d of
                                                            FuncDecl (Function (FunctionSignature mods _ _ _) _) -> Inline `elem` mods
                                                            otherwise -> False)
                                            decls 
                              inlineDependencies = map (\x -> ((\d -> case d of
                                                                        FuncDecl f -> fName f
                                                                        VarDecl (VarBinding (DecoratedIdentifier _ n _) _) -> n
                                                                        StructDecl (Structure _ n _) -> n) x, map fst $ inlineDepends inlineFuncs x)) decls
                              dependsTree = filter (\(DependsTree _ ds) -> not $ null ds) $ createDependsTrees inlineDependencies
                          in walkDependsTree dependsTree $ SAST decls

class InlineDepends d where
    inlineDepends :: [T.Text] -> d -> [(T.Text, [Expression])]

instance InlineDepends Declaration where
    inlineDepends fs (FuncDecl (Function _ s)) = inlineDepends fs s
    inlineDepends fs (VarDecl (VarBinding _ e)) = inlineDepends fs e
    inlineDepends fs (StatementDecl s) = inlineDepends fs s
    inlineDepends _ _ = []

instance InlineDepends Statement where
    inlineDepends fs (ExpressionStatement e) = inlineDepends fs e
    inlineDepends fs (IfElseStatement e s1 s2 _ _) = inlineDepends fs e `union` inlineDepends fs s1 `union` inlineDepends fs s2
    inlineDepends fs (DoWhileStatement e s _) = inlineDepends fs e `union` inlineDepends fs s
    inlineDepends fs (ReturnStatement e) = inlineDepends fs e
    inlineDepends fs (Block ds) = foldl (\x y -> x `union` inlineDepends fs y) [] ds
    inlineDepends _ EmptyStatement = []
                        
instance InlineDepends Expression where
    inlineDepends fs (Binary _ e1 e2 _) = inlineDepends fs e1 `union` inlineDepends fs e2
    inlineDepends fs (Unary _ e _) = inlineDepends fs e
    inlineDepends _ (Literal _) = []
    inlineDepends fs (Array es) = foldl (\x y -> x `union` inlineDepends fs y) [] es
    inlineDepends fs (Call n es _) = if n `elem` fs
                                     then (n, es):(foldl (\x y -> x `union` inlineDepends fs y) [] es)
                                     else foldl (\x y -> x `union` inlineDepends fs y) [] es
    inlineDepends fs (Cast e _) = inlineDepends fs e
    inlineDepends fs (LValueExpression lv) = inlineDepends fs lv
    inlineDepends fs (Assign _ lv e) = inlineDepends fs lv `union` inlineDepends fs e
    inlineDepends fs (Address lv) = inlineDepends fs lv
    inlineDepends fs (Crement _ lv _) = inlineDepends fs lv
    inlineDepends _ Undefined = []

instance InlineDepends LValue where
    inlineDepends fs (Dereference e _) = inlineDepends fs e
    inlineDepends fs (Access lv _ _) = inlineDepends fs lv
    inlineDepends fs (Index lv e _ _) = inlineDepends fs lv `union` inlineDepends fs e
    inlineDepends _ (Identifier _ _) = []

createDependsTrees :: [(T.Text, [T.Text])] -> [DependsTree]
createDependsTrees [] = []
createDependsTrees depends = let bases = map fst $ filter (\(_, x) -> null x) depends
                             in map (\x -> DependsTree x $
                                           createDependsTrees $
                                           map (\(n, d) -> (n, delete x d)) $
                                           filter (\(_, x) -> not $ null x) $
                                           filter (\(n, _) -> x /= n) depends) bases

walkDependsTree :: [DependsTree] -> SAST -> SAST
walkDependsTree [] s = s
walkDependsTree ((DependsTree n sxs):xs) s@(SAST ds) = walkDependsTree xs (walkDependsTree sxs (inline ((\(FuncDecl f) -> f) $ head $ filter (\x -> case x of
                                                                                                                                                      FuncDecl f -> fName f == n
                                                                                                                                                      otherwise -> False) ds) s))

inline :: Function -> SAST -> SAST
inline f (SAST ds) = SAST $ inlineHelperD f [] ds
    where
      inlineHelperD :: Function -> [VarBinding] -> [Declaration] -> [Declaration]
      inlineHelperD _ _ [] = []
      inlineHelperD f gs (d:ds) = case d of
                                    FuncDecl func -> (FuncDecl $ inlineHelperF f gs func):(inlineHelperD f gs ds)
                                    VarDecl var -> d:(inlineHelperD f (var:gs) ds)
                                    otherwise -> d:(inlineHelperD f gs ds)
      inlineHelperF :: Function -> [VarBinding] -> Function -> Function
      inlineHelperF ifunc gs (Function sig s) = Function sig $ Block (evalState (inlineInsideFunc ifunc gs $ ensureInBlock s) 0)

inlineInsideFunc :: Function -> [VarBinding] -> [Declaration] -> State Int [Declaration]
inlineInsideFunc _ _ [] = return []
inlineInsideFunc ifunc gs (d:ds) = case d of
                                     StatementDecl s ->
                                         case s of
                                           ExpressionStatement e -> do
                                                  (newE, inlined) <- inlineExpr ifunc gs e
                                                  after <- inlineInsideFunc ifunc gs ds
                                                  return (inlined ++ ensureInBlock (ExpressionStatement newE) ++ after)
                                           IfElseStatement e s1 s2 b1 b2 -> do
                                                  (newE, inlined) <- inlineExpr ifunc gs e
                                                  pos <- inlineInsideFunc ifunc gs $ ensureInBlock s1
                                                  neg <- inlineInsideFunc ifunc gs $ ensureInBlock s2
                                                  after <- inlineInsideFunc ifunc gs ds
                                                  return (inlined ++ ensureInBlock (IfElseStatement newE (Block pos) (Block neg) b1 b2) ++ after)
                                           DoWhileStatement e s b -> do
                                                  (newE, inlined) <- inlineExpr ifunc gs e
                                                  body <- inlineInsideFunc ifunc gs $ ensureInBlock s
                                                  after <- inlineInsideFunc ifunc gs ds
                                                  return (inlined ++ ensureInBlock (DoWhileStatement newE (Block body) b) ++ after)
                                           ReturnStatement e -> do
                                                  (newE, inlined) <- inlineExpr ifunc gs e
                                                  after <- inlineInsideFunc ifunc gs ds
                                                  return (inlined ++ ensureInBlock (ReturnStatement newE) ++ after)
                                           Block bs -> do
                                                  curr <- inlineInsideFunc ifunc gs bs
                                                  after <- inlineInsideFunc ifunc gs ds
                                                  return $ curr ++ after
                                           EmptyStatement -> inlineInsideFunc ifunc gs ds
                                     VarDecl (VarBinding di e) -> do
                                            (newE, inlined) <- inlineExpr ifunc gs e
                                            after <- inlineInsideFunc ifunc gs ds
                                            return (inlined ++ [VarDecl (VarBinding di newE)] ++ after)

ensureInBlock :: Statement -> [Declaration]
ensureInBlock (Block ds) = ds
ensureInBlock s = [StatementDecl s]

inlineExpr :: Function -> [VarBinding] -> Expression -> State Int (Expression, [Declaration])
inlineExpr ifunc gs e = do
  let dependencies = inlineDepends [fName ifunc] e
  case dependencies of
    [] -> return (e, [])
    (d:ds) -> do
      (newE, inline) <- inlineInstance ifunc gs e
      (restE, restInline) <- inlineExpr ifunc gs newE
      return (restE, inline ++ restInline)

inlineInstance :: Function -> [VarBinding] -> Expression -> State Int (Expression, [Declaration])
inlineInstance ifunc gs e = do
  num <- get
  let retValName = T.concat [T.pack "@", fName ifunc, T.pack "RETURN", T.pack $ show num]
      retVal = VarDecl $ VarBinding (DecoratedIdentifier [] retValName $ fRetType ifunc) Undefined
      rfunc = renameVarsInFunc ifunc num gs retValName $ fRetType ifunc
      newE = evalState (replaceCall (fName ifunc) retValName (fRetType ifunc) e) True
  put $ num + 1
  return $ if fRetType ifunc == PureType Void
           then (Undefined, ensureInBlock $ fBody rfunc)
           else (newE, retVal:(ensureInBlock $ fBody rfunc))

renameVarsInFunc :: Function -> Int -> [VarBinding] -> T.Text -> DecoratedType -> Function
renameVarsInFunc (Function sig s) n gs rvn rvt = let globalNames = map (\(VarBinding (DecoratedIdentifier _ n _) _) -> n) gs
                                                 in Function sig $ rename globalNames n rvn rvt s 

class Renamable d where
    rename :: [T.Text] -> Int -> T.Text -> DecoratedType -> d -> d

instance Renamable Function where
    rename gs num rvn rvt f = (\(FuncDecl x) -> x) $ rename gs num rvn rvt (FuncDecl f)

instance Renamable Declaration where
    rename gs num rvn rvt (VarDecl (VarBinding di e)) = (VarDecl (VarBinding (rename gs num rvn rvt di) (rename gs num rvn rvt e)))
    rename gs num rvn rvt (FuncDecl (Function (FunctionSignature m n dis dt) s)) = (FuncDecl (Function (FunctionSignature m n (map (rename gs num rvn rvt) dis) dt) $ rename gs num rvn rvt s))
    rename gs num rvn rvt (StatementDecl s) = StatementDecl $ rename gs num rvn rvt s
    rename _ _ _ _ x = x

instance Renamable Statement where
    rename gs num rvn rvt (ExpressionStatement e) = ExpressionStatement $ rename gs num rvn rvt e
    rename gs num rvn rvt (IfElseStatement e s1 s2 b1 b2) = IfElseStatement (rename gs num rvn rvt e) (rename gs num rvn rvt s1) (rename gs num rvn rvt s2) b1 b2
    rename gs num rvn rvt (DoWhileStatement e s b) = DoWhileStatement (rename gs num rvn rvt e) (rename gs num rvn rvt s) b
    rename gs num rvn rvt (ReturnStatement e) = let assignTo = rename gs num rvn rvt e
                                                in if assignTo == Undefined then EmptyStatement else ExpressionStatement $ Assign Equals (Identifier rvn rvt) assignTo
    rename gs num rvn rvt (Block ds) = Block $ map (rename gs num rvn rvt) ds
    rename _ _ _ _ EmptyStatement = EmptyStatement

instance Renamable Expression where
    rename gs num rvn rvt (Binary bop e1 e2 dt) = Binary bop (rename gs num rvn rvt e1) (rename gs num rvn rvt e2) dt
    rename gs num rvn rvt (Unary uop e dt) = Unary uop (rename gs num rvn rvt e) dt
    rename _ _ _ _ (Literal cv) = Literal cv
    rename gs num rvn rvt (Array es) = Array $ map (rename gs num rvn rvt) es
    rename gs num rvn rvt (Call fn es dt) = Call fn (map (rename gs num rvn rvt) es) dt
    rename gs num rvn rvt (Cast e dt) = Cast (rename gs num rvn rvt e) dt
    rename gs num rvn rvt (LValueExpression lv) = LValueExpression $ rename gs num rvn rvt lv
    rename gs num rvn rvt (Assign aop lv e) = Assign aop (rename gs num rvn rvt lv) (rename gs num rvn rvt e)
    rename gs num rvn rvt (Address lv) = Address $ rename gs num rvn rvt lv
    rename gs num rvn rvt (Crement cop lv dt) = Crement cop (rename gs num rvn rvt lv) dt
    rename _ _ _ _ Undefined = Undefined

instance Renamable LValue where
    rename gs num rvn rvt (Dereference e dt) = Dereference (rename gs num rvn rvt e) dt
    rename gs num rvn rvt (Access lv w dt) = Access (rename gs num rvn rvt lv) w dt
    rename gs num rvn rvt (Index lv e dt w) = Index (rename gs num rvn rvt lv) (rename gs num rvn rvt e) dt w
    rename gs num rvn rvt (Identifier n dt) = Identifier (rename gs num rvn rvt n) dt

instance Renamable DecoratedIdentifier where
    rename gs num rvn rvt (DecoratedIdentifier m n d) = (DecoratedIdentifier m (rename gs num rvn rvt n) d)

instance Renamable T.Text where
    rename gs num _ _ t = if t `elem` gs
                      then t
                      else (T.pack "@") `T.append` (T.pack $ show num) `T.append` t

class ReplaceCall d where
    replaceCall :: T.Text -> T.Text -> DecoratedType -> d -> State Bool d

instance ReplaceCall Declaration where
    replaceCall n rn t (VarDecl (VarBinding di e)) = (\x -> VarDecl $ VarBinding di x) <$> replaceCall n rn t e
    replaceCall n rn t (StatementDecl s) = StatementDecl <$> replaceCall n rn t s
    replaceCall _ _ _ x = return x

instance ReplaceCall Statement where
    replaceCall n rn t (ExpressionStatement e) = ExpressionStatement <$> replaceCall n rn t e
    replaceCall n rn t (IfElseStatement e s1 s2 b1 b2) = IfElseStatement <$> replaceCall n rn t e <*> replaceCall n rn t s1 <*> replaceCall n rn t s2 <*> return b1 <*> return b2
    replaceCall n rn t (DoWhileStatement e s b) = DoWhileStatement <$> replaceCall n rn t e <*> replaceCall n rn t s <*> return b
    replaceCall n rn t (ReturnStatement e) = ReturnStatement <$> replaceCall n rn t e
    replaceCall n rn t (Block ds) = Block <$> mapM (replaceCall n rn t) ds
    replaceCall _ _ _ EmptyStatement = return EmptyStatement
    
instance ReplaceCall Expression where
    replaceCall n rn t (Binary bop e1 e2 dt) = Binary <$> return bop <*> replaceCall n rn t e1 <*> replaceCall n rn t e2 <*> return dt 
    replaceCall n rn t (Unary uop e dt) = Unary <$> return uop <*> replaceCall n rn t e <*> return dt
    replaceCall _ _ _ (Literal cv) = return $ Literal cv
    replaceCall n rn t (Call fn es dt) = get >>= (\x -> if x && n == fn then (do put False
                                                                                 return $ LValueExpression $ Identifier rn t) else Call <$> return fn <*> mapM (replaceCall n rn t) es <*> return dt)
    replaceCall n rn t (LValueExpression lv) = LValueExpression <$> replaceCall n rn t lv
    replaceCall n rn t (Assign aop lv e) = Assign <$> return aop <*> replaceCall n rn t lv <*> replaceCall n rn t e
    replaceCall n rn t (Address lv) = Address <$> replaceCall n rn t lv
    replaceCall n rn t (Crement cop lv dt) = Crement <$> return cop <*> replaceCall n rn t lv <*> return dt
    replaceCall _ _ _ Undefined = return Undefined
                           
instance ReplaceCall LValue where
    replaceCall n rn t (Dereference e dt) = Dereference <$> replaceCall n rn t e <*> return dt
    replaceCall n rn t (Access lv w dt) = Access <$> replaceCall n rn t lv <*> return w <*> return dt
    replaceCall n rn t (Index lv e dt w) = Index <$> replaceCall n rn t lv <*> replaceCall n rn t e <*> return dt <*> return w
    replaceCall _ _ _ (Identifier n t) = return $ Identifier n t
                           
fName :: Function -> T.Text
fName (Function (FunctionSignature _ n _ _) _) = n

fRetType :: Function -> DecoratedType
fRetType (Function (FunctionSignature _ _ _ r) _) = r

fBody :: Function -> Statement
fBody (Function _ s) = s
