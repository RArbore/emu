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

import Parser.AST (Modifier (Inline))
    
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

inlineExpr :: Function -> [VarBinding] -> Expression -> State Int (Expression, [Declaration])
inlineExpr = undefined

ensureInBlock :: Statement -> [Declaration]
ensureInBlock (Block ds) = ds
ensureInBlock s = [StatementDecl s]

renameVarsInFunc :: Function -> Int -> [VarBinding] -> Function
renameVarsInFunc (Function sig s) n gs = let globalNames = map (\(VarBinding (DecoratedIdentifier _ n _) _) -> n) gs
                                         in Function sig $ rename globalNames n s 

class Renamable d where
    rename :: [T.Text] -> Int -> d -> d

instance Renamable Function where
    rename gs num f = (\(FuncDecl x) -> x) $ rename gs num (FuncDecl f)

instance Renamable Declaration where
    rename gs num (VarDecl (VarBinding di e)) = (VarDecl (VarBinding (rename gs num di) (rename gs num e)))
    rename gs num (FuncDecl (Function (FunctionSignature m n dis dt) s)) = (FuncDecl (Function (FunctionSignature m n (map (rename gs num) dis) dt) $ rename gs num s))
    rename gs num (StatementDecl s) = StatementDecl $ rename gs num s
    rename _ _ x = x

instance Renamable Statement where
    rename gs num (ExpressionStatement e) = ExpressionStatement $ rename gs num e
    rename gs num (IfElseStatement e s1 s2 b1 b2) = IfElseStatement (rename gs num e) (rename gs num s1) (rename gs num s2) b1 b2
    rename gs num (DoWhileStatement e s b) = DoWhileStatement (rename gs num e) (rename gs num s) b
    rename gs num (ReturnStatement e) = ReturnStatement $ rename gs num e
    rename gs num (Block ds) = Block $ map (rename gs num) ds
    rename _ _ EmptyStatement = EmptyStatement

instance Renamable Expression where
    rename gs num (Binary bop e1 e2 dt) = Binary bop (rename gs num e1) (rename gs num e2) dt
    rename gs num (Unary uop e dt) = Unary uop (rename gs num e) dt
    rename gs num (Literal cv) = Literal cv
    rename gs num (Array es) = Array $ map (rename gs num) es
    rename gs num (Call fn es dt) = Call fn (map (rename gs num) es) dt
    rename gs num (Cast e dt) = Cast (rename gs num e) dt
    rename gs num (LValueExpression lv) = LValueExpression $ rename gs num lv
    rename gs num (Assign aop lv e) = Assign aop (rename gs num lv) (rename gs num e)
    rename gs num (Address lv) = Address $ rename gs num lv
    rename gs num (Crement cop lv dt) = Crement cop (rename gs num lv) dt
    rename _ _ Undefined = Undefined

instance Renamable LValue where
    rename gs num (Dereference e dt) = Dereference (rename gs num e) dt
    rename gs num (Access lv w dt) = Access (rename gs num lv) w dt
    rename gs num (Index lv e dt w) = Index (rename gs num lv) (rename gs num e) dt w
    rename gs num (Identifier n dt) = Identifier (rename gs num n) dt

instance Renamable DecoratedIdentifier where
    rename gs num (DecoratedIdentifier m n d) = (DecoratedIdentifier m (rename gs num n) d)

instance Renamable T.Text where
    rename gs num t = if t `elem` gs
                      then t
                      else (T.pack "@") `T.append` (T.pack $ show num) `T.append` t

fName :: Function -> T.Text
fName (Function (FunctionSignature _ n _ _) _) = n
