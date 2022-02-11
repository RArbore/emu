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

import Data.List
import Data.Text (Text)

import Debug.Trace

import Parser.AST (Modifier (Inline))
    
import Semantics.SAST

data DependsTree = DependsTree Text [DependsTree] deriving (Show)

inlinePass :: SAST -> SAST
inlinePass (SAST decls) = let inlineFuncs = map (\(FuncDecl f) -> fName f) $ 
                                            filter (\d -> case d of
                                                            FuncDecl (Function (FunctionSignature mods _ _ _) _) -> Inline `elem` mods
                                                            otherwise -> False)
                                            decls 
                              inlineDependencies = map (\x -> ((\d -> case d of
                                                                        FuncDecl f -> fName f
                                                                        VarDecl (VarBinding (DecoratedIdentifier _ n _) _) -> n
                                                                        StructDecl (Structure _ n _) -> n) x, inlineDepends inlineFuncs x)) decls
                              dependsTree = filter (\(DependsTree _ ds) -> not $ null ds) $ createDependsTrees inlineDependencies
                          in trace (show dependsTree) $ SAST decls

class InlineDepends d where
    inlineDepends :: [Text] -> d -> [Text]

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
                                     then n:(foldl (\x y -> x `union` inlineDepends fs y) [] es)
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

createDependsTrees :: [(Text, [Text])] -> [DependsTree]
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

class Inlinable d where
    inline :: Function -> d -> d

instance Inlinable SAST where
    inline f (SAST ds) = SAST $ map (inline f) ds

instance Inlinable Declaration where
    inline = undefined
                             
instance Inlinable Statement where
    inline = undefined
                             
instance Inlinable Expression where
    inline = undefined
                             
instance Inlinable LValue where
    inline = undefined
                             
fName :: Function -> Text
fName (Function (FunctionSignature _ n _ _) _) = n
