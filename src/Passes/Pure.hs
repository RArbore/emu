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

module Passes.Pure
    (

     purePass

    ) where

import Control.Monad.State

import Data.List
import Data.Maybe
import qualified Data.Text as T

import Parser.AST (Modifier (..))
    
import Semantics.SAST

data PureCall = PureCall T.Text [Expression] DecoratedType T.Text deriving (Eq)

type Purity = State ([PureCall], [T.Text], [T.Text], Bool) -- Pure calls already made, defined pure functions, defined const vars, currently inside a loop we may hoist code out of

purePass :: SAST -> SAST
purePass (SAST decls) = SAST $ pureHelperD [] decls
    where
      pureFuncs = getPureFunctions decls
      pureHelperD _ [] = []
      pureHelperD cgs (d:ds) = case d of
                                 FuncDecl func -> (FuncDecl $ pureHelperF cgs func):(pureHelperD cgs ds)
                                 VarDecl (VarBinding (DecoratedIdentifier mods n _) _) -> if Const `elem` mods then d:(pureHelperD (n:cgs) ds) else d:(pureHelperD cgs ds)
                                 otherwise -> d:(pureHelperD cgs ds)
      pureHelperF cgs (Function sig s) = Function sig $ Block $ header ++ body
          where (header, body) = evalState (purifyInsideFunc $ ensureInBlock s) ([], pureFuncs, cgs, False)

getPureFunctions :: [Declaration] -> [T.Text]
getPureFunctions = mapMaybe (\x -> case x of
                                     FuncDecl (Function (FunctionSignature mods n _ _) _) -> if Pure `elem` mods then Just n else Nothing
                                     otherwise -> Nothing)

ensureInBlock :: Statement -> [Declaration]
ensureInBlock (Block ds) = ds
ensureInBlock s = [StatementDecl s]

purifyInsideFunc :: [Declaration] -> Purity ([Declaration], [Declaration])
purifyInsideFunc [] = return ([], [])
purifyInsideFunc (d:ds) =
    case d of
      StatementDecl s ->
          do
            case s of
              ExpressionStatement e ->
                  do
                    (newE, header) <- purifyUnit e
                    (afterH, after) <- purifyInsideFunc ds
                    return (header, ensureInBlock (ExpressionStatement newE) ++ afterH ++ after)
              IfElseStatement e s1 s2 b1 b2 ->
                  do
                    (newE, header) <- purifyUnit e
                    prevState <- get
                    (posH, pos) <- purifyInsideFunc $ ensureInBlock s1
                    posState <- get
                    put prevState
                    (negH, neg) <- purifyInsideFunc $ ensureInBlock s2
                    negState <- get
                    put (tup1 posState `intersect` tup1 negState, tup2 prevState, tup3 prevState, tup4 prevState)
                    (afterH, after) <- purifyInsideFunc ds
                    return (header, ensureInBlock (IfElseStatement newE (Block $ posH ++ pos) (Block $ negH ++ neg) b1 b2) ++ afterH ++ after)
              DoWhileStatement e s b ->
                  do
                    (newE, header) <- purifyUnit e
                    prevState <- get
                    put (tup1 prevState, tup2 prevState, tup3 prevState, True)
                    (hoisted, body) <- (purifyInsideFunc $ ensureInBlock s)
                    afterState <- get
                    put (tup1 afterState, tup2 prevState, tup3 prevState, tup4 prevState)
                    (afterH, after) <- purifyInsideFunc ds
                    return (header ++ hoisted, ensureInBlock (DoWhileStatement newE (Block body) b) ++ afterH ++ after)
              ReturnStatement e ->
                  do
                    (newE, header) <- purifyUnit e
                    (afterH, after) <- purifyInsideFunc ds
                    return (header, ensureInBlock (ReturnStatement newE) ++ afterH ++ after)
              Block body ->
                  do
                    prevState <- get
                    (newBodyH, newBody) <- purifyInsideFunc body
                    put prevState
                    (afterH, after) <- purifyInsideFunc ds
                    return (newBodyH, [StatementDecl $ Block newBody] ++ afterH ++ after)
              EmptyStatement -> purifyInsideFunc ds
      VarDecl (VarBinding di@(DecoratedIdentifier mods n _) e) ->
          do
            (newE, header) <- purifyUnit e
            when (Const `elem` mods) $ modify $ \(pcs, pfs, gcs, loop) -> (pcs, pfs, n:gcs, loop)
            (afterH, after) <- purifyInsideFunc ds
            return (header, [VarDecl (VarBinding di newE)] ++ afterH ++ after)

class UnitPurifiable e where
    purifyUnit :: e -> Purity (e, [Declaration])
    purifyConst :: e -> Purity Bool

instance UnitPurifiable Expression where
    purifyUnit (Binary bop e1 e2 dt) =
        do
          (newe1, header1) <- purifyUnit e1
          (newe2, header2) <- purifyUnit e2
          return (Binary bop newe1 newe2 dt, header1 ++ header2)
    purifyUnit (Unary uop e dt) =
        do
          (newe, header) <- purifyUnit e
          return (Unary uop newe dt, header)
    purifyUnit (Literal cv) = return (Literal cv, [])
    purifyUnit (Array es) =
        do
          newes <- mapM purifyUnit es
          return (Array $ map fst newes, foldl (++) [] $ map snd newes)
    purifyUnit (Call fn es dt) =
        do
          state <- get
          newes <- mapM purifyUnit es
          isPure <- (&&) <$> return (fn `elem` tup2 state) <*> (all id <$> (mapM purifyConst $ map fst newes))
          if isPure then
              do
                let search = find (\(PureCall pfn pes pdt _) -> pfn == fn && pes == es && pdt == dt) $ tup1 state
                case search of
                  Just (PureCall pfn pes pdf pin) -> return (LValueExpression $ Identifier pin dt, [])
                  Nothing ->
                      do
                        let idenName = T.pack "@PURE" `T.append` fn `T.append` (T.pack $ show $ length $ tup1 state)
                        put ((PureCall fn (map fst newes) dt idenName):(tup1 state), tup2 state, tup3 state, tup4 state)
                        modify $ \(pcs, pfs, gcs, loop) -> (pcs, pfs, idenName:gcs, loop)
                        return (LValueExpression $ Identifier idenName dt, (foldl (++) [] $ map snd newes) ++ [VarDecl (VarBinding (DecoratedIdentifier [Const] idenName dt) (Call fn (map fst newes) dt))])
          else return (Call fn (map fst newes) dt, foldl (++) [] $ map snd newes)
    purifyUnit (Cast e dt) =
        do
          (newe, header) <- purifyUnit e
          return (Cast newe dt, header)
    purifyUnit (LValueExpression lv) =
        do
          (newlv, header) <- purifyUnit lv
          return (LValueExpression newlv, header)
    purifyUnit (Assign aop lv e) =
        do
          (newlv, headerlv) <- purifyUnit lv
          (newe, headere) <- purifyUnit e
          return (Assign aop newlv newe, headerlv ++ headere)
    purifyUnit (Address lv) = 
        do
          (newlv, header) <- purifyUnit lv
          return (Address newlv, header)
    purifyUnit (Crement cop lv dt) = 
        do
          (newlv, header) <- purifyUnit lv
          return (Crement cop newlv dt, header)
    purifyUnit Undefined = return (Undefined, [])
    purifyConst (Binary _ e1 e2 _) = (&&) <$> purifyConst e1 <*> purifyConst e2
    purifyConst (Unary _ e _) = purifyConst e
    purifyConst (Literal _) = return True
    purifyConst (Array es) = all id <$> mapM purifyConst es
    purifyConst (Call fn es dt) = do
      (_, pfns, _, _) <- get
      if fn `elem` pfns
      then all id <$> mapM purifyConst es
      else return False
    purifyConst (Cast e _) = purifyConst e
    purifyConst (LValueExpression lv) = purifyConst lv
    purifyConst (Assign _ lv e) = (&&) <$> purifyConst lv <*> purifyConst e
    purifyConst (Address lv) = purifyConst lv
    purifyConst (Crement _ lv _) = purifyConst lv
    purifyConst Undefined = return True

instance UnitPurifiable LValue where
    purifyUnit (Dereference e dt) =
        do
          (newe, header) <- purifyUnit e
          return (Dereference newe dt, header)
    purifyUnit (Access e w dt) =
        do
          (newe, header) <- purifyUnit e
          return (Access newe w dt, header)
    purifyUnit (Index lv e dt w) =
        do
          (newlv, headerlv) <- purifyUnit lv
          (newe, headere) <- purifyUnit e
          return (Index newlv newe dt w, headerlv ++ headere)
    purifyUnit (Identifier n dt) = return (Identifier n dt, [])
    purifyConst (Dereference e _) = purifyConst e
    purifyConst (Access lv _ _) = purifyConst lv
    purifyConst (Index lv e _ _) = (&&) <$> purifyConst lv <*> purifyConst e
    purifyConst (Identifier iden _) = elem iden <$> (tup3 <$> get)

tup1 :: (a, b, c, d) -> a
tup1 (a, _, _, _) = a

tup2 :: (a, b, c, d) -> b
tup2 (_, b, _, _) = b

tup3 :: (a, b, c, d) -> c
tup3 (_, _, c, _) = c

tup4 :: (a, b, c, d) -> d
tup4 (_, _, _, d) = d
