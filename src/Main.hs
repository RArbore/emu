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

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}

import Control.Monad.State
import Control.Monad.Except

import Data.Bifunctor
import Data.Either
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Foreign
import Foreign.Marshal.Alloc

import System.Environment

import Interface.ParseArgs

import Text.Megaparsec

import qualified Parser.Parser as P
import qualified Parser.AST as PA

import qualified Semantics.Check as SC
import qualified Semantics.Error as SE
import Semantics.Marshal
import qualified Semantics.SAST as SA

foreign import capi "codegen.h print_comptime_value" print_comptime_value :: Ptr SA.ComptimeValue -> IO ()

main :: IO ()
main = do
  args <- getArgs
  checkedArgs <- checkArgs $ parseFromArgs args
  if argsInvalidated checkedArgs then print checkedArgs
  else do
    filesContents <- mapM TIO.readFile $ map T.unpack $ inputFiles checkedArgs
    let parsed = zipWith (runParser P.pProgram) (map T.unpack $ inputFiles checkedArgs) filesContents 
    if not $ null $ lefts parsed then mapM_ putStrLn $ map errorBundlePretty $ lefts parsed
    else do
      let checked = map ((\x -> runState x (SC.Environment M.empty M.empty M.empty Nothing)) . runExceptT . SC.check . fromRight undefined) parsed
      if not $ null $ lefts $ map fst checked then mapM_ putStrLn $ zipWith ($) (map uncurry $ map SE.showSError $ lefts $ map fst checked) $ map snd $ filter fst $ zip (map isLeft $ map fst checked) $ zip (inputFiles checkedArgs) filesContents
      else do
        print $ map (fst . bimap (fromRight undefined) id) checked
{-      let compVal = SA.ComptimeStruct [SA.ComptimeBool True, SA.ComptimeF32 1.5, SA.ComptimePointer 5 $ SA.PureType $ PA.I32] $ T.pack "a_struct"
        ptr <- callocBytes (sizeOf compVal)
        poke ptr compVal
        print_comptime_value ptr
        free ptr
        let decIden = SA.DecoratedIdentifier [PA.Const, PA.Register] (T.pack "a_dec_iden") $ SA.DerefType $ SA.PureType $ PA.StructType $ T.pack "a_struct_name"
        ptr <- callocBytes (sizeOf decIden)
        poke ptr decIden
        print_decorated_identifier ptr
        free ptr
        print decIden -}
