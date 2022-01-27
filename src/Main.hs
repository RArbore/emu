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

import Data.Either
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc

import System.Environment
import System.IO

import Interface.ParseArgs

import Text.Megaparsec

import qualified Parser.Parser as P
import qualified Parser.AST as PA

import qualified Semantics.Check as SC
import qualified Semantics.Error as SE
import Semantics.Marshal
import qualified Semantics.SAST as SA

foreign import capi "lib.h c_entry_point" c_entry_point :: Ptr SA.SAST -> CString -> CString -> IO (CInt)

main :: IO ()
main = do
  args <- getArgs
  checkedArgs <- checkArgs $ parseFromArgs args
  if argsInvalidated checkedArgs then print checkedArgs
  else do
    let (emuFiles, objFiles) = partition (T.isSuffixOf $ T.pack ".emu") $ inputFiles checkedArgs
    filesContents <- mapM TIO.readFile $ map T.unpack $ emuFiles
    let parsed = zipWith (runParser P.pProgram) (map T.unpack emuFiles) filesContents 
    if not $ null $ lefts parsed then mapM_ putStrLn $ map errorBundlePretty $ lefts parsed
    else do
      let checked = map ((\x -> runState x (SC.Environment M.empty M.empty M.empty Nothing)) . runExceptT . SC.check . fromRight undefined) parsed
      if not $ null $ lefts $ map fst checked then mapM_ putStrLn $ zipWith ($) (map uncurry $ map SE.showSError $ lefts $ map fst checked) $ map snd $ filter fst $ zip (map isLeft $ map fst checked) $ zip emuFiles filesContents
      else do
        let sasts = map (fromRight undefined . fst) checked
            codegen sast fileName moduleName = do
              ptr <- callocBytes (sizeOf sast)
              poke ptr sast
              cFileName <- newCString $ fileName
              cModuleName <- newCString $ moduleName
              statusCode <- c_entry_point ptr cFileName cModuleName
              free cFileName
              free cModuleName
              return statusCode
        pathsAndHandles <- mapM (openTempFile ".") (map ((\x -> x ++ ".o") . T.unpack) emuFiles)
        mapM_ hClose $ map snd pathsAndHandles
        statusCodes <- sequence $ zipWith3 codegen sasts (map fst pathsAndHandles) (map (T.unpack . fromJust . T.stripSuffix (T.pack ".emu")) emuFiles)
        return ()
