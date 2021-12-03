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

import Data.Either
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import System.Environment

import Interface.ParseArgs

import qualified Lexer.Lexer as L
import qualified Lexer.Token as LT

import qualified Parser.Parser as P

main :: IO ()
main = do
  args <- getArgs
  checkedArgs <- checkArgs $ parseFromArgs args
  if argsInvalidated checkedArgs then print checkedArgs
  else do
    filesContents <- mapM TIO.readFile $ map T.unpack $ inputFiles checkedArgs
    let lexed = map (L.lexer 0 1) filesContents
    if any isJust $ map L.lexerFailed lexed then mapM_ (mapM_ putStrLn) $ map (map T.unpack) $ map fromJust $ filter isJust $ map L.lexerFailed lexed
    else do
      let parsed = map (P.parser) lexed
      print $ map LT.tokenType $ fromRight undefined $ lexed !! 0
