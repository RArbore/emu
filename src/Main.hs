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
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import System.Environment

import Interface.ParseArgs

import Text.Megaparsec

import qualified Parser.Parser as P

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
      print parsed
