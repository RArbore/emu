{-  This file is part of rwm.
    rwm is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    any later version.
    rwm is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
    You should have received a copy of the GNU General Public License
    along with rwm. If not, see <https://www.gnu.org/licenses/>.  -}

module Interface.ParseArgs
  (

    ParsedArgs (..),

    parseFromArgs,
    checkArgs,
    argsInvalidated

  ) where

import System.Directory

import qualified Data.Text as T

data ParsedArgs = ParsedArgs { inputFiles :: [T.Text],
                               outputObject :: T.Text }
                | InvalidArgs { errors :: [T.Text] }

instance Show ParsedArgs where
  show (ParsedArgs inFiles outObject) = T.unpack $ T.pack "Input files:" `T.append` T.concat (map (T.cons ' ') $ inFiles) `T.append` (T.singleton '\n') `T.append` (T.pack "Output object file: ") `T.append` outObject
  show (InvalidArgs err) = T.unpack $ T.intercalate (T.singleton '\n') $ map (T.pack "ERROR: " `T.append`) err

addInputFile :: ParsedArgs -> T.Text -> ParsedArgs
addInputFile inv@(InvalidArgs _)  _ = inv
addInputFile args@(ParsedArgs _ _) file = args {inputFiles = file:(inputFiles args)}

setOutputObject :: ParsedArgs -> T.Text -> ParsedArgs
setOutputObject inv@(InvalidArgs _) _ = inv
setOutputObject args@(ParsedArgs _ _) obj = args {outputObject = obj}

addErrors :: ParsedArgs -> T.Text -> ParsedArgs
addErrors (InvalidArgs existingErrors) newError = InvalidArgs $ newError:existingErrors
addErrors (ParsedArgs _ _) newError = InvalidArgs [newError]

parseFromArgs :: [String] -> ParsedArgs
parseFromArgs (x:xs)
  | x == "-o" = if null xs then addErrors (parseFromArgs $ xs) $ T.pack "-o flag requires an argument (filename to output object file to)" else setOutputObject (parseFromArgs $ tail xs) $ T.pack $ head xs
  | head x == '-' = addErrors (parseFromArgs $ xs) $ T.pack $ "Invalid argument " ++ x
  | otherwise = addInputFile (parseFromArgs $ xs) $ T.pack x
parseFromArgs [] = ParsedArgs [] $ T.empty

checkArgs :: ParsedArgs -> IO (ParsedArgs)
checkArgs inv@(InvalidArgs _) = do return inv
checkArgs args@(ParsedArgs _ _) = do
  existChecks <- mapM doesFileExist (map T.unpack $ inputFiles args)
  return $ checkArgsExistChecks 0 existChecks args
  where checkArgsExistChecks n (b:bs) cargs
          | b = checkArgsExistChecks (n + 1) bs cargs
          | otherwise = addErrors (checkArgsExistChecks (n + 1) bs cargs) $ T.pack $ "Couldn't find file at " ++ (T.unpack $ inputFiles cargs !! n)
        checkArgsExistChecks _ _ cargs = cargs

argsInvalidated :: ParsedArgs -> Bool
argsInvalidated (InvalidArgs _) = True
argsInvalidated (ParsedArgs _ _) = False
