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

module Interface.Error
  (

    Error (..),

  ) where

import qualified Data.Text as T

data Error = Error { error :: T.Text,
                     filename :: T.Text,
                     problematic :: T.Text,
                     line :: Int,
                     startColumn :: Int,
                     endColumn :: Int }

instance Show Error where
  show (Error e f p l sc ec) = T.unpack f
                                   ++ ":"
                                   ++ show l
                                   ++ ":"
                                   ++ show sc
                                   ++ ": ERROR:\n"
                                   ++ take (2 + spacing) (repeat ' ')
                                   ++ T.unpack e
                                   ++ "\n"
                                   ++ take spacing (repeat ' ')
                                   ++ " |\n"
                                   ++ show l
                                   ++ " |   "
                                   ++ T.unpack p
                                   ++ "\n"
                                   ++ take spacing (repeat ' ')
                                   ++ " |   "
                                   ++ take sc (repeat ' ')
                                   ++ take (ec - sc) (repeat '^')
                                   ++ "\n"
    where spacing = length $ show l
  
