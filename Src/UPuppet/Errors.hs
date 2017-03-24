{------------------------------------------------------------------------------
    uPuppet: Error Handling
------------------------------------------------------------------------------}

module UPuppet.Errors ( Error, Errors, showErrors, showError, parseError ) where

import Data.String.Utils (join,rstrip)
import Data.List (nub)
import Text.Parsec
import Text.Parsec.Error
import System.FilePath.Posix (takeFileName)

-- later on, we might want to have an error type which contains some
-- extra info such as source position etc ....
-- but we just have a string for now

type Error = String
type Errors = [Error]

{------------------------------------------------------------------------------
   message formatting
------------------------------------------------------------------------------}

showError :: Error -> String
showError s = ( "** " ++ (join "\n   " $ lines s) ) ++ "\n"

showErrors :: Errors -> String
showErrors errs = foldl (++) [] $ nub $ map showError errs

{------------------------------------------------------------------------------
   convert ParSec error to error string
------------------------------------------------------------------------------}

parseError :: ParseError -> Error
parseError pe =  if (isfailStep msgs)

        then posString ++ ": " ++ (failStepMessage msgs)
        else posString ++ ": " ++ ( rstrip $ unlines $ tail $ lines $ show pe )
  
    where
        msgs = errorMessages pe
        pos = errorPos pe
        posString = ( takeFileName $ sourceName pos ) ++
            "[" ++ (show $ sourceLine pos) ++ "." ++ (show $ sourceColumn pos) ++ "]"

        isfailStepMessage (Message m) = True
        isfailStepMessage _ = False

        isfailStep msgList = any isfailStepMessage msgList

        failStepMessage msgList = s
            where (Message s) = head $ filter isfailStepMessage msgList
