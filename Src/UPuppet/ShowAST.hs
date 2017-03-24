{------------------------------------------------------------------------------
    uPuppet: AST rendering
------------------------------------------------------------------------------}

module UPuppet.ShowAST ( showAST ) where

import UPuppet.CState
import UPuppet.AST

-- on success, return Just ast-string 
-- on error, return Nothing & put errors messages in the state

showAST :: CState -> AST -> String
showAST st ast = show ast
