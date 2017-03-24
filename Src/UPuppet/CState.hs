{------------------------------------------------------------------------------
    uPuppet: Compiler State
------------------------------------------------------------------------------}

module UPuppet.CState
	( CResult, CState(..), newState, getOpts
	) where

import UPuppet.Options

import Control.Monad.State hiding (join)

{------------------------------------------------------------------------------
    compiler state
------------------------------------------------------------------------------}

-- this state is passed through the compiler
-- it contains the options & the error messages

type CResult a = StateT CState IO a

data CState = CState { sOpts :: Opts }

newState opts = CState
	{ sOpts=opts }

-- extract options from state

getOpts :: Monad x => (StateT CState x) Opts
getOpts = do
	state <- get
	return $ sOpts state
