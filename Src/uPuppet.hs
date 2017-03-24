{------------------------------------------------------------------------------
    uPuppet: Main program
------------------------------------------------------------------------------}

import UPuppet.Errors
import UPuppet.Options
import UPuppet.CState
import UPuppet.AST
import UPuppet.Catalog

import UPuppet.Parser
import UPuppet.Eval
import UPuppet.ShowAST
import UPuppet.ShowCatalog
import UPuppet.ShowJSON

import System.Environment (getArgs)
import System.IO (hPutStr, stderr)
import System.Exit (exitWith, ExitCode(..), exitSuccess)

{------------------------------------------------------------------------------
    main program
------------------------------------------------------------------------------}

main = do

	-- command line args
	(opts, files) <- getArgs >>= parseOptions

	-- process each file on the command line
	status <- mapM (compileAndSave opts) files

	-- return failure if any of the compilations fails
	if (and status) then exitSuccess else exitWith (ExitFailure 1)

{------------------------------------------------------------------------------
    parse/evaluate/render source from file
------------------------------------------------------------------------------}

compile :: CState -> IO (Either Errors String)
compile st = do

		-- parse
		src <- readFile $ srcPath (sOpts st)
		astOrError <- parsePuppet st src
		case astOrError of
			Right ast -> evaluate st ast
			Left errs -> return $ Left errs

	where

		-- evaluate
		evaluate :: CState -> AST -> IO (Either Errors String)
		evaluate st ast = case (format (sOpts st)) of
			AST_FORMAT -> return $ Right $ showAST st ast
			otherwise -> do
				catalogOrError <- evalPuppet st ast
				case catalogOrError of
					Right catalog -> render st catalog
					Left errs -> return $ Left errs
	
		-- render
		render :: CState -> Catalog -> IO (Either Errors String)
		render st catalog = case (format $ sOpts st) of
				CATALOG_FORMAT -> return $ Right $ showCatalog st catalog
				JSON_FORMAT -> return $ Right $ showJSON st catalog
				otherwise -> error "unsupported output format"

{------------------------------------------------------------------------------
   compile and save to file (or report errors)
------------------------------------------------------------------------------}

compileAndSave :: Opts -> String -> IO (Bool)
compileAndSave opts path = do
	let opts' = opts { srcPath=path }
	let st = newState opts'
	resultOrError <- compile st
	case resultOrError of
		Left errs -> do
			hPutStr stderr $ showErrors errs
			return False
		Right result -> do
			let dstPath = outputPath opts'
			if (dstPath == "-") then putStrLn result else writeFile dstPath (result++"\n")
			return True
