module BannedFuncs (bannedFuncs) where

import Data.IORef
import Control.Monad.IO.Class

import DynFlags
import GHC
import GHC.LanguageExtensions
import GHC.Paths (libdir)
import Name
import HscTypes
import NameEnv
import OccName
import Outputable
import RdrName
import TcRnTypes


{-
Uses the GHC API to load a given module and parse it for all imported functions.
The imported functions are converted to strings and returned.

Example usage: bannedFuncs "src/Permute.hs" "Permute"
-}
bannedFuncs :: String -> String -> [String] -> IO Bool 
bannedFuncs modPath modName = runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        let compdflags = foldl xopt_set dflags [Cpp, ImplicitPrelude, MagicHash]
        setSessionDynFlags compdflags
        target <- guessTarget modPath Nothing
        setTargets [target]
        load LoadAllTargets

        modSum <- getModSummary $ mkModuleName modName
        parsedModule <- parseModule modSum
        tmod <- typecheckModule parsedModule
        let (tcenv, moddets) = tm_internals_ tmod

        let names = map getOccString $ concatMap (map gre_name) $ occEnvElts $ tcg_rdr_env tcenv
        return names