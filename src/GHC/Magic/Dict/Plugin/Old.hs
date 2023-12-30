{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module GHC.Magic.Dict.Plugin.Old (plugin) where

import Control.Applicative (liftA2)
import Data.Bitraversable (bitraverse)
import qualified Data.DList as DL
import Data.Maybe (mapMaybe)
import GHC.Builtin.Types.Prim (openAlphaTy, openAlphaTyVar, runtimeRep1TyVar)
import qualified GHC.Core as Core
import GHC.Core.Class
import GHC.Core.Coercion (mkSubCo, mkSymCo, mkTransCo)
import GHC.Core.DataCon
import GHC.Core.Make (mkCoreLams)
import GHC.Core.Predicate
import GHC.Core.TyCon
import GHC.Core.Type
import GHC.Data.FastString
import GHC.Plugins (Plugin (..), defaultPlugin, mkModuleName)
import GHC.Tc.Instance.Family (tcInstNewTyCon_maybe)
import GHC.Tc.Plugin hiding (newWanted)
import GHC.Tc.Types
import GHC.Tc.Types.Constraint
import GHC.Tc.Types.Evidence
import GHC.TcPluginM.Extra
import GHC.Types.Id
import GHC.Types.Name

plugin :: Plugin
plugin = defaultPlugin {tcPlugin = const $ Just withDictPlugin}

withDictPlugin :: TcPlugin
withDictPlugin =
  TcPlugin
    { tcPluginStop = const $ pure ()
    , tcPluginSolve = const solveWithDict
    , tcPluginInit = pure ()
    }

data Info = Info
  { _WithDict :: !Class
  , _WithDictDataCon :: !DataCon
  , _unsafeWithDict :: !TyCon
  }

solveWithDict :: TcPluginSolver
solveWithDict _gs _ds wanteds = do
  info <- lookupInfo
  let withDicts =
        mapMaybe
          ( liftA2 (,)
              <$> (liftA2 (,) <$> pure . ctLoc <*> decodeWithDictPred info . ctPred)
              <*> pure
          )
          wanteds
  (contrs, solved, wants) <-
    foldMap
      ( \case
          (Nothing, ct) -> (DL.singleton ct, mempty, mempty)
          (Just (pf, newWants), ct) -> (mempty, DL.singleton (pf, ct), DL.fromList newWants)
      )
      <$> mapM (bitraverse (uncurry $ solveWithDictPred info) pure) withDicts
  pure $
    if null contrs
      then TcPluginContradiction $ DL.toList contrs
      else TcPluginOk (DL.toList solved) (DL.toList wants)

solveWithDictPred :: Info -> CtLoc -> DecodedPred -> TcPluginM (Maybe (EvTerm, [Ct]))
solveWithDictPred Info {} loc DecodedPred {..} = do
  case tcInstNewTyCon_maybe constrTyCon constrArgs of
    Nothing -> pure Nothing
    Just (onlyMethodType, co) -> do
      want <- newWanted loc (mkPrimEqPred argType onlyMethodType)
      sv <- unsafeTcPluginTcM $ mkSysLocalM (fsLit "withDict_s") Many argType
      k <- unsafeTcPluginTcM $ mkSysLocalM (fsLit "withDict_k") Many (mkInvisFunTy Many openAlphaTy constraint)
      -- Given co2 : mty ~N# inst_meth_ty, construct the method of
      -- the WithDict dictionary:
      --
      --   \@(r :: RuntimeRep) @(a :: TYPE r) (sv :: mty) (k :: cls => a) ->
      --     k (sv |> (sub co ; sym co2))
      let proof =
            mkCoreLams [runtimeRep1TyVar, openAlphaTyVar, sv, k] $
              Core.Var k
                `Core.App` (Core.Var sv `Core.Cast` mkTransCo (mkSubCo (ctEvCoercion want)) (mkSymCo co))
      pure $ Just (EvExpr proof, [CNonCanonical want])

data DecodedPred = DecodedPred
  { constraint :: !PredType
  , constrTyCon :: !TyCon
  , constrArgs :: ![Type]
  , argType :: !Type
  }

decodeWithDictPred :: Info -> PredType -> Maybe DecodedPred
decodeWithDictPred Info {..} pt
  | ClassPred withDic [cls, argType] <- classifyPredType pt
  , withDic == _WithDict
  , Just (dict_tc, dict_args) <- tcSplitTyConApp_maybe cls =
      pure
        DecodedPred
          { constraint = cls
          , constrTyCon = dict_tc
          , constrArgs = dict_args
          , ..
          }
  | otherwise = Nothing

lookupInfo :: TcPluginM Info
lookupInfo = do
  theMod <-
    lookupModule
      (mkModuleName "GHC.Magic.Dict.Compat")
      (fsLit "ghc-magic-dict-compat")
  _WithDict <- tcLookupClass =<< lookupOrig theMod (mkTcOcc "WithDict")
  let _WithDictDataCon = classDataCon _WithDict
  _unsafeWithDict <- tcLookupTyCon =<< lookupOrig theMod (mkTcOcc "unsafeWithDict")
  pure Info {..}
