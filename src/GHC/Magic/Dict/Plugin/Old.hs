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
import GHC.Plugins (Plugin (..), defaultPlugin, mkModuleName, purePlugin)
import GHC.Tc.Instance.Family (tcInstNewTyCon_maybe)
import GHC.Tc.Plugin hiding (newWanted)
import GHC.Tc.Types
import GHC.Tc.Types.Constraint
import GHC.Tc.Types.Evidence
import GHC.TcPluginM.Extra
import GHC.Types.Id
import GHC.Types.Name
import GHC.Utils.Outputable

plugin :: Plugin
plugin =
  defaultPlugin
    { tcPlugin = const $ Just withDictPlugin
    , pluginRecompile = purePlugin
    }

withDictPlugin :: TcPlugin
withDictPlugin =
  tracePlugin
    "WithDictPlugin"
    TcPlugin
      { tcPluginStop = const $ pure ()
      , tcPluginSolve = const solveWithDict
      , tcPluginInit = pure ()
      }

data Info = Info
  { _WithDict :: !Class
  , _WithDictDataCon :: !DataCon
  }

solveWithDict :: TcPluginSolver
solveWithDict _ _ [] = pure $ TcPluginOk [] []
solveWithDict gs _ wanteds = do
  let subs = map fst $ mkSubst' gs
  info <- lookupInfo
  let withDicts =
        mapMaybe
          ( liftA2 (,)
              <$> (liftA2 (,) <$> pure . ctLoc <*> decodeWithDictPred info . ctPred . substCt subs)
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
  tcPluginTrace
    "solveWithDict/contradictions"
    (ppr $ DL.toList contrs)
  tcPluginTrace "solveWithDict/solveds" $ ppr $ DL.toList solved
  tcPluginTrace "solveWithDict/newWanteds" $ ppr $ DL.toList wants
  pure $
    if null contrs
      then TcPluginOk (DL.toList solved) (DL.toList wants)
      else TcPluginContradiction $ DL.toList contrs

mkNonCanonical' ::
  CtLoc -> CtEvidence -> Ct
mkNonCanonical' origCtl ev =
  let ct_ls = ctLocSpan origCtl
      ctl = ctEvLoc ev
      wanted = mkNonCanonical ev
   in setCtLoc wanted (setCtLocSpan ctl ct_ls)

solveWithDictPred :: Info -> CtLoc -> DecodedPred -> TcPluginM (Maybe (EvTerm, [Ct]))
solveWithDictPred Info {} loc DecodedPred {..} = do
  tcPluginTrace "solveWithDictPred" (ppr (constraint, argType))
  case tcInstNewTyCon_maybe constrTyCon constrArgs of
    Nothing -> do
      tcPluginTrace "solveWithDictPred: Failed!" (ppr (constraint, argType))
      pure Nothing
    Just (onlyMethodType, co) -> do
      tcPluginTrace "solveWithDictPred: singleton class found" (ppr (constraint, argType, onlyMethodType, co))
      let nomEq = mkPrimEqPred argType onlyMethodType
      hole <- newCoercionHole nomEq
      let want = CtWanted nomEq (HoleDest hole) WDeriv loc
      sv <- unsafeTcPluginTcM $ mkSysLocalM (fsLit "withDict_s") Many argType
      k <- unsafeTcPluginTcM $ mkSysLocalM (fsLit "withDict_k") Many (mkInvisFunTy Many constraint openAlphaTy)
      -- Given co2 : mty ~N# inst_meth_ty, construct the method of
      -- the WithDict dictionary:
      --
      --   \@(r :: RuntimeRep) @(a :: TYPE r) (sv :: mty) (k :: cls => a) ->
      --     k (sv |> (sub co ; sym co2))
      let proof =
            mkCoreLams [runtimeRep1TyVar, openAlphaTyVar, sv, k] $
              Core.Var k
                `Core.App` (Core.Var sv `Core.Cast` mkTransCo (mkSubCo (ctEvCoercion want)) (mkSymCo co))
      pure $ Just (EvExpr proof, [mkNonCanonical' loc want])

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
  pure Info {..}
