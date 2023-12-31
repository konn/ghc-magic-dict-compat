{-# LANGUAGE CPP #-}

{- |
A type-checker plugin to resolve 'WithDict' constraints for GHC \<9.4; it is just a no-op for GHC >=9.4.

This plugin is inteded to be used with the compatibility layer exposed from "GHC.Magic.Dict.Compat".
For GHC \<9.4, the definitions of 'GHC.Magic.Dict.Compat.WithDict' and 'GHC.Magic.Dict.Compat.withDict' are slightly different from those of GHC \>= 9.4 to prevent user-defined instances.

Example usage:

@
{\-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeApplications, ConstraintKinds #-\}
{\-# GHC_OPTIONS -fplugin GHC.Magic.Dict.Plugin #-\}
module MyModule where
import "GHC.Magic.Dict.Compat"

class Given a where
  given :: a

give :: a -> (Given a => r) -> r
give = 'GHC.Magic.Dict.Compat.withDict' \@(Given a) \@a
@

For GHC \>=9.4, this module just re-exports the module "GHC.Magic.Dict" and the plugin is just a no-op - so you can safely use this package without concerning break anything in newer GHCs.
-}
module GHC.Magic.Dict.Plugin (plugin) where

import GHC.Plugins (Plugin)
#if MIN_VERSION_ghc(9,4,0)
import GHC.Plugins (defaultPlugin, pluginRecompile, purePlugin)

plugin :: Plugin
plugin = defaultPlugin { pluginRecompile = purePlugin }
#else
import qualified GHC.Magic.Dict.Plugin.Old as Old

plugin :: Plugin
plugin = Old.plugin
#endif
