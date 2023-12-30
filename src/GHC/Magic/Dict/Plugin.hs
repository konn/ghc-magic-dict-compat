{-# LANGUAGE CPP #-}

{- | A type-checker plugin to resolve 'WithDict' constraints in older versions of GHC.
This becomes no-op for newer versions of GHC.
-}
module GHC.Magic.Dict.Plugin (plugin) where

import GHC.Plugins (Plugin)
#if MIN_VERSION_ghc(9,4,0)
import GHC.Plugins (defaultPlugin, purePlugin)

plugin :: Plugin
plugin = defaultPlugin { pluginRecompile = purePlugin }
#else
import qualified GHC.Magic.Dict.Plugin.Old as Old

plugin :: Plugin
plugin = Old.plugin
#endif
