{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Import.NoFoundation
    ( module Import
#if MIN_VERSION_base(4, 11, 0)
#else
    , (<&>)
#endif
    ) where

import ClassyPrelude.Yesod as Import
import Control.Monad.Trans.Maybe as Import
import Settings as Import
import Settings.StaticFiles as Import
import Yesod.Auth as Import
import Yesod.Core.Types as Import (loggerSet)
import Yesod.Default.Config2 as Import
import Text.Julius           as Import

import Model as Import
import ModelCustom as Import
import Types as Import
import Pretty as Import
import Data.Functor as Import hiding (unzip)
import Generic as Import
import Database.Persist.Sql as Import (fromSqlKey, toSqlKey)


#if MIN_VERSION_base(4, 11, 0)
#else
(<&>) :: Functor f => f a -> (a -> b) -> f b
as <&> f = f <$> as
infixl 1 <&>
#endif
