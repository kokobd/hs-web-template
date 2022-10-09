module Demo.Common.API.Prelude
  ( Text,
    Generic,
    module Servant.API,
    module Data.Aeson,
    aesonOptions
  )
where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.API

aesonOptions :: Options
aesonOptions =
  defaultOptions
    { fieldLabelModifier = dropWhile (== '_') . dropWhile (/= '_') . dropWhile (== '_')
    }
