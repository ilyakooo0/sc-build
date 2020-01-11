module Control.AppRequestable
  ( AppRequestable (..),
  )
where

import Data.Aeson
import GitHub

class AppRequestable m where
  appRequest :: FromJSON a => GenRequest 'MtJSON rw a -> m (Either Error a)
