module MonadAnimate where

import Control.Monad (class Monad)
import Control.Monad.Reader.Trans (ReaderT)
import Control.Monad.State.Trans (StateT)
import Control.Monad.Trans.Class (lift)
import Data.Function (($), (<<<))
import Data.Time.Duration (class Duration)
import Data.Time.Duration as Duration
import Data.Unit (Unit)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen (HalogenM)

class
  Monad m <= MonadAnimate m where
  delay :: forall t. Duration t => t -> m Unit

instance monadAnimateReader :: MonadAnimate m => MonadAnimate (ReaderT env m) where
  delay = lift <<< delay

instance monadAnimateState :: MonadAnimate m => MonadAnimate (StateT s m) where
  delay = lift <<< delay

instance monadAnimateHalogenApp :: MonadAff m => MonadAnimate (HalogenM state action slots output m) where
  delay time = liftAff $ Aff.delay $ Duration.fromDuration time
