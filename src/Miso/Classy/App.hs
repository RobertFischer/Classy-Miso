{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax        #-}

module Miso.Classy.App
	( MisoApp(..)
	, appToRecord
	, startClientApp
	, startIsoClientApp
	, ViewSpec(..)
	, vsRoutePath
	, specToView
	, defaultSubscriptions
	, module Miso.Classy
	) where

import           Control.Lens hiding ( view )
import           Miso         hiding ( App (..) )
import qualified Miso
import           Miso.Classy
import           Network.URI  ( URI (..), parseURI )
import           RFC.Prelude  hiding ( init )

class (Component model) => MisoApp model where
  initialAction :: model -> Action model
  notFoundView :: model -> WrappedComponent
  viewSpec :: ALens' model ViewSpec

  subscriptions :: model -> [Sub WrappedAction]
  subscriptions = const defaultSubscriptions
  {-# INLINE subscriptions #-}

instance (MisoApp model) => HasURI model where
  lensURI :: Lens' model URI
  lensURI = lens
    (\model -> model^.(cloneLens viewSpec) & vsUri)
    (\model newUri -> model & (cloneLens viewSpec) .~
      (let theRoutePath = toRoutePath newUri in
        fromMaybe
          (ViewSpec (notFoundView model, theRoutePath))
          (applyRoute model theRoutePath)
      )
    )
  {-# INLINE lensURI #-}

-- | These are the default subscriptions that we automatically connect to.
defaultSubscriptions :: [Sub WrappedAction]
defaultSubscriptions = []
{-# INLINE defaultSubscriptions #-}

applyRoute :: (MisoApp model) => model -> RoutePath -> Maybe ViewSpec
applyRoute model routePath =
    foldr foldImpl (parseToVs model) subs
  where
    parseToVs :: forall c. Component c => c -> Maybe ViewSpec
    parseToVs it = (\result -> ViewSpec (result, routePath)) <$> routeParser it routePath
    foldImpl wrapped rest = mapWrappedComponent parseToVs wrapped <|> rest
    subs = model^.(cloneLens subcomponents)
{-# INLINE applyRoute #-}

-- | Converts a 'MisoApp' into a 'Miso.App' record with reasonable defaults
--   for 'Miso.events' and 'Miso.mountPoint', and everything else derived
--   from the 'MisoApp' class.
appToRecord :: (MisoApp model) => model -> Miso.App model WrappedAction
appToRecord model = Miso.App
  { Miso.model = model
  , Miso.update = update'
  , Miso.view = fmap WrappedAction . view
  , Miso.subs = subscriptions model
  , Miso.events = Miso.defaultEvents
  , Miso.initialAction = WrappedAction $ initialAction model
  , Miso.mountPoint = Nothing
  }
{-# INLINE appToRecord #-}

-- | Starts an app with the default 'appToRecord' conversion and without
--   expecting there to be a DOM in existence.
startClientApp :: (MisoApp model, MonadIO m) => InitArgs model -> m ()
startClientApp args = liftIO $ do
  app <- init args
  Miso.startApp $ appToRecord app
{-# INLINE startClientApp #-}

-- | Starts an app with the default 'appToRecord' conversion, and assuming
--   that there is an isomorphic DOM in existence already.
startIsoClientApp :: (MisoApp model, MonadIO m) => InitArgs model -> m ()
startIsoClientApp args = liftIO $ do
  app <- init args
  Miso.startApp $ appToRecord app
{-# INLINE startIsoClientApp #-}

-- | This contains the information necessary to render a view.
newtype ViewSpec = ViewSpec (WrappedComponent, RoutePath) deriving (Eq)

-- | Get the route path which matched for the 'ViewSpec'
vsRoutePath :: ViewSpec -> RoutePath
vsRoutePath (ViewSpec (_,rp)) = rp
{-# INLINE vsRoutePath #-}

vsUri :: ViewSpec -> URI
vsUri (ViewSpec (_,(_,_,uri))) = uri
{-# INLINE vsUri #-}

-- | Renders a view based on the viewspec
specToView :: ViewSpec -> View WrappedAction
specToView (ViewSpec (wrapped,_)) = mapWrappedComponent (fmap WrappedAction . view) wrapped
{-# INLINE specToView #-}
