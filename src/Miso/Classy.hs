{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UnicodeSyntax  #-}

module Miso.Classy
	( Component(..)
	, update'
	, WrappedComponent(..)
	, WrappedAction(..)
	, RouteParser
	, ClassyEffect
	, WrappedEffect
	, ClassyTransition
	, ClassySub
	, ClassySink
	, WrappedSub
	, WrappedSink
	, wrapAction
	, wrapSub
	, ViewSpec
	, routeToURI
	, toRoutePath
	, getCurrentURI
	, parseCurrentURI
	, vsRoutePath
	, specToView
	, module Miso
	, module Network.URI
	, module RFC.Miso.String
	) where

import           Control.Lens    hiding ( view )
import           Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import           Data.Typeable   ( Typeable, cast )
import           Miso            hiding ( App (..), getCurrentURI )
import           Network.URI     ( URI (..), parseURI )
import qualified Network.URL     as URL
import           RFC.Miso.String
import           RFC.Prelude

-- | An 'Effect' where we wrap up the action type as a 'WrappedAction'.
type WrappedEffect model = Effect WrappedAction model

-- | A 'Sink' where we wrap up the action type as a 'WrappedAction'.
type WrappedSink = WrappedAction -> IO ()

-- | A 'Sub' where we wrap up the action type as a 'WrappedSub'.
type WrappedSub = WrappedSink -> IO ()

-- | Handy alias specifying the 'Effect' in terms of type families.
type ClassyEffect model = Effect (Action model) model

-- | Handy alias specifying the 'Transition' in terms of type families.
type ClassyTransition model = Transition (Action model) model

-- | Handy alias specifying the 'Sink' in terms of type families.
type ClassySink model = Action model -> IO ()

-- | Handy alias specifying the 'Sub' in terms of type families.
type ClassySub model = ClassySink model -> IO ()

-- | Wraps the concept of th e
class (Eq model, Typeable model, Typeable (Action model)) => Component model where
	{-# MINIMAL init, view, subcomponents, (update|transition) #-}

	-- | The actions for this component, such as those returned in an 'Effect'
	type Action model = action | action -> model

	-- | Defines the initial arguments that the component expects to receive.
	type InitArgs model = initargs | initargs -> model

	init :: InitArgs model → IO model     -- ^ Initialize the model
	view :: model -> View (Action model)  -- ^ Render the model into a VDOM-friendly structure

	subcomponents :: ALens' model [WrappedComponent] -- ^ Holds onto all the subcomponents

	routeParser :: model -> RouteParser -- ^ Provides a parser that defines the route for this model
	routeParser _ _ = fail "Model is not a route"
	{-# INLINE routeParser #-}

	update :: Action model -> model -> ClassyEffect model
	update = fromTransition . transition
	{-# INLINE update #-}

	transition :: Action model -> ClassyTransition model ()
	transition = toTransition . update
	{-# INLINE transition #-}

-- | For the component itself, and then recursively for all the subcomponents, this attempts to cast
--   the action (unwrapped from 'WrappedAction') to the relevant type for that component.
--   If that works, we calls 'update' on that component.
update' :: (Component model) =>  WrappedAction -> model -> WrappedEffect model
update' wrapped@(WrappedAction action) initModel =
		Effect finalModel allEffects
	where
		(finalModel, allEffects) = (foldedModel, foldedEffects)
		Effect foldedModel foldedEffects =
			foldr foldImpl initEffect (initModel^.(cloneLens subcomponents))
		foldImpl (WrappedComponent subcomp) (Effect roundModel roundEffects) =
			let result = update' wrapped subcomp in
			let Effect wrappedModel wrappedEffects = result &
				bimap
					id
					(\subModel -> roundModel & (cloneLens subcomponents) %~ ((:) (WrappedComponent subModel)))
			in
			Effect wrappedModel (wrappedEffects <> roundEffects)
		initEffect =
			case cast action of
				Nothing -> noEff initModel
				Just myAction -> bimap wrapAction id $ update myAction initModel
{-# INLINABLE update' #-}

-- | The function for processing routes from route paths. It will consume the current
--   'RoutePath', and (if it successfully parsed), it returns the action that should
--   fire.
type RouteParser = forall m. MonadFail m => RoutePath -> m WrappedAction

-- | A wrapper around components that hides the existential quantification.
data WrappedComponent = forall child. Component child ⇒ WrappedComponent child
instance Eq WrappedComponent where
  (==) (WrappedComponent left) (WrappedComponent right) =
    maybe False (left ==) (cast right)

-- | A wrapper around actions, such as those returned in an 'Effect', which
--   hides the existential quantification.
data WrappedAction = forall child. Component child => WrappedAction (Action child)

-- | Utility function for wrapping up an action into a 'WrappedAction'
wrapAction :: Component model => Action model -> WrappedAction
wrapAction = WrappedAction
{-# INLINE wrapAction #-}

-- | Utility function for wrapping up a 'Sub' into a 'WrappedSub'
wrapSub :: Component model => Sub (Action model) -> WrappedSub
wrapSub sub sink = sub (sink . wrapAction)
{-# INLINE wrapSub #-}


-- | A single segment of a route (a piece between slashes in the URI fragment)
type RouteSegment = StrictText

-- | The value of the query section of a route
type RouteQuery = Map StrictText [StrictText]

-- | The full path of the route. The third URL is the "base URI",
--   and does not necessarily contain the 'RouteSegment' and the
--   'RouteQuery' pieces.
type RoutePath = ([RouteSegment], RouteQuery, URI)

-- | Generates a URI from the route path by merging the 'RouteQuery' into the 'uriQuery'
--   and the '[RouteSegment]' into the 'uriFragment'.
routeToURI :: RoutePath -> URI
routeToURI (segs, qry, uri) =
  uri
    { uriQuery = '?':query
    , uriFragment = '#':frag
    }
  where
    encodeString :: ConvertibleStrings a String => Bool -> a -> String -- Second argument is: "Is this a query string?"
    encodeString spaceToPlus = URL.encString spaceToPlus URL.ok_param . cs
    query :: String
    query = intercalate "&" . pairsToTerms $ Map.toList qry
    pairsToTerms [] = []
    pairsToTerms ((key,[]):rest)       = (encodeString True key) : pairsToTerms rest
    pairsToTerms ((key,([val])):rest) = (encodeString True key <> "=" <> (encodeString True val)) : pairsToTerms rest
    pairsToTerms ((key,vals):rest)     = (encodeString True key <> "=" <> intercalate "," (encodeString True <$> vals)) : pairsToTerms rest
    frag :: String
    frag = intercalate "/" (segToPath <$> segs)
    segToPath :: RouteSegment -> String
    segToPath = encodeString False
{-# INLINABLE routeToURI #-}


-- | Parses a 'URI' into a 'RoutePath'
toRoutePath :: URI -> RoutePath
toRoutePath uri@URI{uriFragment,uriQuery} =
      ( pathToSegments . parseHash $ uriFragment
      , parseQuery uriQuery
      , uri
      )
  where
    decodeString :: String -> StrictText
    decodeString str = cs . fromMaybe str . URL.decString True $ str
    pathToSegments :: String -> [RouteSegment]
    pathToSegments "" = []
    pathToSegments ('/':rest) = pathToSegments rest
    pathToSegments path = (decodeString start) : pathToSegments rest
      where
        (start,rest) = splitOnSlash path
        splitOnSlash "" = ("","")
        splitOnSlash ('/':content) = splitOnSlash content
        splitOnSlash content =
          case span ('/' /=) content of
            ([],[])               -> ("","")
            ([], _:theRest)       -> splitOnSlash theRest
            (theStart, [])        -> (theStart, "")
            (theStart, _:theRest) -> (theStart, theRest)
    parseHash ('#':rest) = parseHash rest
    parseHash ('!':rest) = parseHash rest
    parseHash ('/':rest) = parseHash rest
    parseHash hash       = hash
    parseQuery ('?':rest) = parseQuery rest
    parseQuery query =
      case URL.importParams query of
        Nothing ->
          Map.empty
        Just pairs ->
          fmap (second listify) pairs &
          Map.fromListWith (++) &
          Map.map sort &
          Map.map (fmap cs) &
          Map.mapKeys cs
    listify :: String -> [String]
    listify ""        = []
    listify (',':val) = listify val
    listify val       = start : listify rest
      where
        (start,rest) = splitOnComma val
        splitOnComma ""            = ("","")
        splitOnComma (',':content) = splitOnComma content
        splitOnComma (' ':content) = splitOnComma content
        splitOnComma content =
          case span (',' /=) content of
            ([], [])              -> ("", "")
            ([], _:theRest)       -> splitOnComma theRest
            (theStart, [])        -> (theStart, "")
            (theStart, _:theRest) -> (theStart, theRest)
{-# INLINABLE toRoutePath #-}

-- | Retrieves the current location of the window from "window.location.href".
foreign import javascript safe "$r = (window && window.location && window.location.href) || '';"
  getWindowLocationHref :: IO MisoString

-- | Gets the current URI.
getCurrentURI :: (MonadIO m, MonadFail m) => m URI
getCurrentURI = do
  href <- fromMisoString <$> liftIO getWindowLocationHref
  case parseURI href of
    Nothing  -> fail $ "Could not parse URI from window.location: " <> href
    Just uri -> return uri
{-# INLINE getCurrentURI #-}

-- | Utility method to parse the current URI.
parseCurrentURI :: (MonadIO m) => m RoutePath
parseCurrentURI = toRoutePath <$> liftIO getCurrentURI
{-# INLINE parseCurrentURI #-}

-- | This contains the information necessary to render a view.
newtype ViewSpec = ViewSpec (WrappedComponent, RoutePath) deriving (Eq)

-- | Get the route path which matched for the 'ViewSpec'
vsRoutePath :: ViewSpec -> RoutePath
vsRoutePath (ViewSpec (_,rp)) = rp
{-# INLINE vsRoutePath #-}

-- | Renders a view based on the viewspec
specToView :: ViewSpec -> View WrappedAction
specToView (ViewSpec (WrappedComponent viewModel,_)) = WrappedAction <$> view viewModel
{-# INLINE specToView #-}
