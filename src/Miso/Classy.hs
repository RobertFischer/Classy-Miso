{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilyDependencies    #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UnicodeSyntax             #-}
#if !MIN_VERSION_base(4,10,0)
{-# OPTIONS_GHC -fno-warn-deprecations #-}
#endif

module Miso.Classy
	( Component(..)
	, update'
	, WrappedComponent
	, wrapComponent
	, mapWrappedComponent
	, addSubcomponent
	, WrappedAction(..)
	, RouteParser
	, ClassyEffect
	, WrappedEffect
	, ClassyTransition
	, effectToTransition
	, ClassySub
	, ClassySink
	, WrappedView
	, WrappedSub
	, WrappedSink
	, wrapAction
	, wrapSub
	, routeToURI
	, toRoutePath
	, getCurrentURI
	, parseCurrentURI
	, RoutePath
	, actionToSub
	, actionToWrappedSub
	, viewSub
	, viewSubs
	, viewSubBy
	, viewSubsBy
	, emptyView
	, fireEvent
	, addEventHandler
	, module Miso
	, module Network.URI
	, module RFC.Miso.String
	, Default(..)
	) where

import           Control.Lens                      hiding ( view )
import           Control.Monad.Trans.State.Strict  ( put )
import           Control.Monad.Trans.Writer.Strict ( tell )
import           Data.Default
import           Data.Map.Strict                   ( Map )
import qualified Data.Map.Strict                   as Map
import           Data.Typeable
import           Miso                              hiding ( App (..), getCurrentURI )
import qualified Miso.Html                         as Html
import           Network.URI                       ( URI (..), parseURI )
import qualified Network.URL                       as URL
import           RFC.Miso.String
import           RFC.Prelude                       hiding ( init )

-- Need the ability to compare proxies for equivalence
#if MIN_VERSION_base(4,10,0)
import Type.Reflection ( eqTypeRep )

eqProxy :: Proxy a -> Proxy b -> Maybe (a :~: b)
eqProxy a b = eqTypeRep (typeRep a) (typeRep b)
#else
-- | Hand-rolled and dangerous implementation of 'eqTypeRep'. BOO!
eqProxy :: (Typeable a, Typeable b) => Proxy a -> Proxy b -> Maybe (a :~: b)
eqProxy a b
	| typeRep a == typeRep b     = Just undefined
	| otherwise                 = Nothing
#endif

{-# ANN module ("HLint: ignore Use fewer imports"::String) #-}

-- | A 'View' where we wrap up the action type as a 'WrappedAction'.
type WrappedView = View WrappedAction

-- | An 'Effect' where we wrap up the action type as a 'WrappedAction'.
type WrappedEffect model = Effect WrappedAction model

-- | A 'Sink' where we consume the action type as a 'WrappedAction'.
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
class (Eq model, Typeable model, Typeable (Action model), Default(EventHandler model)) => Component model where
	{-# MINIMAL init, view, eventHandlers, subcomponents, (update|transition) #-}

	-- | The actions for this component, such as those returned in an 'Effect'
	type Action model = action | action -> model

	-- | Defines the initial arguments that the component expects to receive.
	type InitArgs model = initargs | initargs -> model

	-- | Defines the kinds of events that the parent can handle.
	type EventHandler model

	init :: InitArgs model → IO model			-- ^ Initialize the model
	view :: model -> View (Action model)	-- ^ Render the model into a VDOM-friendly structure

	eventHandlers :: ALens' model [EventHandler model] -- ^ Holds onto all the event handlers.

	subcomponents :: ALens' model [WrappedComponent] -- ^ Holds onto all the subcomponents
	subcomponents = lens (const []) (const)
	{-# INLINE subcomponents #-}

	routeParser :: model -> RouteParser -- ^ Provides a parser that defines the route for this model
	routeParser _ _ = fail "Model is not a route"
	{-# INLINE routeParser #-}

	-- | Implement this if you want to use the classic way to update.
	update :: Action model -> model -> WrappedEffect model
	update = fromTransition . transition
	{-# INLINE update #-}

	-- | Implement this if you want to use the 'Transition'-based way to update.
	transition :: Action model -> Transition WrappedAction model ()
	transition = toTransition . update
	{-# INLINE transition #-}

	-- | Whether or not this action is really for this model. By default, we accept all actions. But, if you've
	--   got multiple components of the same type, you may need to be more picky.
	acceptAction :: Action model -> model -> Bool
	acceptAction _ _ = True
	{-# INLINE acceptAction #-}

-- | Converts an 'Effect' into a 'Transition'.
effectToTransition :: Effect action model -> Transition action model ()
effectToTransition (Effect model actions) = do
	lift $ tell actions
	put model

fireEvent :: (Component model) => model -> (EventHandler model -> WrappedSub) -> WrappedEffect model
fireEvent model event = Effect model $ event <$> events
	where
		events = model^.(cloneLens eventHandlers)
{-# INLINE fireEvent #-}

-- | Utility method to add an 'EventHandler' instance to a 'Component'.
addEventHandler :: (Component model) => model -> EventHandler model -> model
addEventHandler model handler = model & (cloneLens eventHandlers) %~ ((:) handler)
{-# INLINE addEventHandler #-}

-- | Handy method for the common case when you can pass your args down to your subcomponents.
addSubcomponent :: (Component model, Component sub) =>
	InitArgs model -> model -> (InitArgs model -> IO (InitArgs sub)) -> EventHandler sub -> IO model
addSubcomponent parentArgs parent argPicker handler = addSub . addHandler <$> (subArgs >>= init)
	where
		addHandler sub = addEventHandler sub handler
		subArgs = argPicker parentArgs
		initSubs = parent^.(cloneLens subcomponents)
		addSub newSub = parent & (cloneLens subcomponents) .~ (wrapComponent newSub:initSubs)
{-# INLINE addSubcomponent #-}

-- | For the component itself, and then recursively for all the subcomponents, this attempts to cast
--	 the action (unwrapped from 'WrappedAction') to the relevant type for that component.
--	 If that works, we calls 'update' on that component.
update' :: (Component model) =>  WrappedAction -> model -> WrappedEffect model
update' wrapped@(WrappedAction action) initModel =
		Effect finalModel allEffects
	where
		(finalModel, allEffects) = (foldedModel, foldedEffects)
		Effect foldedModel foldedEffects =
			foldr foldImpl initEffect (initModel^.(cloneLens subcomponents))
		foldImpl (WrappedComponent (_,subcomp)) (Effect roundModel roundEffects) =
			let result = update' wrapped subcomp in -- TODO Do this recursion as a scheduled IO
			let Effect wrappedModel wrappedEffects = result &
				bimap
					id
					(\subModel -> roundModel & (cloneLens subcomponents) %~ ((:) (WrappedComponent (Proxy,subModel))))
			in
			Effect wrappedModel (wrappedEffects <> roundEffects)
		initEffect =
			case cast action of
				Nothing -> noEff initModel
				Just myAction ->
					if acceptAction myAction initModel then
						update myAction initModel
					else
						noEff initModel
{-# INLINABLE update' #-}

-- | The function for processing routes from route paths. It will consume the current
--	 'RoutePath', and (if it successfully parsed), it returns the component that handles the route.
type RouteParser = forall m. MonadFail m => RoutePath -> m WrappedComponent

-- | A wrapper around components that hides the existential quantification.
data WrappedComponent = forall child. Component child ⇒ WrappedComponent (Proxy child, child)
instance Eq WrappedComponent where
	(==) (WrappedComponent left) (WrappedComponent right) =
		maybe False (left ==) (cast right)
	{-# INLINE (==) #-}

-- | Smart constructor for 'WrappedComponent'.
wrapComponent :: Component child => child -> WrappedComponent
wrapComponent child = WrappedComponent (Proxy, child)
{-# INLINE wrapComponent #-}

-- | Allows you to effectively extract the component, but note that the type of the
--   wrapped component can't escape its scope.
mapWrappedComponent :: (forall model. Component model => model -> a) -> WrappedComponent -> a
mapWrappedComponent f (WrappedComponent(_,it)) = f it
{-# INLINE mapWrappedComponent #-}

-- | A wrapper around actions, such as those returned in an 'Effect', which
--	 hides the existential quantification.
data WrappedAction = forall child. Component child => WrappedAction (Action child)

-- | Utility function for wrapping up an action into a 'WrappedAction'
wrapAction :: Component model => Action model -> WrappedAction
wrapAction = WrappedAction
{-# INLINE wrapAction #-}

-- | Utility method for converting an action into a 'Sub'
actionToSub :: Action model -> Sub (Action model)
actionToSub action sink = sink action
{-# INLINE actionToSub #-}

-- | Utility function for wrapping up a 'Sub' into a 'WrappedSub'
wrapSub :: Component model => Sub (Action model) -> WrappedSub
wrapSub sub sink = sub (sink . wrapAction)
{-# INLINE wrapSub #-}

-- | Ties together 'actionToSub' and 'wrapSub' for you,
--   so you can go all the way from an action to a 'WrappedSub'.
actionToWrappedSub :: Component model => Action model -> WrappedSub
actionToWrappedSub = wrapSub . actionToSub
{-# INLINE actionToWrappedSub #-}

-- | A single segment of a route (a piece between slashes in the URI fragment)
type RouteSegment = StrictText

-- | The value of the query section of a route
type RouteQuery = Map StrictText [StrictText]

-- | The full path of the route. The third URL is the "base URI",
--	 and does not necessarily contain the 'RouteSegment' and the
--	 'RouteQuery' pieces.
type RoutePath = ([RouteSegment], RouteQuery, URI)

-- | Generates a URI from the route path by merging the 'RouteQuery' into the 'uriQuery'
--	 and the '[RouteSegment]' into the 'uriFragment'.
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
		pairsToTerms ((key,[]):rest)			 = (encodeString True key) : pairsToTerms rest
		pairsToTerms ((key,([val])):rest) = (encodeString True key <> "=" <> (encodeString True val)) : pairsToTerms rest
		pairsToTerms ((key,vals):rest)		 = (encodeString True key <> "=" <> intercalate "," (encodeString True <$> vals)) : pairsToTerms rest
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
						([],[])								-> ("","")
						([], _:theRest)				-> splitOnSlash theRest
						(theStart, [])				-> (theStart, "")
						(theStart, _:theRest) -> (theStart, theRest)
		parseHash ('#':rest) = parseHash rest
		parseHash ('!':rest) = parseHash rest
		parseHash ('/':rest) = parseHash rest
		parseHash hash			 = hash
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
		listify ""				= []
		listify (',':val) = listify val
		listify val				= start : listify rest
			where
				(start,rest) = splitOnComma val
				splitOnComma ""						 = ("","")
				splitOnComma (',':content) = splitOnComma content
				splitOnComma (' ':content) = splitOnComma content
				splitOnComma content =
					case span (',' /=) content of
						([], [])							-> ("", "")
						([], _:theRest)				-> splitOnComma theRest
						(theStart, [])				-> (theStart, "")
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
		Nothing  -> fail $ "Co uld not parse URI from window.location: " <> href
		Just uri -> return uri
{-# INLINE getCurrentURI #-}

-- | Utility method to parse the current URI.
parseCurrentURI :: (MonadIO m) => m RoutePath
parseCurrentURI = toRoutePath <$> liftIO getCurrentURI
{-# INLINE parseCurrentURI #-}

-- | View a particular subcomponent by specifying a test. For the first (leftmost) subcomponent
--   that returns 'True', we will return their view. Failing that, we return an
--   empty (but defined) view.
viewSubBy :: (Component model) => model -> (WrappedComponent -> Bool) -> WrappedView
viewSubBy parent subTest = fromMaybe emptyView . safeHead $ viewSubsBy parent subTest
{-# INLINE viewSubBy #-}

-- | View a particular subcomponent by specifying its type. For the first (leftmost) subcomponent
--   whose type is equal to the given type, we will return their view. Failing that, we
--   return an empty (but defined) view.
viewSub :: (Component model, Component sub) => model -> Proxy sub -> WrappedView
viewSub parent subType = viewSubBy parent (wrappedComponentPxyEq subType)
{-# INLINE viewSub #-}

-- | View subcomponents by a given test. For every subcomponent that returns 'True', we will
--   include their 'WrappedView' in the resulting list.
viewSubsBy :: (Component model) => model -> (WrappedComponent -> Bool) -> [WrappedView]
viewSubsBy parent test =
		toWrappedView <$> filter test targets
	where
		targets = (wrapComponent parent : parent^.(cloneLens subcomponents))
		toWrappedView :: WrappedComponent -> WrappedView
		toWrappedView (WrappedComponent(_,sub)) = WrappedAction <$> view sub
{-# INLINE viewSubsBy #-}

-- | View subcomponents by specifying their type. For every subcomponent of whose type
--   is equal to the given type, we will include their 'WrappedView' in the resulting list.
viewSubs :: (Component model, Component sub) => model -> Proxy sub -> [WrappedView]
viewSubs parent subType = viewSubsBy parent (wrappedComponentPxyEq subType)
{-# INLINE viewSubs #-}

-- | Test that a 'WrappedComponent' is equal to a given type.
wrappedComponentPxyEq :: (Component target) => Proxy target -> WrappedComponent -> Bool
wrappedComponentPxyEq targetPxy (WrappedComponent(pxy,_)) = isJust $ eqProxy targetPxy pxy
{-# INLINE wrappedComponentPxyEq #-}

-- | A definition of an empty view, which has no impact on how the screen is rendered.
emptyView :: View anything
emptyView = Html.text ""
{-# INLINE emptyView #-}

