{-# LANGUAGE UnicodeSyntax #-}

module Miso.Classy
	( Component(..)
	, update'
	, WrappedComponent(..)
	, WrappedAction(..)
	, RouteParser
	, RouteError
	, ClassyEffect
	, WrappedEffect
	, ClassyTransition
	, ClassySub
	, ClassySink
	, WrappedSub
	, WrappedSink
	, wrapAction
	, wrapSub
	) where

import Control.Lens
import Data.Typeable   ( Typeable, cast )
import Data.Void       ( Void )
import Miso            hiding ( App (..) )
import RFC.Prelude
import Text.Megaparsec

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
	routeParser _ = fail "Model is not a route"
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

-- | The type of a 'Parsec' parser for processing routes. It will consume a segment
--   of the URL hash value, and (if it successfully parsed), it returns the action
--   that should fire.
type RouteParser = Parsec Void StrictText WrappedAction
type RouteError = ParseError Char Void -- ^ The error type for routes.

-- | A wrapper around components that hides the existential quantification.
data WrappedComponent = forall child. Component child ⇒ WrappedComponent child

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
