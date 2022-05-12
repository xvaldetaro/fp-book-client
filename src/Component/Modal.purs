module Component.Modal where

import Prelude

import CSS (column)
import CSS.Background (backgroundColor)
import CSS.Color (rgba)
import CSS.Common (center)
import CSS.Display (display, zIndex, position, fixed, flex)
import CSS.Flexbox (alignItems, flexDirection, justifyContent)
import CSS.Geometry (height, left, top, width)
import CSS.Overflow (overflow, overflowAuto)
import CSS.Property (value)
import CSS.Size (Size(..), pct, rem)
import Capability.Navigate (class Navigate)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

data InnerOutputInternal iOutput
  = PassThrough iOutput
  | SetModalConfig Config
  | CloseAffirmative
  | CloseNegative

data Output iOutput
  = Affirmative
  | Negative
  | InnerOutput iOutput

data ButtonDisplay
  = DisplayBothButtons
  | DisplayNoButtons
  | DisplayAffirmative
  | DisplayNegative

type Config
  = { affirmativeLabel :: String
  , negativeLabel :: String
  , buttonDisplay :: ButtonDisplay
  , isAffirmativeDisabled :: Boolean
  , isNegativeDisabled :: Boolean
  }

defaultConfig :: Config
defaultConfig =
  { affirmativeLabel: "OK"
  , negativeLabel: "Cancel"
  , buttonDisplay: DisplayBothButtons
  , isAffirmativeDisabled: false
  , isNegativeDisabled: false
  }

type State iInput = { iInput :: iInput, config :: Config }

data Action iInput iOutput
  = Input iInput
  | ReceivedInnerOutput (InnerOutputInternal iOutput)
  | DidTapOk
  | DidTapCancel

type Slots iQuery iOutput = (inner :: H.Slot iQuery (InnerOutputInternal iOutput) Unit)

_inner = Proxy :: Proxy "inner"

component
  :: ∀ m route iQuery iInput iOutput
   . MonadAff m
  => Navigate m route
  -- => StyleM Unit
  => H.Component iQuery iInput (InnerOutputInternal iOutput) m
  -> H.Component iQuery iInput (Output iOutput) m
component innerComponent =
  H.mkComponent
    { initialState: \input -> { iInput: input, config: defaultConfig }
    , render
    , eval:
        H.mkEval
          H.defaultEval
            { handleQuery = handleQuery
            , handleAction = handleAction
            , receive = Just <<< Input
            }
    }
  where
  handleAction
    :: Action iInput iOutput
    -> H.HalogenM (State iInput) (Action iInput iOutput) (Slots iQuery iOutput) (Output iOutput) m
         Unit
  handleAction = case _ of
    Input input -> H.modify_ _ { iInput = input }
    ReceivedInnerOutput outputWrapper -> case outputWrapper of
      PassThrough output -> H.raise $ InnerOutput output
      SetModalConfig config -> H.modify_ _ { config = config }
      CloseAffirmative -> H.raise $ Affirmative
      CloseNegative -> H.raise $ Negative
    DidTapOk -> H.raise Affirmative
    DidTapCancel -> H.raise Negative

  handleQuery
    :: ∀ a
     . iQuery a
    -> H.HalogenM (State iInput) (Action iInput iOutput) (Slots iQuery iOutput) (Output iOutput) m
         (Maybe a)
  handleQuery = H.query _inner unit

  render :: State iInput -> H.ComponentHTML (Action iInput iOutput) (Slots iQuery iOutput) m
  render { iInput, config } =
    fullPageWithCenteredInnerComponent modalCenteredDialog

    where
    fullPageWithCenteredInnerComponent centeredHtml =
      HH.div
        [ HC.style do
            display flex
            alignItems center
            justifyContent center
            position fixed
            top $ Size $ value 0.0
            left $ Size $ value 0.0
            width (pct 100.0)
            height (pct 100.0)
            overflow overflowAuto
            backgroundColor (rgba 0 0 0 0.4)
            zIndex 1
        ]
        [ centeredHtml ]

    modalCenteredDialog = HH.div
      [ HP.class_ $ ClassName "card"
      , HC.style $ width (rem 28.0)
      ]
      [ HH.div
          [ HP.class_ $ ClassName "card-body" ]
          [ cardBody ]
      ]
    cardBody = HH.div
      [ HC.style $ display flex *> flexDirection column ]
      [ HH.slot _inner unit innerComponent iInput ReceivedInnerOutput
      , buttonGroup
      ]

    buttonGroup =
      let (Tuple showOk showCancel) = case config.buttonDisplay of
            DisplayBothButtons -> Tuple true true
            DisplayNoButtons -> Tuple false false
            DisplayAffirmative -> Tuple true false
            DisplayNegative -> Tuple false true
      in
      HH.div
        [ HP.class_ $ ClassName "btn-group" ]
        [ maybeHtml showOk $ HH.button
            [ HP.class_ $ ClassName "btn btn-primary"
            , HE.onClick $ const DidTapOk
            , HP.disabled $ config.isAffirmativeDisabled
            ]
            [ HH.text config.affirmativeLabel ]
        , maybeHtml showCancel $ HH.button
            [ HP.class_ $ ClassName "btn btn-secondary"
            , HE.onClick $ const DidTapCancel
            , HP.disabled $ config.isAffirmativeDisabled
            ]
            [ HH.text config.negativeLabel ]
        ]
    maybeHtml pred html = if pred then html else HH.text ""