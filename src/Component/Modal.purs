module Component.Modal where

import Prelude

import AppTheme (themeColor, paperColor, themeFont)
import CSS (StyleM, column, display, flex, flexDirection, rem, width)
import CSS as CSS
import CSS.Background (backgroundColor)
import CSS.Box (boxShadow)
import CSS.Color (rgba, white)
import CSS.Common (center)
import CSS.Cursor (cursor, pointer)
import CSS.Display (display, zIndex, position, fixed, flex)
import CSS.Flexbox (flexDirection, row, flexStart, flexEnd, flexBasis, flexShrink, flexGrow, alignItems, justifyContent)
import CSS.Font (FontWeight(..), color, fontSize, fontWeight)
import CSS.Geometry (top, height, left, minHeight, padding, paddingLeft, paddingRight, paddingTop, width)
import CSS.Overflow (overflow, overflowAuto)
import CSS.Property (value)
import CSS.Size (Size(..), pct, px, rem, vh)
import CSS.Text (letterSpacing)
import CSS.Text.Shadow (textShadow)
import Capability.Navigate (class Navigate, navigate)
import Data.Maybe (Maybe(..))
import Data.Route (Route)
import Data.Route as Route
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Image.BookCover (bookCover)
import Type.Proxy (Proxy(..))

data Output iOutput
  = Affirmative
  | Negative
  | InnerOutput iOutput

type State iInput = { iInput :: iInput }

data Action iInput iOutput
  = Input iInput
  | Output iOutput
  | DidTapOk
  | DidTapCancel

type Slots iQuery iOutput = (inner :: H.Slot iQuery iOutput Unit)

_inner = Proxy :: Proxy "inner"

component
  :: ∀ m iQuery iInput iOutput
   . MonadAff m
  => Navigate m Route
  -- => StyleM Unit
  => H.Component iQuery iInput iOutput m
  -> H.Component iQuery iInput (Output iOutput) m
component innerComponent =
  H.mkComponent
    { initialState: \input -> { iInput: input }
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
    Output output -> H.raise $ InnerOutput output
    DidTapOk -> H.raise Affirmative
    DidTapCancel -> H.raise Negative

  handleQuery
    :: ∀ a
     . iQuery a
    -> H.HalogenM (State iInput) (Action iInput iOutput) (Slots iQuery iOutput) (Output iOutput) m
         (Maybe a)
  handleQuery = H.query _inner unit

  render :: State iInput -> H.ComponentHTML (Action iInput iOutput) (Slots iQuery iOutput) m
  render { iInput } =
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
      [ HH.slot _inner unit innerComponent iInput Output
      , buttonGroup
      ]

    buttonGroup = HH.div
      [ HP.class_ $ ClassName "btn-group" ]
      [ HH.button
          [ HP.class_ $ ClassName "btn btn-primary"
          , HE.onClick $ const DidTapOk
          ]
          [ HH.text "OK" ]
      , HH.button
          [ HP.class_ $ ClassName "btn btn-secondary"
          , HE.onClick $ const DidTapCancel
          ]
          [ HH.text "Cancel" ]
      ]