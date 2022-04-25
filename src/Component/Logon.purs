module Component.Logon where

import Prelude

import AppTheme (paperColor, themeColor, themeFont)
import CSS (CSS, borderRadius, column, paddingBottom, pct, px, rem, vw)
import CSS as CSS
import CSS.Background (backgroundColor)
import CSS.Box (boxShadow)
import CSS.Color (Color, rgba, rgb, white)
import CSS.Common (center)
import CSS.Cursor (cursor, pointer)
import CSS.Display (display, zIndex, position, fixed, flex)
import CSS.Flexbox (flexDirection, row, flexStart, flexEnd, flexBasis, flexShrink, flexGrow, alignItems, justifyContent)
import CSS.Font (FontWeight(..), sansSerif, fontFamily, color, fontSize, fontWeight)
import CSS.Geometry (padding, paddingTop, paddingLeft, paddingRight, width, height, minHeight)
import CSS.Missing (spaceEvenly)
import CSS.Property (value)
import Control.Alt ((<|>))
import Control.Monad.Rec.Class (forever)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, Fiber, Milliseconds(..), delay, error, forkAff, killFiber, launchAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import Halogen.Query.Event as HQE
import Halogen.Query.HalogenM (SubscriptionId(..))
import Halogen.Query.HalogenM as HQ
import Halogen.Subscription as HS
import Image.BookCover (bookCover)
import Undefined (undefined)
import Web.HTML (HTMLDocument, window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as ME
import Web.UIEvent.MouseEvent.EventTypes (click)

type Input = Unit
type Output = Void

type Slots :: ∀ k. Row k
type Slots = ()

type State = {}

type Query :: ∀ k. k -> Type
type Query = Const Void

data Action = Logon

component :: ∀ m . MonadAff m => H.Component Query Input Output m
component = H.mkComponent
  { initialState: \_ -> {}
  , render
  , eval: H.mkEval H.defaultEval
  }
  where
    render :: State -> H.ComponentHTML Action Slots m
    render {} =
      HH.div [
          HC.style do
            display flex
            flexDirection column
            width $ vw 22.0
            height $ vw 25.0
            paddingTop $ vw 1.25
            paddingBottom $ vw 1.25
            paddingLeft $ vw 1.50
            paddingRight $ vw 1.50
            backgroundColor paperColor
            borderRadius (px 7.0) (px 7.0) (px 7.0) (px 7.0)
            boxShadow (px 10.0) (px 10.0) (px 20.0) (rgba 0 0 24 0.75)
      ]
      [ HH.div [
          HC.style do
            paddingRight $ vw 1.0
            display flex
            flexDirection column
            alignItems center
            justifyContent center
            flexGrow 10.0
        ]
        [ HH.img [
            HC.style do
              width $ pct 40.0
          , HP.src bookCover
          ]
        ]
      , HH.div [
          HC.style do
            display flex
            flexDirection column
            justifyContent spaceEvenly
            alignItems center
            flexGrow 3.0
        ]
        [ HH.div [
            HC.style do
              display flex
              flexDirection row
              alignItems center
              justifyContent center
              themeFont
          ]
          [ HH.label [
              HC.style do
                width (rem 6.0)
            ] [ HH.text "Username: " ]
          , HH.input [
              HC.style do
                backgroundColor paperColor
                width (vw 8.3)
                paddingLeft (rem 0.5)
                paddingRight (rem 0.5)
                fontSize (vw 1.0)
            ]
          ]
        , HH.div [
            HC.style do
              display flex
              flexDirection row
              alignItems center
              justifyContent center
              themeFont
          ]
          [ HH.label [
              HC.style do
                width (rem 6.0)
            ] [ HH.text "Password: " ]
          , HH.input [
              HP.type_ InputPassword
            , HC.style do
              backgroundColor paperColor
              width (vw 8.3)
              paddingLeft (rem 0.5)
              paddingRight (rem 0.5)
              fontSize (vw 1.0)
            ]
          ]
        ]
      , HH.div [
          HC.style do
            display flex
            flexDirection row
            alignItems center
            justifyContent center
            paddingTop $ vw 0.65
        ]
        [ HH.button
          [
            HC.style do
              backgroundColor themeColor
              themeFont
              fontWeight $ FontWeight $ value "500"
              fontSize $ vw 1.0
              width (rem 20.0)
              height $ vw 3.0
              color white
              cursor pointer
            , HE.onClick $ const Logon
            ]
            [ HH.text "LOGON" ]
          ]
        ]