module SimpleComponent where
--https://functionalprogramming.slack.com/files/U5KHK7Z1C/FAJ5VSRRQ/simplecomponent__with_aff_.hs

import Prelude

import Control.Monad.Aff (Aff)
import DOM (DOM)
import DOM.Classy.Event (preventDefault, toEvent)
import DOM.Event.Event (Event)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data Query a
  = ToggleState a
  | PreventDefault Event a

type State = { on :: Boolean }
type Effects eff = ( dom :: DOM | eff )

component :: forall eff. H.Component HH.HTML Query Unit Void (Aff (Effects eff))
component =
    H.component
      { initialState: const initialState
      , render
      , eval
      , receiver: const Nothing
      }
    where
        initialState :: State
        initialState = { on: true }

        render :: State -> H.ComponentHTML Query
        render state =
          HH.div_
            [ HH.img
              [ HP.src $ urlForState state
              , HE.onClick $ HE.input_ ToggleState
              , HE.onDragStart $ HE.input $ PreventDefault <<< toEvent
              ]
            ]


        urlForState :: State -> String
        urlForState state = 
            case state.on of
                true -> "../src/resource/blackDisk.png"
                false -> "../src/resource/whiteDisk.png"


        eval :: Query ~> H.ComponentDSL State Query Void (Aff (Effects eff))
        eval q = 
            case q of
                ToggleState next -> do
                  H.modify \st -> { on: not st.on }
                  pure next

                PreventDefault event next -> do
                  H.liftEff $ preventDefault event
                  pure next