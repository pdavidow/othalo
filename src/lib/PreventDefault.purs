module PreventDefault 
  ( Query(..)
  , preventClick
  , preventDrag
  )
    where
-- todo unused??????????
  
-- https://github.com/slamdata/purescript-halogen/issues/426  

{-- example usage

render :: H.ComponentHTML Query
render =
  HH.a [ HP.href "", preventClick NoOp ] [ HH.text "Do something" ]

--}

import Prelude
import Data.Maybe (Maybe(..))

import Run (liftEff)
import DOM.Classy.Event (preventDefault, toEvent)
import DOM.Event.Event (Event)

import Halogen as H
import Halogen.HTML.Events as HE

  
data Query a
  = NoOp a
  | PreventDefault Event (Query a)


eval = case _ of
  NoOp next -> pure next
  PreventDefault ev q -> do
    liftEff $ preventDefault ev
    eval q 


preventClick q =
  HE.onClick \e -> Just $ PreventDefault (toEvent e) $ H.action q


preventDrag q =
  HE.onDragStart \e -> Just $ PreventDefault (toEvent e) $ H.action q


