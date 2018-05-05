import StartApp
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

main =
  StartApp.start { model = model, view = view, update = update }


--Define Model. Model is similar to a struct in C, or an object in JS, but it is immutable. Elm gets around this by
--Generating a new copy of that object but which shares memory with the original record, except for the changed values.
type alias Model = {
  str : String
}
--MODEL (Data)
--Set as Type Model
model : Model
--Instance Variables , Instantiate
model = "Hi"



--UPDATE (Change Data)
-- Define Msg
-- Msg is signal to the system, we use case matching to allow the view section to give a particular signal
type Action = 
  SetStr String --Submits the current model values and updates it for the answerer's POV

--Update will take a message signal based on what kind of messgae the view section has given
--The common property with all of them is that they may pass parameters of their own, and they replace the model data with
--an updated copy of itself.
update actn model =
  case actn of
    SetStr string-> 
      {model | str = string}


{-VIEW (Print out the data)
View's job is to interpret the model every time it is updated. We will take the date from model and display it in GUI
View's second job is to provide controls to the user to update the model. This is done through text fields and buttons
of all kinds. They will take input, and then pass a message to update. It is up for us to decide what parameters are
necessary and how it works. -}
view : Signal.Address Action -> Model -> Html
view address model =
  div [] [
    text ("YOOFDSOFFAFA") 
    ]

    
