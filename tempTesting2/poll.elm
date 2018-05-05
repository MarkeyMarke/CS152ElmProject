import StartApp
import Effects exposing (Effects)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

app =
    StartApp.start { init = init, update = update, view = view, inputs = [] }

main = 
    app.html

--INITILIAZATION of our poll elm application, initiliazes with a model and no effect
init : ( Model, Effects Action )
init =
    (model, Effects.none )


--basic implementation of model with a string variable
type alias Model = {
  str : String
}

--MODEL (Data)
--Set as Type Model
model : Model
--Instantiate Instance Variables
model = Model "initiliazed"



-- UPDATE (Change Data)
-- Define Action (updated to Msg in Elm v18)
-- Action is signal to the system, we use case matching to allow the view section to give a particular signal
type Action = 
  SetStr String--Submits the current model values and updates it for the answerer's POV

--Update takes an Action and a model then returns a tuple of a Model and an Effect (updated to Cmd in v18) with and Action (updated to Msg in Elm v18)
update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    --SetStr will set the string of a model to the input string and pass no Effect thru
    SetStr string -> 
      (Model string, Effects.none)


-- VIEW 
-- Prints out the values of our model using html
--Text box sends input value and replaces our model's string
-- NO CLUE WHAT SIGNAL.ADDRESS IS
view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ input [placeholder "Enter seomthign ...", on "input" targetValue (Signal.message address << SetStr)] []
        ,div [] [text (model.str)]
    ]

    
