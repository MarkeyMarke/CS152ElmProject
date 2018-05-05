import StartApp
import Effects exposing (Effects)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html exposing (Html, div, input, output, label, text, a)
import Html.Events exposing (on, targetValue)
import Html.Attributes exposing (href, target)
import Signal exposing (Signal, Mailbox, mailbox, message)
import Task exposing (Task)
import Json.Encode as JE exposing (string, encode)
import ElmFire exposing
  ( fromUrl, set, subscribe, valueChanged, noOrder, noLimit
  , Reference, Snapshot, Subscription, Error
  )

url : String
url = "https://testproj1-5fbcf.firebaseio.com/QuestionBody/Answer1"

values : Mailbox JE.Value
values = mailbox JE.null

inputString : Mailbox String
inputString = mailbox ""

port runSet : Signal (Task Error Reference)
port runSet = Signal.map
  (\str -> set (string str) (fromUrl url))
  inputString.signal

doNothing : a -> Task x ()
doNothing = always (Task.succeed ())

port runQuery : Task Error Subscription
port runQuery =
    subscribe
        (Signal.send values.address << .value)
        doNothing
        (valueChanged noOrder)
        (fromUrl url)


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
  question : String
  , choice1 : String
  , choice2 : String
  , choice3 : String
  , choice4 : String
  , answerIndex : Int
}

--MODEL (Data)
--Set as Type Model
model : Model
--Instantiate Instance Variables
model = Model "Your question will show here." "A goes here." "B goes here." "C goes here." "D goes here." 1



-- UPDATE (Change Data)
-- Define Action (updated to Msg in Elm v18)
-- Action is signal to the system, we use case matching to allow the view section to give a particular signal
type Action = 
  SetQuestion String --sets a new question string
  | SetAnswer Int String --sets a new answer string
  | SetCorrectAnswer Bool Int

--Update takes an Action and a model then returns a tuple of a Model and an Effect (updated to Cmd in v18) with and Action (updated to Msg in Elm v18)
update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    --SetStr will set the string of a model to the input string and pass no Effect thru
    SetQuestion q -> ({model | question = q}, Effects.none)
    SetAnswer int ans -> 
        case int of
            --Generate Updated Clone Copy of Model, this is Elm's equivalent to using a setter for an object
            1 -> ({ model | choice1 = ans},  Effects.none)
            2 -> ({ model | choice2 = ans},  Effects.none)
            3 -> ({ model | choice3 = ans},  Effects.none)
            4 -> ({ model | choice4 = ans},  Effects.none)
            _ -> ({ model | choice1 = ans},  Effects.none)
    SetCorrectAnswer bool num -> 
        case bool of 
            True ->
                ({model | answerIndex = num}, Effects.none)
            False ->
                (model, Effects.none)

   {-} Submit bool -> 
        case bool of
            True -> (model, submitButtonPressed)
            False -> (model, Effects.none)
            -}

-- VIEW 
-- Prints out the values of our model using html
--Text box sends input value and replaces our model's string
-- NO CLUE WHAT SIGNAL.ADDRESS IS
view : Signal.Address Action -> Model -> Html
view address model =
    div [] --View is literally one giant hierarchy of HTML. The first [] is for attributes, and the second [] is for content.
    [ 
    br [] [] --We use an empty br to break down into a new line, or provide spacing.
    
     --Beginning of Questioner's POV
    , fieldset [] 
        [
        div [] [ text "Question ", input [placeholder "Enter your question here.", on "input" targetValue (\str -> Signal.message address (SetQuestion str))] []]
        , br [][], div [][text "Set one answer as the correct answer."], br [][]
        , question address "A" 1
        , br [][]
        , question address "B" 2
        , br [][]
        , question address "C" 3
        , br [][]
        , question address "D" 4
        , br [][]
        --IMPLEMENT function to send all model variables into database
        --, button [ on "input" targetChecked (\bool -> (submitButtonPressed bool)) ] [ text "Submit" ]
        ]

    , br [] [] , br [] []

    , fieldset []
        [
        div [] [text model.question]
        , div [] [text ("A ) " ++ model.choice1)]
        , div [] [text ("B ) " ++ model.choice2)]
        , div [] [text ("C ) " ++ model.choice3)]
        , div [] [text ("D ) " ++ model.choice4)]
        , div [] [text ("The correct answer is " ++ (indexToLetter model.answerIndex) ++ ".")]
        ]
    , br [] []
    ]

indexToLetter : Int -> String
indexToLetter index =
  case index of
  1 -> "A"
  2 -> "B"
  3 -> "C"
  4 -> "D"
  _ -> ""

{-}
submitButtonPressed bool = 
    case bool of
            True ->
                message inputString.address model.choice1
            False ->
                message  ""
-}

{-(Radio Button + Text Box)
Radio Buttons use groupNames to exclude other button in the same group, textValue to provide a text paired up with it,
and newAnswerIndex which takes an int for the new answer index
Takes in groupName to pass into radio, takes in a textValue to assign that button, newAnswerIndex is the new answer -}
question : Signal.Address Action -> String -> Int -> Html
question address textValue newAnswerIndex =
  div []
        [ 
          --beginning of radio button
          label
           [ style [("padding", "20px")]]
           [ input [ type' "radio", name "question", on "input" targetChecked (\bool -> Signal.message address (SetCorrectAnswer bool newAnswerIndex))] [], text textValue]
          --ending of radio button
        , input [ placeholder "Enter your answer here.", on "input" targetValue (\str -> Signal.message address (SetAnswer newAnswerIndex str))] []
        ]



    
