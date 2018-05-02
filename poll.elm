import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

main =
  Html.beginnerProgram { model = model, view = view, update = update }

type State =
  Questioning
  | Answering
  | Results

--Define Model. Model is similar to a struct in C, or an object in JS, but it is immutable. Elm gets around this by
--Generating a new copy of that object but which shares memory with the original record, except for the changed values.
type alias Model =
  { 
  --What state the application is in
    state : State
  --Questioner's POV
  --The Question
  , question : String
  --String values for questions
  , questionerChoice1 : String
  , questionerChoice2 : String
  , questionerChoice3 : String
  , questionerChoice4 : String
  --answer index (1 for first answer, x for xth answer)
  , answerIndex : Int
  --Answerer's POV
  , tempChosen : Int
  , answerChoice1 : Int
  , answerChoice2 : Int
  , answerChoice3 : Int
  , answerChoice4 : Int
  }

--MODEL (Data)
--Set as Type Model
model : Model
--Instance Variables , Instantiate
model =
  Model Questioning "Your question will show here." "A1 goes here." "A2 goes here." "A3 goes here." "A4 goes here." 1 0 0 0 0 0






--UPDATE (Change Data)
-- Define Msg
-- Msg is signal to the system, we use case matching to allow the view section to give a particular signal
type Msg = 
  Submit --Submits the current model values and updates it for the answerer's POV
  | SetQuestion String --sets a new question string
  | SetAnswer Int String --sets a new answer string
  | SetCorrectAnswer Int --sets a new answer index (1 for first answer, x for xth answer)
  | SetChosenAnswer Int
  | Answer

--Update will take a message signal based on what kind of messgae the view section has given
--The common property with all of them is that they may pass parameters of their own, and they replace the model data with
--an updated copy of itself.
update msg model =
  case msg of
    Submit -> 
      case model.state of
      Questioning -> {model | state = Answering}
      Answering -> {model | state = Results}
      Results -> Model Questioning "Your question will show here." "A1 goes here." "A2 goes here." "A3 goes here." "A4 goes here." 1 0 0 0 0 0
    SetQuestion q -> {model | question = q}
    SetAnswer int ans-> 
      case int of
      --Generate Updated Clone Copy of Model, this is Elm's equivalent to using a setter for an object
      1 -> { model | questionerChoice1 = ans}
      2 -> { model | questionerChoice2 = ans}
      3 -> { model | questionerChoice3 = ans}
      4 -> { model | questionerChoice4 = ans}
      _ -> { model | questionerChoice1 = ans} --We're setting choice 1 as the default choice if anything goes wrong
    SetCorrectAnswer num -> {model | answerIndex = num}
    SetChosenAnswer num -> {model | tempChosen = num}
    Answer ->
      case model.tempChosen of
        1 -> {model | answerChoice1 = model.answerChoice1 + 1}
        2 -> {model | answerChoice2 = model.answerChoice2 + 1}
        3 -> {model | answerChoice3 = model.answerChoice3 + 1}
        4 -> {model | answerChoice4 = model.answerChoice4 + 1}
        _ -> model






{-VIEW (Print out the data)
View's job is to interpret the model every time it is updated. We will take the date from model and display it in GUI
View's second job is to provide controls to the user to update the model. This is done through text fields and buttons
of all kinds. They will take input, and then pass a message to update. It is up for us to decide what parameters are
necessary and how it works. -}
view: Model -> Html Msg
view model = 
  case model.state of
    Questioning -> questionView model
    Answering -> answerView model
    Results -> resultView model

questionView : Model -> Html Msg
questionView model =
  div [] --View is literally one giant hierarchy of HTML. The first [] is for attributes, and the second [] is for content.
    [ 
    br [] [] --We use an empty br to break down into a new line, or provide spacing.
    
     --Beginning of Questioner's POV
    , fieldset [] 
        [
        div [] [ text "Question ", input [placeholder "Enter your question here.", onInput SetQuestion] []]
        , br [][], div [][text "Set one answer as the correct answer."], br [][]
        , question "A" 1
        , br [][]
        , question "B" 2
        , br [][]
        , question "C" 3
        , br [][]
        , question "D" 4
        , br [][]
        , button [ onClick Submit ] [ text "Submit" ]
        ]
    , br [] [] , br [] []
    --End of Questioners POV
    --Model contents preview.
    , fieldset []
        [
        div [] [text model.question]
        , div [] [text ("A.) " ++ model.questionerChoice1)]
        , div [] [text ("B.) " ++ model.questionerChoice2)]
        , div [] [text ("C.) " ++ model.questionerChoice3)]
        , div [] [text ("D.) " ++ model.questionerChoice4)]
        , div [] [text ("The correct answer is " ++ (indexToLetter model.answerIndex) ++ ".")]
        ]
    , br [] []
    --End of model contents preview.
    ]

answerView: Model -> Html Msg
answerView model = 
  div[]
    [
    br [][]
    , fieldset []
        [
        div [] [text model.question]
        , br [][]
        , answer ("A.) " ++ model.questionerChoice1) 1
        , br [][]
        , answer ("B.) " ++ model.questionerChoice2) 2
        , br [][]
        , answer ("C.) " ++ model.questionerChoice3) 3
        , br [][]
        , answer ("D.) " ++ model.questionerChoice4) 4
        , br [][]
        , button [ onClick Answer ] [ text "Vote" ]
        ]
    , br [] []
    , button [ onClick Submit ] [ text "Done" ]
    ]

resultView model =
  div[]
    [
    br [][]
    , fieldset []
        [
        div [] [text "Results:"]
        , div [] [text ("Votes for A: " ++ (toString model.answerChoice1))]
        , div [] [text ("Votes for B: " ++ (toString model.answerChoice2))]
        , div [] [text ("Votes for C: " ++ (toString model.answerChoice3))]
        , div [] [text ("Votes for D: " ++ (toString model.answerChoice4))]
        ]
    , br [][]
    , fieldset []
        [
        div [] [text model.question]
        , div [] [text ("A.) " ++ model.questionerChoice1)]
        , div [] [text ("B.) " ++ model.questionerChoice2)]
        , div [] [text ("C.) " ++ model.questionerChoice3)]
        , div [] [text ("D.) " ++ model.questionerChoice4)]
        , div [] [text ("The correct answer is " ++ (indexToLetter model.answerIndex) ++ ".")]
        ]
    , br [] []
    , button [ onClick Submit ] [ text "Start Over" ]
    ]

indexToLetter : Int -> String
indexToLetter index =
  case index of
  1 -> "A"
  2 -> "B"
  3 -> "C"
  4 -> "D"
  _ -> ""

-- (Radio Button + Text Box)
{- Radio Buttons use groupNames to exclude other button in the same group, textValue to provide a text paired up with it,
and newAnswerIndex which takes an int for the new answer index
Takes in groupName to pass into radio, takes in a textValue to assign that button, newAnswerIndex is the new answer -}
question : String -> Int -> Html Msg
question textValue newAnswerIndex =
  div []
        [ 
          --beginning of radio button
          label
           [ style [("padding", "20px")]]
           [ input [ type_ "radio", name "question", checked True, onClick (SetCorrectAnswer newAnswerIndex)] [], text textValue]
          --ending of radio button
        , input [ placeholder "Enter your answer here.", onInput (SetAnswer newAnswerIndex)] []
        ]

answer : String -> Int -> Html Msg
answer textValue newAnswerIndex =
  div []
        [
          --beginning of radio button
          label
           [ style [("padding", "20px")]]
           [ input [ type_ "radio", name "answer", checked True, onClick (SetChosenAnswer newAnswerIndex)] [], text textValue]
          --ending of radio button 
        ]