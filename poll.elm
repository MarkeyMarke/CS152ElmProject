port module Poll exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

port clearAnswerButtons: Bool -> Cmd msg
port focusOnQuestionText: Bool -> Cmd msg

main =
  Html.program
      {
      init=init,
      view = view,
      update = update,
      subscriptions= \_ -> Sub.none
      }

type State =
    CreateQuestionState
    | SubmittingVotesState
    | ReviewResultsState

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
  , numberVotesFor1 : Int
  , numberVotesFor2 : Int
  , numberVotesFor3 : Int
  , numberVotesFor4 : Int
  }

init: (Model, Cmd Msg)
init = model ! [focusOnQuestionText True]

--MODEL (Data)
--Set as Type Model
model : Model
--Instance Variables , Instantiate
model =
  Model CreateQuestionState "Your question will show here." "A goes here." "B goes here." "C goes here." "D goes here." 4 0 0 0 0 0






--UPDATE (Change Data)
-- Define Msg
-- Msg is signal to the system, we use case matching to allow the view section to give a particular signal
type Msg = 
  GoToNextState --Submits the current model values and updates it for the answerer's POV
  | SetQuestion String --sets a new question string
  | SetAnswer Int String --sets a new answer string
  | SetCorrectAnswer Int --sets a new answer index (1 for first answer, x for xth answer)
  | SetChosenAnswer Int
  | Answer

--Update will take a message signal based on what kind of messgae the view section has given
--The common property with all of them is that they may pass parameters of their own, and they replace the model data with
--an updated copy of itself.
update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of

    -- GoToNextState is a state machine with a cyclic behavior.
    GoToNextState ->
      case model.state of
        -- submit a new question for people to vote on.
        CreateQuestionState -> {model | state = SubmittingVotesState, tempChosen = ((model.answerIndex % 4) + 1)} ! [focusOnQuestionText True]
        -- finish accepting votes and go to the results page
        SubmittingVotesState -> {model | state = ReviewResultsState} ! []
        ReviewResultsState -> (
            -- reset to a blank model
            (Model CreateQuestionState "Your question will show here." "A goes here." "B goes here." "C goes here." "D goes here." 1 0 0 0 0 0),
            -- focus on the question input text
            Cmd.batch [focusOnQuestionText True]
          )
    SetQuestion q -> {model | question = q} ! []
    SetAnswer int ans->
      let newModel =
        case int of
        --Generate Updated Clone Copy of Model, this is Elm's equivalent to using a setter for an object
        1 -> { model | questionerChoice1 = ans}
        2 -> { model | questionerChoice2 = ans}
        3 -> { model | questionerChoice3 = ans}
        4 -> { model | questionerChoice4 = ans}
        _ -> { model | questionerChoice1 = ans} --We're setting choice 1 as the default choice if anything goes wrong
      in
        newModel ! []
    SetCorrectAnswer num -> {model | answerIndex = num} ! []
    SetChosenAnswer num -> {model | tempChosen = num} ! []
    Answer ->
      let newModel =
        case model.tempChosen of
          1 -> {model | numberVotesFor1 = model.numberVotesFor1 + 1}
          2 -> {model | numberVotesFor2 = model.numberVotesFor2 + 1}
          3 -> {model | numberVotesFor3 = model.numberVotesFor3 + 1}
          4 -> {model | numberVotesFor4 = model.numberVotesFor4 + 1}
          _ -> model
      in
        (newModel, clearAnswerButtons True)






{-VIEW (Print out the data)
View's job is to interpret the model every time it is updated. We will take the date from model and display it in GUI
View's second job is to provide controls to the user to update the model. This is done through text fields and buttons
of all kinds. They will take input, and then pass a message to update. It is up for us to decide what parameters are
necessary and how it works. -}
view: Model -> Html Msg
view model = 
  case model.state of
    CreateQuestionState -> questionView model
    SubmittingVotesState -> submitVotesView model
    ReviewResultsState -> resultView model

questionView : Model -> Html Msg
questionView model =
  div [] --View is literally one giant hierarchy of HTML. The first [] is for attributes, and the second [] is for content.
    [ 
    br [] [] --We use an empty br to break down into a new line, or provide spacing.
    
     --Beginning of Questioner's POV
    , fieldset [] 
        [
        div [] [ text "Question ", input [placeholder "Enter your question here.", onInput SetQuestion, id "questionText"] []]
        , br [][], div [][text "Set one answer as the correct answer."], br [][]
        , question "A" 1
        , br [][]
        , question "B" 2
        , br [][]
        , question "C" 3
        , br [][]
        , question "D" 4
        , br [][]
        , button [ onClick GoToNextState ] [ text "Submit" ]
        ]
    , br [] [] , br [] []
    --End of Questioners POV
    --Model contents preview.
    , fieldset []
        [
        div [] [text model.question]
        , div [] [text ("A ) " ++ model.questionerChoice1)]
        , div [] [text ("B ) " ++ model.questionerChoice2)]
        , div [] [text ("C ) " ++ model.questionerChoice3)]
        , div [] [text ("D ) " ++ model.questionerChoice4)]
        , div [] [text ("The correct answer is " ++ (indexToLetter model.answerIndex) ++ ".")]
        ]
    , br [] []
    --End of model contents preview.
    ]

submitVotesView: Model -> Html Msg
submitVotesView model = 
  div[]
    [
    br [][]
    , fieldset []
        [
        div [] [text model.question]
        , br [][]
        , answerRadioButton ("A ) " ++ model.questionerChoice1) 1 "A"
        , br [][]
        , answerRadioButton ("B ) " ++ model.questionerChoice2) 2 "B"
        , br [][]
        , answerRadioButton ("C ) " ++ model.questionerChoice3) 3 "C"
        , br [][]
        , answerRadioButton ("D ) " ++ model.questionerChoice4) 4 "D"
        , br [][]
        , button [ onClick Answer, id "voteButton" ] [ text "Vote" ]
        ]
    , br [] []
    , button [ onClick GoToNextState ] [ text "Done" ]
    ]

correctMarker answerIndex index =
  if answerIndex == index then "*" else " "

resultView model =
  let
    totalVotes = toFloat (model.numberVotesFor1 + model.numberVotesFor2 + model.numberVotesFor3 + model.numberVotesFor4)
    proportionAnswerChoice1 = toString (round ((toFloat model.numberVotesFor1) / totalVotes * 100))
    proportionAnswerChoice2 = toString (round ((toFloat model.numberVotesFor2) / totalVotes * 100))
    proportionAnswerChoice3 = toString (round ((toFloat model.numberVotesFor3) / totalVotes * 100))
    proportionAnswerChoice4 = toString (round ((toFloat model.numberVotesFor4) / totalVotes * 100))
  in
    div[]
      [
      br [][]
      , fieldset []
          [
          div [] [text ("Results for question '" ++ model.question ++ "':")]
          , div [] [text ((toString model.numberVotesFor1) ++ " (" ++ proportionAnswerChoice1 ++ "%) " ++ (correctMarker model.answerIndex 1) ++ " " ++ model.questionerChoice1)]
          , div [] [text ((toString model.numberVotesFor2) ++ " (" ++ proportionAnswerChoice2 ++ "%) " ++ (correctMarker model.answerIndex 2) ++ " " ++ model.questionerChoice2)]
          , div [] [text ((toString model.numberVotesFor3) ++ " (" ++ proportionAnswerChoice3 ++ "%) " ++ (correctMarker model.answerIndex 3) ++ " " ++ model.questionerChoice3)]
          , div [] [text ((toString model.numberVotesFor4) ++ " (" ++ proportionAnswerChoice4 ++ "%) " ++ (correctMarker model.answerIndex 4) ++ " " ++ model.questionerChoice4)]
          ]
      , br [][]
      , button [ onClick GoToNextState ] [ text "Start Over" ]
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
           [ input [ type_ "radio", name "question", checked False, onClick (SetCorrectAnswer newAnswerIndex)] [], text textValue]
          --ending of radio button
        , input [ placeholder "Enter your answer here.", onInput (SetAnswer newAnswerIndex)] []
        ]

answerRadioButton : String -> Int -> String -> Html Msg
answerRadioButton textValue newAnswerIndex idString =
  div []
        [
          --beginning of radio button
          label [ style [("padding", "20px")]] [
            input [
              type_ "radio",
              name "answer",
              class "answerButton",
              checked False,
              onClick (SetChosenAnswer newAnswerIndex),
              id idString
            ] [],
            text textValue
          ]
          -- ending of radio button
        ]
