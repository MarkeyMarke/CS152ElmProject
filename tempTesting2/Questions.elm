module Main (..) where

import StartApp
import Regex exposing (..)
import Effects exposing (Effects)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html exposing (Html, div, input, output, label, text, a)
import Html.Events exposing (on, targetValue)
import Html.Attributes exposing (href, target)
import Signal exposing (Address, Signal, Mailbox, mailbox, message)
import Task exposing (Task)
import Json.Encode as JE exposing (string, encode)
import ElmFire
    exposing
        ( fromUrl
        , set
        , subscribe
        , valueChanged
        , noOrder
        , noLimit
        , Reference
        , Snapshot
        , Subscription
        , Error
        )


--MAILBOX FOR SENDING OUT SIGNALS TO MULTIPLE ADDRESSES, this is used to update signals for all of our URL mailboxes at once


proxy : Mailbox (List ( Address a, a ))
proxy =
    mailbox []


port broadcast : Signal (Task x (List ()))
port broadcast =
    let
        tasks =
            List.map (uncurry Signal.send)
    in
        Signal.map (Task.sequence << tasks) proxy.signal



--MAIL BOXES INITIALIZED FOR ALL THE KEYS FROM DATABASE
{- Each mailbox is initialized by using the port functions to a specific URL/key to the Firebase DB. The string argument for the mailbox
   is the initial value that the specified key will have upon refreshing the questioner's browser as well as upon initial load of the browser.
-}


inputStringQ : Mailbox String
inputStringQ =
    mailbox model.question


inputStringA1 : Mailbox String
inputStringA1 =
    mailbox model.choice1


inputStringA2 : Mailbox String
inputStringA2 =
    mailbox model.choice2


inputStringA3 : Mailbox String
inputStringA3 =
    mailbox model.choice3


inputStringA4 : Mailbox String
inputStringA4 =
    mailbox model.choice4


inputStringAI : Mailbox String
inputStringAI =
    mailbox (toString model.answerIndex)


inputStringVoteCount : Mailbox String
inputStringVoteCount =
    mailbox "0%0%0%0"



--END OF MAILBOXES
--PORTS FOR EVERY DATA
{- These port functions allow the mailboxes initialized earlier to be linked with a correponding key in the Firebase DB, the url specifies each location
   of the stated key.
-}


port runSet : Signal (Task Error Reference)
port runSet =
    Signal.map
        (\str -> set (string str) (fromUrl "https://elmproj.firebaseio.com/QuestionBody/Question"))
        inputStringQ.signal


port runSetA1 : Signal (Task Error Reference)
port runSetA1 =
    Signal.map
        (\str -> set (string str) (fromUrl "https://elmproj.firebaseio.com/QuestionBody/Answer1"))
        inputStringA1.signal


port runSetA2 : Signal (Task Error Reference)
port runSetA2 =
    Signal.map
        (\str -> set (string str) (fromUrl "https://elmproj.firebaseio.com/QuestionBody/Answer2"))
        inputStringA2.signal


port runSetA3 : Signal (Task Error Reference)
port runSetA3 =
    Signal.map
        (\str -> set (string str) (fromUrl "https://elmproj.firebaseio.com/QuestionBody/Answer3"))
        inputStringA3.signal


port runSetA4 : Signal (Task Error Reference)
port runSetA4 =
    Signal.map
        (\str -> set (string str) (fromUrl "https://elmproj.firebaseio.com/QuestionBody/Answer4"))
        inputStringA4.signal


port runSetAI : Signal (Task Error Reference)
port runSetAI =
    Signal.map
        (\str -> set (string str) (fromUrl "https://elmproj.firebaseio.com/QuestionBody/AnswerIndex"))
        inputStringAI.signal


port runSetVoteCount : Signal (Task Error Reference)
port runSetVoteCount =
    Signal.map
        (\str -> set (string str) (fromUrl "https://elmproj.firebaseio.com/AnswerPicks"))
        inputStringVoteCount.signal



--END OF PORTS
--MAIN PROGRAM STARTS HERE


app =
    StartApp.start { init = init, update = update, view = view, inputs = [] }


main =
    app.html



--INITILIAZATION of our poll elm application, initiliazes with a model and no effect


init : ( Model, Effects Action )
init =
    ( model, Effects.none )



--MODEL (Data)
--Type alias is similar to defining a struct in C. Everything is enclosed in a 'record' with given data types.


type alias Model =
    { question : String
    , choice1 : String
    , choice2 : String
    , choice3 : String
    , choice4 : String
    , answerIndex : Int
    }



--Here, we create a variable called model, and define it as type model. We then initialize it with default values.


model : Model
model =
    Model "Your question will show here." "A goes here." "B goes here." "C goes here." "D goes here." 4



-- UPDATE (Change Data)
-- Define Action (updated to Msg in Elm v18)
-- Action is signal to the system, we use case matching to allow the view section to give a particular signal
-- Update takes an Action and a model then returns a tuple of a Model and an Effect (updated to Cmd in v18). The Effect comes with an Action (updated to Msg in Elm v18)


type Action
    = SetQuestion String
      --sets a new question string
    | SetAnswer Int String
      --sets a new answer string
    | SetCorrectAnswer Bool Int


update : Action -> Model -> ( Model, Effects Action )
update action model =
    case action of
        --SetStr will set the string of a model to the input string and pass no Effect thru
        SetQuestion q ->
            ( { model | question = q }, Effects.none )

        SetAnswer int ans ->
            case int of
                --Generate Updated Clone Copy of Model, this is Elm's equivalent to using a setter for an object
                1 ->
                    ( { model | choice1 = ans }, Effects.none )

                2 ->
                    ( { model | choice2 = ans }, Effects.none )

                3 ->
                    ( { model | choice3 = ans }, Effects.none )

                4 ->
                    ( { model | choice4 = ans }, Effects.none )

                _ ->
                    ( { model | choice1 = ans }, Effects.none )

        SetCorrectAnswer bool num ->
            case bool of
                --In 0.16, Elm didn't provide a library such that the listeners were chcked for input for us. So we have do check if the input worked ourselves.
                True ->
                    --If the action worked, change the model.
                    ( { model | answerIndex = num }, Effects.none )

                False ->
                    --Otherwise, keep the model the same.
                    ( model, Effects.none )



-- VIEW (Converts data from our model into an HTML display)
--Text box sends input value and replaces our model's string
-- NO CLUE WHAT SIGNAL.ADDRESS IS


view : Signal.Address Action -> Model -> Html
view address model =
    --View is literally one giant hierarchy of HTML. The first [] is for attributes, and the second [] is for content.
    div []
        [ br [] []
          --We use an empty br to break down into a new line, or provide spacing.
          --Beginning of Questioner's POV
        , fieldset []
            [ div [] [ text "Question ", input [ placeholder "Enter your question here.", on "input" targetValue (\str -> Signal.message address (SetQuestion str)) ] [] ]
            , br [] []
            , div [] [ text "Set one answer as the correct answer." ]
            , br [] []
            , question address "A" 1
            , br [] []
            , question address "B" 2
            , br [] []
            , question address "C" 3
            , br [] []
            , question address "D" 4
            , br [] []
              --Sends one string to our database
            , button
                [ onClick proxy.address
                    [ ( inputStringQ.address, model.question )
                    , ( inputStringA1.address, model.choice1 )
                    , ( inputStringA2.address, model.choice2 )
                    , ( inputStringA3.address, model.choice3 )
                    , ( inputStringA4.address, model.choice4 )
                    , ( inputStringAI.address, toString (model.answerIndex) )
                    , ( inputStringVoteCount.address, "0%0%0%0" )
                    ]
                ]
                [ text "Submit" ]
            ]
        , br [] []
        , br [] []
        , fieldset []
            [ div [] [ text model.question ]
            , div [] [ text ("A ) " ++ model.choice1) ]
            , div [] [ text ("B ) " ++ model.choice2) ]
            , div [] [ text ("C ) " ++ model.choice3) ]
            , div [] [ text ("D ) " ++ model.choice4) ]
            , div [] [ text ("The correct answer is " ++ (indexToLetter model.answerIndex) ++ ".") ]
            , br [] []
            ]
          -- Answerers POV
        , br [] []
        , br [] []
        ]


indexToLetter : Int -> String
indexToLetter index =
    case index of
        1 ->
            "A"

        2 ->
            "B"

        3 ->
            "C"

        4 ->
            "D"

        _ ->
            "Answer out of bounds."



{- Radio Buttons use groupNames to exclude other button in the same group, textValue to provide a text paired up with it,
   and newAnswerIndex which takes an int for the new answer index
-}


question : Signal.Address Action -> String -> Int -> Html
question address textValue newAnswerIndex =
    div []
        [ --beginning of radio button
          label
            [ style [ ( "padding", "20px" ) ] ]
            [ input [ type' "radio", name "question", checked True, on "input" targetChecked (\bool -> Signal.message address (SetCorrectAnswer bool newAnswerIndex)) ] [], text textValue ]
          --ending of radio button
        , input [ placeholder "Enter your answer here.", on "input" targetValue (\str -> Signal.message address (SetAnswer newAnswerIndex str)) ] []
        ]
