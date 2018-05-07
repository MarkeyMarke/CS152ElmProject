
{- Basic ElmFire Example App
Write the text from a input field to a Firebase location.
Query that same location and display the result.
Use the displayed link to show the Firebase bashboard for the location.
-}
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
import String exposing (toInt, split)
import Array exposing (get, fromList)
import Maybe exposing (withDefault)
import Result exposing (withDefault)
import Regex exposing (..)

import ElmFire exposing
  ( fromUrl, set, subscribe, valueChanged, noOrder, noLimit
  , Reference, Snapshot, Subscription, Error
  )

main : Signal Html
main = Signal.map6 view questionBox.signal a1Box.signal a2Box.signal a3Box.signal a4Box.signal answerPicksBoxRead.signal

-- You may want to change this url, but you don't have to
questionURL : String
questionURL = "https://testproj1-5fbcf.firebaseio.com/QuestionBody/Question"

a1URL : String
a1URL = "https://testproj1-5fbcf.firebaseio.com/QuestionBody/Answer1"

a2URL : String
a2URL = "https://testproj1-5fbcf.firebaseio.com/QuestionBody/Answer2"

a3URL : String
a3URL = "https://testproj1-5fbcf.firebaseio.com/QuestionBody/Answer3"

a4URL : String
a4URL = "https://testproj1-5fbcf.firebaseio.com/QuestionBody/Answer4"


answerPicksBoxWrite : Mailbox String
answerPicksBoxWrite =
    mailbox "0%0%0%0"

answerPicksBoxRead : Mailbox JE.Value
answerPicksBoxRead = mailbox JE.null

questionBox : Mailbox JE.Value
questionBox = mailbox JE.null

a1Box : Mailbox JE.Value
a1Box = mailbox JE.null

a2Box : Mailbox JE.Value
a2Box = mailbox JE.null

a3Box : Mailbox JE.Value
a3Box = mailbox JE.null

a4Box : Mailbox JE.Value
a4Box = mailbox JE.null


doNothing : a -> Task x ()
doNothing = always (Task.succeed ())

port runSetAnswerPicks : Signal (Task Error Reference)
port runSetAnswerPicks =
    Signal.map
        (\str -> set (string str) 
        (fromUrl "https://testproj1-5fbcf.firebaseio.com/AnswerPicks"))
        answerPicksBoxWrite.signal

port runQueryAnswerPicks : Task Error Subscription
port runQueryAnswerPicks =
    subscribe
        (Signal.send answerPicksBoxRead.address << .value)
        doNothing
        (valueChanged noOrder)
        (fromUrl "https://testproj1-5fbcf.firebaseio.com/AnswerPicks")

port runQueryQuestion : Task Error Subscription
port runQueryQuestion =
    subscribe
        (Signal.send questionBox.address << .value)
        doNothing
        (valueChanged noOrder)
        (fromUrl questionURL)

port runQueryA1 : Task Error Subscription
port runQueryA1 =
    subscribe
        (Signal.send a1Box.address << .value)
        doNothing
        (valueChanged noOrder)
        (fromUrl a1URL)

port runQueryA2 : Task Error Subscription
port runQueryA2 =
    subscribe
        (Signal.send a2Box.address << .value)
        doNothing
        (valueChanged noOrder)
        (fromUrl a2URL)

port runQueryA3 : Task Error Subscription
port runQueryA3 =
    subscribe
        (Signal.send a3Box.address << .value)
        doNothing
        (valueChanged noOrder)
        (fromUrl a3URL)

port runQueryA4 : Task Error Subscription
port runQueryA4 =
    subscribe
        (Signal.send a4Box.address << .value)
        doNothing
        (valueChanged noOrder)
        (fromUrl a4URL)


view : JE.Value -> JE.Value -> JE.Value -> JE.Value -> JE.Value -> JE.Value -> Html
view value1 value2 value3 value4 value5 value6 =
  let 
    questionText = encode 0 value1
    a1Text = encode 0 value2
    a2Text = encode 0 value3
    a3Text = encode 0 value4
    a4Text = encode 0 value5
    answerPicks = encode 0 value6
  in
    div []
        [ 
        fieldset [] [
            div []
                [ label [] [ text "Question: ", output [] [ text (questionText) ]]
                ]
            , div []
                [ button [ onClick answerPicksBoxWrite.address (addTo (String.slice 1 ((String.length answerPicks) - 1) answerPicks) 1)] [ text "Vote A" ] 
                , label [] [ text " A ) ", output [] [ text (a1Text) ]]
                , br [][]
                ]
            , div []
                [ button [ onClick answerPicksBoxWrite.address (addTo (String.slice 1 ((String.length answerPicks) - 1) answerPicks) 2)] [ text "Vote B" ] 
                , label [] [ text " B ) ", output [] [ text (a2Text) ]]
                , br [][]
                ]
            , div []
                [ button [ onClick answerPicksBoxWrite.address (addTo (String.slice 1 ((String.length answerPicks) - 1) answerPicks) 3)] [ text "Vote C" ] 
                , label [] [ text " C ) ", output [] [ text (a3Text) ]]
                , br [][]
                ]
            , div []
                [ button [ onClick answerPicksBoxWrite.address (addTo (String.slice 1 ((String.length answerPicks) - 1) answerPicks) 4)] [ text "Vote D" ] 
                , label [] [ text " D ) ", output [] [ text (a4Text) ]]
                , br [][]
                ]
--            , div []
--                [ label [] [ text "ANSWERPICKS ", output [] [ text (answerPicks) ]]
--                ]
            ]
        ]

--addTo function takes the answerChoice string and increments accordingly to the int parameter (index)
--first grabs the index of the string needed to be incremented by 1, then pushes that value back into an array
-- with the original components, then produce a string from that updated array
addTo : String -> Int -> String
addTo inputString index = 
  case index of 
    1 -> 
      let 
        num = toString ((Result.withDefault 0 (String.toInt (Maybe.withDefault "0" (Array.get 0 (Array.fromList (Regex.split All (regex "%") inputString)))))) + 1)
        array = (Array.fromList (Regex.split All (regex "%") inputString))
      in
        String.join "%" (Array.toList (Array.set 0 num array))
    2 -> 
      let 
        num = toString ((Result.withDefault 0 (String.toInt (Maybe.withDefault "0" (Array.get 1 (Array.fromList (Regex.split All (regex "%") inputString)))))) + 1)
        array = (Array.fromList (Regex.split All (regex "%") inputString))
      in
        String.join "%" (Array.toList (Array.set 1 num array))
    3 -> 
      let 
        num = toString ((Result.withDefault 0 (String.toInt (Maybe.withDefault "0" (Array.get 2 (Array.fromList (Regex.split All (regex "%") inputString)))))) + 1)
        array = (Array.fromList (Regex.split All (regex "%") inputString))
      in
        String.join "%" (Array.toList (Array.set 2 num array))
    4 -> 
      let 
        num = toString ((Result.withDefault 0 (String.toInt (Maybe.withDefault "0" (Array.get 3 (Array.fromList (Regex.split All (regex "%") inputString)))))) + 1)
        array = (Array.fromList (Regex.split All (regex "%") inputString))
      in
        String.join "%" (Array.toList (Array.set 3 num array))
    _ -> inputString