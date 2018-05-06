
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
main = Signal.map view values.signal

-- You may want to change this url, but you don't have to
url : String
url = "https://testproj1-5fbcf.firebaseio.com/AnswerPicks"

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

view : JE.Value -> Html
view value =
  let outputText = encode 0 value
  in
  div []
  [ text "ElmFire test at: "
  , a [href url, target "_blank"] [text url]
  , div []
    [ label []
      [ text "set value: "
      , input [ on "input" targetValue (message inputString.address) ] []
      ]
    ]
  , div []
              --outputText is displayed within quotatoin marks, so pass the string without quotes to the addTo function
    [ button [ onClick inputString.address (addTo (String.slice 1 ((String.length outputText) - 1) outputText) 2)] [ text "Submit" ]]
  , div []
    [ label []
      [ text "query result: "
      , output [] [ text outputText ]
      ]
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