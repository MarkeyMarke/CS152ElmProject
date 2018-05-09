module Results where

import Result
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Json.Decode as JD exposing (..)
import Json.Encode as JE exposing (..)
import Signal exposing (Mailbox, Address, mailbox, message)
import Task exposing (Task, andThen)
import Effects exposing (Effects, Never)
import Array exposing (..)
import Basics exposing (..)
import StartApp
import String
import Json.Decode as Json exposing ((:=))
import Task exposing (..)

import ElmFire
import ElmFire.Dict
import ElmFire.Op

--------------------------------------------------------------------------------

-- Configuration

-- URL of the Firebase to use
firebase_foreign : String
firebase_foreign = "https://elmproj.firebaseio.com/Questions"

-- This app uses the same data format as the firebase-angular implementation.
-- So we could use also their Firebase for testing
firebase_test : String
firebase_test = "https://elmproj.firebaseio.com/Questions"

firebaseUrl : String
firebaseUrl = firebase_test

--------------------------------------------------------------------------------

config : StartApp.Config Model Action
config =
  { init = (initialModel, initialEffect)
  , update = updateState
  , view = view
  , inputs = [Signal.map FromServer inputItems]
  }

app : StartApp.App Model
app = StartApp.start config

port runEffects : Signal (Task Never ())
port runEffects = app.tasks

main : Signal Html
main = app.html

--------------------------------------------------------------------------------

type alias Model =
  { items: Items
  , apiKey: Content
  }

type alias Items = Dict Id Item

type alias Id = String




type alias Content = String

initialModel : Model
initialModel =
  { items = Dict.empty
  , apiKey = ""
  }



-- Mirror Firbase's content as the model's items

-- initialTask : Task Error (Task Error ())
-- inputItems : Signal Items
(initialTask, inputItems) =
  ElmFire.Dict.mirror syncConfig

initialEffect : Effects Action
initialEffect = initialTask |> kickOff

--------------------------------------------------------------------------------

type alias Item =
  { choice1: String
  , choice2: String
  , choice3: String
  , choice4: String
  , c1Votes: List String
  , c2Votes: List String
  , c3Votes: List String
  , c4Votes: List String
  }

syncConfig : ElmFire.Dict.Config Item
syncConfig =
  { location = ElmFire.fromUrl (firebaseUrl)
  , orderOptions = ElmFire.noOrder
  , encoder =
      \item -> JE.object
        [ ("Choice1", JE.string item.choice1)
        , ("Choice2", JE.string item.choice2)
        , ("Choice3", JE.string item.choice3)
        , ("Choice4", JE.string item.choice4)
        ]
  , decoder =
      ( JD.object8 Item
          ("Choice1" := JD.string)
          ("Choice2" := JD.string)
          ("Choice3" := JD.string)
          ("Choice4" := JD.string))
          (JD.at [ "Votes", "C1"] (JD.keyValuePairs (JD.succeed ()) |> JD.map (List.map Basics.fst)))
          (JD.at [ "Votes", "C2"] (JD.keyValuePairs (JD.succeed ()) |> JD.map (List.map Basics.fst)))
          (JD.at [ "Votes", "C3"] (JD.keyValuePairs (JD.succeed ()) |> JD.map (List.map Basics.fst)))
          (JD.at [ "Votes", "C4"] (JD.keyValuePairs (JD.succeed ()) |> JD.map (List.map Basics.fst)))

  }

 --------------------------------------------------------------------------------
-- Map any task to an effect, discarding any direct result or error value
kickOff : Task x a -> Effects Action
kickOff =
  Task.toMaybe >> Task.map (always (FromEffect)) >> Effects.task

--------------------------------------------------------------------------------

type Action =
   FromServer Items
  | FromEffect -- no specific actions from effects here
  | SetAPIKey String
  --| SetVotes Items2

-- Process gui events and server events yielding model updates and effects

updateState : Action -> Model -> (Model, Effects Action)
updateState action model =
  case action of

    FromEffect ->
      ( model
      , Effects.none
      )

    FromServer items ->
      ( { model | items = items }
      , Effects.none)

    --SetVotes items2 ->
    --  ({ model | votes = items2}
    --    , Effects.none)


    SetAPIKey str ->
      ({ model | apiKey = str}
        , Effects.none)

   
--------------------------------------------------------------------------------

 --Pre-calculate some values derived from model
 --for more efficient view code

type alias AugModel = {
  itemList: List (Id, Item)
}
augment : Model -> AugModel
augment model =
  let
    itemList = model.items |> Dict.toList
  in
    {
      itemList = itemList
    }

--------------------------------------------------------------------------------

view : Address Action -> Model -> Html
view actionAddress model = 
  let 
    code = model.apiKey
    augmodel = augment model
    itemList = augmodel.itemList
    codeChecker (id, _) = 
      if id == code then
        True
      else
        False

    --filters input list for id that matches input code key -> code list will be list size 1 with the input key values as the item
    codeList = List.filter codeChecker itemList
    array = Array.fromList codeList
    tuple = Maybe.withDefault ("", {choice1 = "", choice2 = "", choice3 = "", choice4 = "", c1Votes = [], c2Votes = [], c3Votes = [], c4Votes = []}) (Array.get 0 array)
    item = Basics.snd tuple
    choice1 = item.choice1
    choice2 = item.choice2
    choice3 = item.choice3
    choice4 = item.choice4


    convertStr : Int -> String
    convertStr int = 
      case int of 
        0 -> "0"
        _ -> toString (int - 1)


    c1Votes = convertStr (List.length item.c1Votes)
    c2Votes = convertStr (List.length item.c2Votes)
    c3Votes = convertStr (List.length item.c3Votes)
    c4Votes = convertStr (List.length item.c4Votes)

  in
    div [] [
      fieldset []
        [ div [] [
            input
              ( [ placeholder "API KEY HERE"
                , autofocus True
                , on "input" targetValue (message actionAddress << SetAPIKey)
                ]
              )
              []
          ]
          , br [] []
        ]
      , br [] []
      , br [] []
      , fieldset []
        --, div [] [ text ("A) " ++ toString (List.length c1List))]
        [ div [] [ text ("A) " ++ choice1)]
        , div [] [ text ("# of Votes: " ++ c1Votes)]
        , br [] []
        , br [] []
        , div [] [ text ("B) " ++ choice2)]
        , div [] [ text ("# of Votes: " ++ c2Votes)]
        , br [] []
        , br [] []
        , div [] [ text ("C) " ++ choice3)]
        , div [] [ text ("# of Votes: " ++ c3Votes)]
        , br [] []
        , br [] []
        , div [] [ text ("D) " ++ choice4)]
        , div [] [ text ("# of Votes: " ++ c4Votes)]

      ]
    ]



customListFilter : String -> (String, b) -> Bool
customListFilter index (a , b) =
    if a == "C" ++ index then
      True
    else
      False

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
