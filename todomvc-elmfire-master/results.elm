module Questions where

{-| TodoMVC implemented in Elm
    using Firebase for storage

    2015 Thomas Weiser
         based on work by Evan Czaplicki and the TodoMVC project

    - [Github Repo](https://github.com/ThomasWeiser/todomvc-elmfire)
    - [Original Elm Implementation by Evan Czaplicki](https://github.com/evancz/elm-todomvc)
    - [ElmFire](https://github.com/ThomasWeiser/elmfire)
    - [Elm Language](http://elm-lang.org/)
    - [TodoMVC Project](http://todomvc.com/)
-}

import Result
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Json.Decode as JD exposing ((:=))
import Json.Encode as JE
import Signal exposing (Mailbox, Address, mailbox, message)
import Task exposing (Task, andThen)
import Effects exposing (Effects, Never)
import Array exposing (..)
import Basics exposing (..)
import StartApp
import String

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

-- But lets use our own
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

-- The model comprises two parts:
--   - Shared persistent state: A list of items together with their ids
--   - Local State: Filtering and editing

type alias Model =
  { items: Items
  , filter: Filter
  , addField: Content
  , editingItem: EditingItem
  , apiKey: Content
  }

type alias Items = Dict Id Item

type alias Id = String
type alias Item =
  { question: Content
  , choice1: Content
  , choice2: Content
  , choice3: Content
  , choice4: Content
  , votes: Content
  }

type alias Content = String

type Filter = All | Active | Completed

type alias EditingItem = Maybe (Id, Content)

initialModel : Model
initialModel =
  { items = Dict.empty
  , filter = All
  , addField = ""
  , editingItem = Nothing
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

syncConfig : ElmFire.Dict.Config Item
syncConfig =
  { location = ElmFire.fromUrl (firebaseUrl)
  , orderOptions = ElmFire.noOrder
  , encoder =
      \item -> JE.object
        [ ("Question", JE.string item.question)
        , ("Choice1", JE.string item.choice1)
        , ("Choice2", JE.string item.choice2)
        , ("Choice3", JE.string item.choice3)
        , ("Choice4", JE.string item.choice4)
        , ("Votes", JE.object item.votes)
        ]
  , decoder =
      ( JD.object6 Item
          ("Question" := JD.string)
          ("Choice1" := JD.string)
          ("Choice2" := JD.string)
          ("Choice3" := JD.string)
          ("Choice4" := JD.string)
          ("Votes" := JD.object)
      )
  }

--syncConfigVotes : String -> String -> ElmFire.Dict.Config Item2
--syncConfigVotes apiKey choiceIndex =
--  { location = ElmFire.fromUrl ("https://elmproj.firebaseio.com/Questions/")
--  , orderOptions = ElmFire.noOrder
--  , encoder =
--      \item2 -> JE.object
--        [ ("Sixtynine", JE.string item2.sixtynine)
--        ]
--  , decoder =
--      ( JD.object1 Item2
--          ("Sixtynine" := JD.string)
--      )
--  }
 --------------------------------------------------------------------------------
-- Map any task to an effect, discarding any direct result or error value
kickOff : Task x a -> Effects Action
kickOff =
  Task.toMaybe >> Task.map (always (FromEffect)) >> Effects.task

--------------------------------------------------------------------------------

type Action =
  --= FromGui GuiEvent
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
    tuple = Maybe.withDefault ("", {question = "", choice1 = "", choice2 = "", choice3 = "", choice4 = ""}) (Array.get 0 array)
    item = Basics.snd tuple
    question = item.question
    choice1 = item.choice1
    choice2 = item.choice2
    choice3 = item.choice3
    choice4 = item.choice4








    --(initialTasks2, inputs) = ElmFire.Dict.mirror (syncConfigVotes code "1")

    --signal = Signal.map SetVotes inputs
    --actn = VoteKickOff initialTasks2

    --c1List = model.votes |> Dict.toList
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
       [ div []
          [ label [] [ text "Question: ", output [] [ text (question) ] ]
          , br [] []
          , br [] []
          ]
        --, div [] [ text ("A) " ++ toString (List.length c1List))]
        , br [] []
        , br [] []
        , div [] [ text ("codelist) " ++ toString (List.length codeList))]
        , br [] []
        , br [] []
        , div [] [ text ("itemlist) " ++ toString (List.length itemList))]
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
