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
  , modAnswerIndex: Int
  }

type alias Items = Dict Id Item
type alias Id = String
type alias Item =
  { question: Content
  , choice1: Content
  , choice2: Content
  , choice3: Content
  , choice4: Content
  }

type alias Item2 =
  { sixtynine: Content
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
  , modAnswerIndex = 0
  }


--------------------------------------------------------------------------------

-- Events originating from the user interacting with the html page

--type GuiEvent
--  = NoGuiEvent
--    -- operations on the item list
--  | AddItem
--  | UpdateItem Id
--  | DeleteItem Id
--  | DeleteCompletedItems
--  | CheckItem Id Bool
--  | CheckAllItems Bool
--    -- operating on local state
--  | EditExistingItem EditingItem
--  | EditAddField Content
--  | SetFilter Filter

--type alias GuiAddress = Address GuiEvent

--------------------------------------------------------------------------------

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
        ]
  , decoder =
      ( JD.object5 Item
          ("Question" := JD.string)
          ("Choice1" := JD.string)
          ("Choice2" := JD.string)
          ("Choice3" := JD.string)
          ("Choice4" := JD.string)
      )
  }

effectItems : ElmFire.Op.Operation Item -> Effects Action
effectItems operation =
  ElmFire.Op.operate
    syncConfig
    operation
  |> kickOff
--------------------------------------------------------------------------------
syncConfigString : String -> ElmFire.Dict.Config Item
syncConfigString apiKey =
  { location = ElmFire.fromUrl (firebaseUrl ++ "/" ++ apiKey)
  , orderOptions = ElmFire.noOrder
  , encoder =
      \item -> JE.object
        [ ("Question", JE.string item.question)
        , ("Choice1", JE.string item.choice1)
        , ("Choice2", JE.string item.choice2)
        , ("Choice3", JE.string item.choice3)
        , ("Choice4", JE.string item.choice4)
        ]
  , decoder =
      ( JD.object5 Item
          ("Question" := JD.string)
          ("Choice1" := JD.string)
          ("Choice2" := JD.string)
          ("Choice3" := JD.string)
          ("Choice4" := JD.string)
      )
  }


effectItemsString : String -> ElmFire.Op.Operation Item -> Effects Action
effectItemsString str operation =
  ElmFire.Op.operate
    (syncConfigString str)
    operation
  |> kickOff

--------------------------------------------------------------------------------
syncConfigDblString : String -> String -> ElmFire.Dict.Config Item2
syncConfigDblString apiKey choiceIndex =
  { location = ElmFire.fromUrl (firebaseUrl ++ "/" ++ apiKey ++ "/Votes/C" ++ choiceIndex)
  , orderOptions = ElmFire.noOrder
  , encoder =
      \item -> JE.object
        [ ("Sixtynine", JE.string item.sixtynine)
        ]
  , decoder =
      ( JD.object1 Item2
          ("Sixtynine" := JD.string)
      )
  }

effectItemsDblString : String -> String -> ElmFire.Op.Operation Item2 -> Effects Action
effectItemsDblString str str2 operation =
  ElmFire.Op.operate
    (syncConfigDblString str str2)
    operation
  |> kickOff

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
  | SetTempAnswer Bool Int
  | Vote
  | ClearVotes
  | EditVote


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
      , Effects.none
      )

    SetAPIKey str ->
      ({ model | apiKey = str}
        , Effects.none)

    SetTempAnswer bool num ->
      case bool of
          --In 0.16, Elm didn't provide a library such that the listeners were chcked for input for us. So we have do check if the input worked ourselves.
          True ->
              --If the action worked, change the model.
              ( { model | modAnswerIndex = num }, Effects.none )
          False ->
              ( model, Effects.none )

    Vote ->
      let 
        indexStr = toString model.modAnswerIndex
      in
        ( model, effectItemsDblString model.apiKey indexStr (ElmFire.Op.push {sixtynine = "Sixtynine"})
        )

    ClearVotes -> (model, effectItemsString model.apiKey <| ElmFire.Op.remove "Votes")

    EditVote -> 
      ( model
      , case model.editingItem of
          Just (id1, title) ->
            if (id == id1)
            then
              if title |> String.trim |> String.isEmpty
              then
                effectItems <| ElmFire.Op.remove id
              else
                effectItems <| ElmFire.Op.update id
                  ( Maybe.map
                      (\item -> { item | title = title |> String.trim })
                  )
            else Effects.none
          Nothing -> Effects.none



   
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

    --filters input list for id that matches input code key
    singleList = List.filter codeChecker itemList
    array = Array.fromList singleList
    tuple = Maybe.withDefault ("", {question = "", choice1 = "", choice2 = "", choice3 = "", choice4 = ""}) (Array.get 0 array)
    item = Basics.snd tuple
    question = item.question
    choice1 = item.choice1
    choice2 = item.choice2
    choice3 = item.choice3
    choice4 = item.choice4
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
        , questionMaker actionAddress choice1 1
        , br [] []
        , br [] [] 
        , questionMaker actionAddress choice2 2
        , br [] []
        , br [] [] 
        , questionMaker actionAddress choice3 3
        , br [] []
        , br [] [] 
        , questionMaker actionAddress choice4 4
        , br [] []
        , br [] [] 
        , button [ onClick actionAddress Vote][text "Vote"]
        , button [ onClick actionAddress ClearVotes][text "Clear All Votes"]
      ]
    ]

questionMaker : Signal.Address Action -> String -> Int -> Html
questionMaker address textValue index =
    div []
        [ --beginning of radio button
          label
            [ style [ ( "padding", "20px" ) ] ]
            [ input [ type' "radio", name "question", checked True, on "input" targetChecked (\bool -> Signal.message address (SetTempAnswer bool index)) ] [], text ((indexToLetter index) ++ ")") ]
          --ending of radio button
        , text (textValue)
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
