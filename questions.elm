module Questions (..) where

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
import StartApp
import String
import ElmFire
import ElmFire.Dict
import ElmFire.Op


--------------------------------------------------------------------------------
-- Configuration
-- URL of the Firebase to use


firebase_foreign : String
firebase_foreign =
    "https://elmproj.firebaseio.com/Questions"



-- This app uses the same data format as the firebase-angular implementation.
-- So we could use also their Firebase for testing


firebase_test : String
firebase_test =
    "https://elmproj.firebaseio.com/Questions"


firebaseUrl : String
firebaseUrl =
    firebase_test



--------------------------------------------------------------------------------


config : StartApp.Config Model Action
config =
    { init = ( initialModel, initialEffect )
    , update = updateState
    , view = view
    , inputs = [ Signal.map FromServer inputItems ]
    }


app : StartApp.App Model
app =
    StartApp.start config


port runEffects : Signal (Task Never ())
port runEffects =
    app.tasks


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


main : Signal Html
main =
    app.html



--------------------------------------------------------------------------------
-- The model comprises two parts:
--   - Shared persistent state: A list of items together with their ids
--   - Local State: Filtering and editing


type alias Model =
    { items : Items
    , apiKey : Content
    , modQuestion : Content
    , modChoice1 : Content
    , modChoice2 : Content
    , modChoice3 : Content
    , modChoice4 : Content
    , modAnswerIndex : Int
    }


type alias Items =
    Dict Id Item


type alias Id =
    String


type alias Item =
    { question : Content
    , choice1 : Content
    , choice2 : Content
    , choice3 : Content
    , choice4 : Content
    , answerIndex : Content
    }


type alias Item2 =
    { cs152 : Content
    }


type alias Content =
    String


type Filter
    = All
    | Active
    | Completed


type alias EditingItem =
    Maybe ( Id, Content )


initialModel : Model
initialModel =
    { items = Dict.empty
    , apiKey = ""
    , modQuestion = ""
    , modChoice1 = ""
    , modChoice2 = ""
    , modChoice3 = ""
    , modChoice4 = ""
    , modAnswerIndex = 4
    }



-- initialTask : Task Error (Task Error ())
-- inputItems : Signal Items


( initialTask, inputItems ) =
    ElmFire.Dict.mirror syncConfig


initialEffect : Effects Action
initialEffect =
    initialTask |> kickOff



--------------------------------------------------------------------------------


syncConfig : ElmFire.Dict.Config Item
syncConfig =
    { location = ElmFire.fromUrl (firebaseUrl)
    , orderOptions = ElmFire.noOrder
    , encoder =
        \item ->
            JE.object
                [ ( "Question", JE.string item.question )
                , ( "Choice1", JE.string item.choice1 )
                , ( "Choice2", JE.string item.choice2 )
                , ( "Choice3", JE.string item.choice3 )
                , ( "Choice4", JE.string item.choice4 )
                , ( "AnswerIndex", JE.string item.answerIndex )
                ]
    , decoder =
        (JD.object6 Item
            ("Question" := JD.string)
            ("Choice1" := JD.string)
            ("Choice2" := JD.string)
            ("Choice3" := JD.string)
            ("Choice4" := JD.string)
            ("AnswerIndex" := JD.string)
        )
    }



--------------------------------------------------------------------------------


effectItems : ElmFire.Op.Operation Item -> Effects Action
effectItems operation =
    ElmFire.Op.operate
        syncConfig
        operation
        |> kickOff



--------------------------------------------------------------------------------


syncConfigDblString : String -> String -> ElmFire.Dict.Config Item2
syncConfigDblString apiKey choiceIndex =
    { location = ElmFire.fromUrl (firebaseUrl ++ "/" ++ apiKey ++ "/Votes/C" ++ choiceIndex)
    , orderOptions = ElmFire.noOrder
    , encoder =
        \item ->
            JE.object
                [ ( "cs152", JE.string item.cs152 )
                ]
    , decoder =
        (JD.object1 Item2
            ("cs152" := JD.string)
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


type Action
    = NoOp
    | FromServer Items
    | FromEffect
      -- no specific actions from effects here
    | SetAPIKey String
    | SetQuestion String
    | SetChoice1 String
    | SetChoice2 String
    | SetChoice3 String
    | SetChoice4 String
    | SetIndex Int String
    | SetCorrectAnswer Bool Int
    | AddParams
    | AddParamsChoices String



-- Process gui events and server events yielding model updates and effects


updateState : Action -> Model -> ( Model, Effects Action )
updateState action model =
    case action of
        NoOp ->
            ( model
            , Effects.none
            )

        FromEffect ->
            ( model
            , Effects.none
            )

        FromServer items ->
            ( { model | items = items }
            , Effects.none
            )

        SetAPIKey str ->
            ( { model | apiKey = str }
            , Effects.none
            )

        SetQuestion str ->
            ( { model | modQuestion = str }
            , Effects.none
            )

        SetChoice1 str ->
            ( { model | modChoice1 = str }
            , Effects.none
            )

        SetChoice2 str ->
            ( { model | modChoice2 = str }
            , Effects.none
            )

        SetChoice3 str ->
            ( { model | modChoice3 = str }
            , Effects.none
            )

        SetChoice4 str ->
            ( { model | modChoice4 = str }
            , Effects.none
            )

        SetIndex int ans ->
            case int of
                --Generate Updated Clone Copy of Model, this is Elm's equivalent to using a setter for an object
                1 ->
                    ( { model | modChoice1 = ans }, Effects.none )

                2 ->
                    ( { model | modChoice2 = ans }, Effects.none )

                3 ->
                    ( { model | modChoice3 = ans }, Effects.none )

                4 ->
                    ( { model | modChoice4 = ans }, Effects.none )

                _ ->
                    ( { model | modChoice1 = ans }, Effects.none )

        SetCorrectAnswer bool num ->
            case bool of
                --In 0.16, Elm didn't provide a library such that the listeners were chcked for input for us. So we have do check if the input worked ourselves.
                True ->
                    --If the action worked, change the model.
                    ( { model | modAnswerIndex = num }, Effects.none )

                False ->
                    --Otherwise, keep the model the same.
                    ( model, Effects.none )

        AddParams ->
            ( model
            , if model.modQuestion |> String.trim |> String.isEmpty then
                Effects.none
              else
                (effectItems
                    (ElmFire.Op.insert model.apiKey
                        { question = model.modQuestion
                        , choice1 = model.modChoice1
                        , choice2 = model.modChoice2
                        , choice3 = model.modChoice3
                        , choice4 = model.modChoice4
                        , answerIndex = toString (model.modAnswerIndex)
                        }
                    )
                )
            )

        AddParamsChoices indexStr ->
            ( model
            , effectItemsDblString model.apiKey indexStr (ElmFire.Op.push { cs152 = "cs152" })
            )


view : Address Action -> Model -> Html
view actionAddress model =
    div []
        [ fieldset []
            [ div []
                [ input
                    ([ placeholder "API KEY HERE"
                     , autofocus True
                     , on "input" targetValue (message actionAddress << SetAPIKey)
                     ]
                    )
                    []
                ]
            , br [] []
            , div [] [ text "Question ", input [ placeholder "Enter your question here.", on "input" targetValue (\str -> Signal.message actionAddress (SetQuestion str)) ] [] ]
            , br [] []
            , div [] [ text "Set one answer as the correct answer." ]
            , br [] []
            , question actionAddress "A)" 1
            , br [] []
            , question actionAddress "B)" 2
            , br [] []
            , question actionAddress "C)" 3
            , br [] []
            , question actionAddress "D)" 4
            , br [] []
            , button
                [ onClick proxy.address
                    [ ( actionAddress, AddParams )
                    , ( actionAddress, AddParamsChoices "1" )
                    , ( actionAddress, AddParamsChoices "2" )
                    , ( actionAddress, AddParamsChoices "3" )
                    , ( actionAddress, AddParamsChoices "4" )
                    ]
                ]
                [ text "Submit" ]
            , br [] []
            , br [] []
            ]
        , br [] []
        , br [] []
        , fieldset []
            [ div [] [ text model.modQuestion ]
            , div [] [ text ("A ) " ++ model.modChoice1) ]
            , div [] [ text ("B ) " ++ model.modChoice2) ]
            , div [] [ text ("C ) " ++ model.modChoice3) ]
            , div [] [ text ("D ) " ++ model.modChoice4) ]
            , div [] [ text ("The correct answer is " ++ (indexToLetter model.modAnswerIndex) ++ ".") ]
            , br [] []
            ]
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


question : Signal.Address Action -> String -> Int -> Html
question address textValue newAnswerIndex =
    div []
        [ --beginning of radio button
          label
            [ style [ ( "padding", "20px" ) ] ]
            [ input [ type' "radio", name "question", checked True, on "input" targetChecked (\bool -> Signal.message address (SetCorrectAnswer bool newAnswerIndex)) ] [], text textValue ]
          --ending of radio button
        , input [ placeholder "Enter your answer here.", on "input" targetValue (\str -> Signal.message address (SetIndex newAnswerIndex str)) ] []
        ]
