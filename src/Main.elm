module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, text, node, ul, li)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Parser exposing (run, DeadEnd, Problem (..))
import PiParser as PP
import VM 
import Set as S
import Dict as D

-- main                               
main = Browser.sandbox { init = init
                       , update = update
                       , view = view
                       }
         
-- Model
type alias Model =
    { inputString : String
    , result : String
    , errors : List DeadEnd
    }

init : Model
init =
    { inputString = "0"
    , result = ""
    , errors = []
    }

-- Update

type Msg
    = Change String
    | Eval String

update : Msg -> Model -> Model
update msg model =
    case msg of
        Eval str -> case run PP.parser str of
                        Ok term -> { model | errors = [], result = VM.show <| VM.normalize <| VM.lit2PFN term }
                        Err err -> { model | errors = err, result = "error" }
                    
        Change str ->
            { model | inputString = str }

-- View

css path =
    node "link" [rel "stylesheet", href path ] []

view : Model -> Html Msg
view model =
    div [ class "interpreter" ]
        [ node "link"
              [rel "stylesheet"
              , href "https://fonts.googleapis.com/css2?family=Inconsolata:wght@300&display=swap"
              ] []
        , css "style.css"
        , div [ class "console" ]
            [ input [ class "reader"
                    , placeholder "input lambda expression \u{23CE}"
                    , value model.inputString, onInput Change ] []
            , button [ class "submitter"
                     , onClick <| Eval model.inputString ] [ text "run" ]
            , div [] [ text model.result ]
            ]
        ]
    
