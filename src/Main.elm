-- Copyright 2021 James Heppell

module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Url



-- MAIN/PROGRAM


main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }


--UTIL

puzzleRows : Int
puzzleRows = 3

puzzleColumns : Int
puzzleColumns = 5

-- MODEL


type alias Model =
  { helloMessage : String
    ,buttonClicks : Int
  }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags _ _ =
   ( Model "Hello World " 0, Cmd.none )



-- UPDATE

type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | ButtonLeftClicked
  | ButtonRightClicked 
  | ButtonUpClicked
  | ButtonDownClicked


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ButtonLeftClicked ->
      ( {model | buttonClicks = model.buttonClicks + 1}, Cmd.none )

    _ ->
      ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW

createTD : Attribute Msg -> Html Msg
createTD msg = 
    td [msg] [text("   ")]

createTR : List(Attribute Msg) -> Html Msg
createTR msgs =
    tr [] (List.map createTD msgs)

createTable : List(List(Attribute Msg)) -> Html Msg
createTable msgs = 
    table [] (List.map createTR msgs)

greenBG : Attribute Msg
greenBG = style "background-color" "rgb(26, 148, 49)"

grayBG :  Attribute Msg
grayBG = style "background-color" "rgb(192, 192, 192)"

redBG :  Attribute Msg
redBG = style "background-color" "rgb(255, 0, 0)"

viewPuzzle : Model -> Html Msg
viewPuzzle model = 
            createTable [[greenBG,grayBG],[grayBG,redBG],[grayBG,redBG]]                    
                     
                

view : Model -> Browser.Document Msg
view model =
  { title = "My test Page"
  , body = 
      [ div [] [text("Puzzles")]
      , div [] [text("Get the GREEN Square to the RED Square")]
      , div [] [button [ onClick ButtonLeftClicked ] [ text "Left" ],
                button [ onClick ButtonRightClicked ] [ text "Right" ],
                button [ onClick ButtonUpClicked ] [ text "Up" ],
                button [ onClick ButtonDownClicked ] [ text "Down" ]]
      , viewPuzzle model
      , div [] [ text (model.helloMessage ++ String.fromInt model.buttonClicks) ]
      ]
  }