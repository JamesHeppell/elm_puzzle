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
  | ButtonClicked 


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ButtonClicked ->
      ( {model | buttonClicks = model.buttonClicks + 1}, Cmd.none )

    _ ->
      ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
  { title = "My test Page"
  , body = 
      [ button [ onClick ButtonClicked ] [ text "Click Me!" ]
      , div [] [ text (model.helloMessage ++ String.fromInt model.buttonClicks) ]
      ]
  }