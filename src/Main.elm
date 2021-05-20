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
puzzleColumns = 2

-- MODEL


type alias Model =
  { helloMessage : String
    ,turnCounter : Int
    ,gameBoard : List(List(Int))
    ,playerPositionRow : Int
    ,playerPositionColumm : Int
    ,maxRows: Int
    ,maxCols: Int
  }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags _ _ =
   ( Model "Hello World " 0 [[1,0],[0,3],[0,2]] 0 0 puzzleRows puzzleColumns, Cmd.none )



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
      if isValidMove model 0 -1 then 
          ( {model | turnCounter = model.turnCounter + 1
                    ,playerPositionColumm = model.playerPositionColumm - 1}
        , Cmd.none )
      else 
        (model, Cmd.none)
    ButtonRightClicked ->
      if isValidMove model 0 1 then 
          ( {model | turnCounter = model.turnCounter + 1
                    ,playerPositionColumm = model.playerPositionColumm + 1}
        , Cmd.none )
      else 
        (model, Cmd.none)
    ButtonUpClicked ->
     if isValidMove model -1 0 then 
          ( {model | turnCounter = model.turnCounter + 1
                    ,playerPositionRow = model.playerPositionRow - 1}
        , Cmd.none )
      else 
        (model, Cmd.none)
    ButtonDownClicked ->
      if isValidMove model 1 0 then 
          ( {model | turnCounter = model.turnCounter + 1
                    ,playerPositionRow = model.playerPositionRow + 1}
        , Cmd.none )
      else 
        (model, Cmd.none)
    _ ->
      ( model, Cmd.none )



isValidMove: Model -> Int -> Int -> Bool
isValidMove model rowIncrease colIncrease = 
  if model.playerPositionColumm + colIncrease >= 0 &&
     model.playerPositionColumm + colIncrease < model.maxCols then
    if model.playerPositionRow + rowIncrease >= 0 &&
       model.playerPositionRow + rowIncrease < model.maxRows then
      True
    else 
      False
  else
    False

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW

createTD: Int -> Html Msg
createTD int = 
    case int of
       0 -> td [grayBG, grayText] [text("A")]
       1 -> td [greenBG, greenText] [text("A")]
       2 -> td [redBG, redText] [text("A")]
       3 -> td [blackBG, blackText] [text("A")]
       _ -> td [grayBG, grayText] [text("A")]


createTR : List(Int) -> Html Msg
createTR msgs =
    tr [] (List.map createTD msgs)

createTable : List(List(Int)) -> Html Msg
createTable msgs = 
    table [] (List.map createTR msgs)

greenBG : Attribute Msg
greenBG = style "background-color" "rgb(26, 148, 49)"
greenText : Attribute Msg
greenText = style "color" "rgb(26, 148, 49)"

grayBG :  Attribute Msg
grayBG = style "background-color" "rgb(192, 192, 192)"
grayText : Attribute Msg
grayText = style "color" "rgb(192, 192, 192)"

redBG :  Attribute Msg
redBG = style "background-color" "rgb(255, 0, 0)"
redText : Attribute Msg
redText = style "color" "rgb(255, 0, 0)"

blackBG :  Attribute Msg
blackBG = style "background-color" "rgb(0, 0, 0)"
blackText : Attribute Msg
blackText = style "color" "rgb(0, 0, 0)"


viewPuzzle : Model -> Html Msg
viewPuzzle model = 
            createTable model.gameBoard                  
                     
                

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
      , div [] [text("Moves " ++ String.fromInt model.turnCounter)]
      , div [] [text("Player Position (row,col) " ++ 
                      String.fromInt model.playerPositionRow ++ " " ++
                      String.fromInt model.playerPositionColumm)]
      ]
  }