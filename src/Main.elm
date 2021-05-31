-- Copyright 2021 James Heppell

module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Url
import Platform.Cmd exposing (none)



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

initialGoalPosition : Coordinate
initialGoalPosition = {row=2, col=1}

-- MODEL


type alias Model =
  { gameMessage : String
    ,turnCounter : Int
    ,gameBoard : List(List(Int))
    ,playerPositionRow : Int
    ,playerPositionColumm : Int
    ,maxRows : Int
    ,maxCols : Int
    ,goalPosition : Coordinate
    ,isGameFinished : Bool
  }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags _ _ =
   ( initialModel, Cmd.none )


initialModel: Model
initialModel = Model "Navigate the maze..." 0 [[1,0],[0,3],[0,2]] 0 0 puzzleRows puzzleColumns initialGoalPosition False

type alias Coordinate = 
    { row : Int
    , col : Int
    }

-- UPDATE

type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | ButtonLeftClicked
  | ButtonRightClicked 
  | ButtonUpClicked
  | ButtonDownClicked
  | ResetPuzzle


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ResetPuzzle ->
      (initialModel, Cmd.none)
    ButtonLeftClicked ->
      if isPlayerAtGoal model 0 -1 then
        ({model | isGameFinished = True
                  ,gameMessage = "You win!"}, Cmd.none)
      else
        if isValidMove model 0 -1 then 
            ( {model | turnCounter = model.turnCounter + 1
                      ,playerPositionColumm = model.playerPositionColumm - 1
                      ,gameMessage = ""}
          , Cmd.none )
        else 
          ({model | gameMessage = "Not a valid move!"}, Cmd.none)
    ButtonRightClicked ->
      if isPlayerAtGoal model 0 1 then
        ({model | isGameFinished = True
                  ,gameMessage = "You win!"}, Cmd.none)
      else
        if isValidMove model 0 1 then 
            ( {model | turnCounter = model.turnCounter + 1
                      ,playerPositionColumm = model.playerPositionColumm + 1
                      ,gameMessage = ""}
          , Cmd.none )
        else 
          ({model | gameMessage = "Not a valid move!"}, Cmd.none)
    ButtonUpClicked ->
      if isPlayerAtGoal model -1 0 then
        ({model | isGameFinished = True
                  ,gameMessage = "You win!"}, Cmd.none)
      else
        if isValidMove model -1 0 then 
              ( {model | turnCounter = model.turnCounter + 1
                        ,playerPositionRow = model.playerPositionRow - 1
                        ,gameMessage = ""}
            , Cmd.none )
          else 
            ({model | gameMessage = "Not a valid move!"}, Cmd.none)
    ButtonDownClicked ->
      if isPlayerAtGoal model 1 0 then
        ({model | isGameFinished = True
                  ,gameMessage = "You win!"}, Cmd.none)
      else
        if isValidMove model 1 0 then 
            ( {model | turnCounter = model.turnCounter + 1
                      ,playerPositionRow = model.playerPositionRow + 1
                      ,gameMessage = ""}
          , Cmd.none )
        else 
          ({model | gameMessage = "Not a valid move!"}, Cmd.none)
    _ ->
      ( model, Cmd.none )

isPlayerAtGoal: Model -> Int -> Int -> Bool
isPlayerAtGoal model rowIncrease colIncrease =
    if model.playerPositionColumm + colIncrease == model.goalPosition.col &&
       model.playerPositionRow + rowIncrease == model.goalPosition.row then
      True
    else
      False

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
    table [style "margin" "0 auto"] (List.map createTR msgs)

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

alignTextCentre: Attribute Msg
alignTextCentre = style "text-align" "center"
titleStyle : List (Attribute Msg)
titleStyle = [style "margin" "auto"
             ,style "padding" "10px"
             ,style "width" "60%"
             ]

divStyle : List (Attribute Msg)
divStyle = [style "margin" "auto"
           ,style "padding" "5px"
           ,style "width" "90%"
           ]


divButtonStyle: List (Attribute Msg)
divButtonStyle = [style "display" "flex"
                 ,style "justify-content" "center"
                 ,style "align-items" "center"
                 ]

buttonStyle : List(Attribute Msg)
buttonStyle = [style "padding" "10px"
              ,style "margin" "0 10px"
              ,style "background-color" "rgb(135, 206, 250)"]

gameStyle : List(Attribute Msg)
gameStyle = [style "padding" "20px"]

viewPuzzle : Model -> Html Msg
viewPuzzle model = 
            createTable model.gameBoard                  
                     

viewResetButton : Model -> Html Msg
viewResetButton model = 
              if model.isGameFinished then
                div divButtonStyle [button (buttonStyle ++ [onClick ResetPuzzle]) [text "Restart"]]
              else 
                text ""
                

viewGameAndText : Model -> List(Html Msg)
viewGameAndText model = 
        if model.isGameFinished then
          [text ""]
        else
          [div divStyle [ p [alignTextCentre] [text("Get the GREEN Square to the RED Square")]]
          , div divButtonStyle [button (buttonStyle ++ [ onClick ButtonLeftClicked ]) [ text "Left" ],
                button (buttonStyle ++ [ onClick ButtonRightClicked ]) [ text "Right" ],
                button (buttonStyle ++ [ onClick ButtonUpClicked ]) [ text "Up" ],
                button (buttonStyle ++ [ onClick ButtonDownClicked ]) [ text "Down" ]]
          , div (gameStyle) [viewPuzzle model]
          , div divStyle [p [alignTextCentre] [text("Moves " ++ String.fromInt model.turnCounter)]]
          --debug, remove once working
          , div divStyle [text("Player Position (row,col) " ++ 
                          String.fromInt model.playerPositionRow ++ " " ++
                          String.fromInt model.playerPositionColumm)]
          ----------------------------
          ]

view : Model -> Browser.Document Msg
view model =
  { title = "My Puzzles"
  , body = 
      [div titleStyle [h1 [alignTextCentre] [text("Puzzles")]]]
      ++
      viewGameAndText model
      ++
      [div divStyle [p [alignTextCentre] [text(model.gameMessage)]]
      , viewResetButton model
      ]
  }