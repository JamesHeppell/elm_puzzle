-- Copyright 2021 James Heppell

module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style, attribute)
import Html.Events exposing (onClick)
import Url
import Dict
import List exposing (..)


-- MAIN/PROGRAM


main : Program Size Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

--UTIL 
puzzleRows : Int
puzzleRows = 3

puzzleColumns : Int
puzzleColumns = 3

inputgrid: List((Int,Int),State)
inputgrid = 
            [((0,0),Player)
            ,((0,1),Space)
            ,((0,2),Space)
            ,((1,0),Wall)
            ,((1,1),Space)
            ,((1,2),Space)
            ,((2,0),Space)
            ,((2,1),Goal)
            ,((2,2),Space)
            ]

-- inputgrid: List((Int,Int),State)
-- inputgrid = 
--             [((0,0),Player)
--             ,((0,1),Space)
--             ,((1,0),Wall)
--             ,((1,1),Goal)
--             ]

-- CELLS

type alias Index =
  (Int, Int)

type alias Cells = 
  Dict.Dict Index State

type State
    = Space
    | Player
    | Goal
    | Wall

stateToString: State -> String
stateToString state =
  case state of
     Space -> "Space"
     Player -> "Player"
     Goal -> "Goal"
     Wall -> "Wall"


-- MODEL

type alias Size = 
      {height : Int
      ,width : Int
      }

type alias Flags = Size

type alias Model =
  { gameMessage : String
    ,turnCounter : Int
    ,board : Cells
    ,playerPositionRow : Int
    ,playerPositionColumm : Int
    ,maxRows : Int
    ,maxCols : Int
    ,isGameFinished : Bool
    ,size : Size
  }


init : Flags -> ( Model, Cmd Msg )
init flags =
   ( initialModel flags, Cmd.none )


initialModel: Flags -> Model
initialModel flags = 
    Model "Navigate the maze..." 0 initialGrid 0 0 puzzleRows puzzleColumns False {width=flags.width, height=flags.height}


initialGrid : Cells
initialGrid =
  let
    dict = Dict.fromList inputgrid
  in 
    dict



getBoardCell : Index -> Cells -> State
getBoardCell idx board =
    Maybe.withDefault
        Space
        (Dict.get idx board)


changeCell : State -> Index -> Cells -> Cells
changeCell state idx board = 
  let
    newBoard = 
      Dict.insert
          idx
          state
          board
  in
    newBoard


convertBoardIntoLists : Cells -> List (List(State))
convertBoardIntoLists cells = 
  cells
  |> Dict.values
  |> split puzzleRows


split : Int -> List a -> List (List a)
split i list =
  case take i list of 
    [] -> []
    listHead -> listHead :: split i (drop i list)

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
      (initialModel model.size, Cmd.none)
    ButtonLeftClicked ->
      if isPlayerAtGoal model 0 -1 then
        ({model | isGameFinished = True
                  ,gameMessage = "You win!"}, Cmd.none)
      else
        if isValidMove model 0 -1 then 
            ( {model | turnCounter = model.turnCounter + 1
                      ,board = updateBoard model 0 -1
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
                      ,board = updateBoard model 0 1
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
                        ,board = updateBoard model -1 0
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
                      ,board = updateBoard model 1 0
                      ,playerPositionRow = model.playerPositionRow + 1
                      ,gameMessage = ""}
          , Cmd.none )
        else 
          ({model | gameMessage = "Not a valid move!"}, Cmd.none)
    _ ->
      ( model, Cmd.none )


updateBoard: Model -> Int -> Int -> Cells    
updateBoard model rowIncrease colIncrease =
  let
      --modify the move to then the move from
      newboard = changeCell Player (model.playerPositionRow + rowIncrease
                      ,model.playerPositionColumm + colIncrease
                      ) model.board 
  in
    changeCell Space (model.playerPositionRow
                      ,model.playerPositionColumm
                      ) newboard 


isPlayerAtGoal: Model -> Int -> Int -> Bool
isPlayerAtGoal model rowIncrease colIncrease =
    if getBoardCell (model.playerPositionRow + rowIncrease
                    ,model.playerPositionColumm + colIncrease) 
                    model.board == Goal then
      True
    else
      False

isValidMove: Model -> Int -> Int -> Bool
isValidMove model rowIncrease colIncrease = 
  isMoveInsideGameArea model rowIncrease colIncrease
  &&
  isMoveToEmptySquare model rowIncrease colIncrease

isMoveInsideGameArea: Model -> Int -> Int -> Bool
isMoveInsideGameArea model rowIncrease colIncrease = 
  if model.playerPositionColumm + colIncrease >= 0 &&
     model.playerPositionColumm + colIncrease < model.maxCols then
    if model.playerPositionRow + rowIncrease >= 0 &&
       model.playerPositionRow + rowIncrease < model.maxRows then
      True
    else 
      False
  else
    False

isMoveToEmptySquare: Model -> Int -> Int -> Bool
isMoveToEmptySquare model rowIncrease colIncrease =
  if getBoardCell (model.playerPositionRow + rowIncrease
                    ,model.playerPositionColumm + colIncrease) 
                    model.board /= Wall then
      True
    else
      False
    
  
-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW

cellSize : Size -> Int
cellSize size = 
  min size.height size.width // (puzzleRows * 3)

createTD: Size -> State -> Html Msg
createTD size state = 
    case state of
       Space -> td [grayBG, grayText, tdMaxHeight (cellSize size), tdMaxWidth (cellSize size)] [text("")]
       Player -> td [greenBG, greenText, tdMaxHeight (cellSize size), tdMaxWidth (cellSize size)] [text("")]
       Goal -> td [redBG, redText, tdMaxHeight (cellSize size), tdMaxWidth (cellSize size)] [text("")]
       Wall -> td [blackBG, blackText, tdMaxHeight (cellSize size), tdMaxWidth (cellSize size)] [text("")]


createTR : Size -> List(State) -> Html Msg
createTR size msgs =
    tr [] (List.map (createTD size) msgs)

createTable : Size -> List(List(State)) -> Html Msg
createTable size msgs = 
    table [style "margin" "0 auto"] (List.map (createTR size) msgs)

tdMaxWidth : Int -> Attribute Msg
tdMaxWidth width = style "width" (String.fromInt width ++ "px")

tdMaxHeight :Int -> Attribute Msg
tdMaxHeight height = style "height" (String.fromInt height ++ "px")

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
                 ,style "text-align" "center"
                 ]

buttonStyle : List(Attribute Msg)
buttonStyle = [style "width" "25px"
              ,style "height" "25px"
              ,style "margin" "auto 12px"
              ,blueBG]

blueBG :  Attribute Msg
blueBG = style "background-color" "rgb(135, 206, 250)"

gameStyle : List(Attribute Msg)
gameStyle = [style "padding" "20px"]

viewPuzzle : Model -> Html Msg
viewPuzzle model = 
            createTable model.size (convertBoardIntoLists model.board)             
                     

viewResetButton : Model -> Html Msg
viewResetButton model = 
              if model.isGameFinished then
                div divButtonStyle [button ([blueBG,  onClick ResetPuzzle]) [text "Restart"]]
              else 
                text ""
                
viewButtons : Html Msg
viewButtons = 
    div [] [
                div divButtonStyle [button (buttonStyle ++ [ onClick ButtonUpClicked ]) [ text "↑" ]]
                ,div divButtonStyle [
                          button (buttonStyle ++ [ onClick ButtonLeftClicked ]) [ text "←" ]
                         ,button (buttonStyle ++ [ onClick ButtonRightClicked ]) [ text "→" ]
                         ]
                ,div divButtonStyle [button (buttonStyle ++ [ onClick ButtonDownClicked ]) [ text "↓" ]]]


viewGameAndText : Model -> Html Msg
viewGameAndText model = 
        if model.isGameFinished then
          text ""
        else
          div [] [
          div divStyle [ p [alignTextCentre] [text "Get the "
                                              ,span [greenText] [text "GREEN"]
                                              ,text " square to the "
                                              ,span [redText] [text "RED"]
                                              ,text " square"]]
          , viewButtons
          , div (gameStyle) [viewPuzzle model]
          , div divStyle [p [alignTextCentre] [text("Moves " ++ String.fromInt model.turnCounter)]]
          --debug, remove once working
          -- , div divStyle [text("Player Position (row,col) " ++ 
          --                 String.fromInt model.playerPositionRow ++ " " ++
          --                 String.fromInt model.playerPositionColumm)]
          ----------------------------
          ]

view : Model -> Html Msg
view model =
      div []
      [
        div titleStyle [h1 [alignTextCentre] [text("Puzzles")]]
        ,
        viewGameAndText model
        ,
        div divStyle [p [alignTextCentre] [text(model.gameMessage)]]
        , viewResetButton model
        
      ]
      