module Snake
    exposing
        ( Msg
        , Model
        , initialModel
        , subscriptions
        , view
        , update
        )

import Html exposing (..)
import Html.Attributes as H
import Html.Events exposing (onClick)
import Svg exposing (Svg, svg, circle, rect, g, pattern, defs)
import Svg.Attributes as S
import Time
import Keyboard
import Random
import Score


type alias Model =
    { score : Int
    , highScore : Int
    , grid : Grid
    , snake : Snake
    , food : Position
    , direction : Direction
    , speed : Float
    , state : State
    }


initialModel : Model
initialModel =
    { score = 0
    , highScore = 0
    , grid = Grid 20 20 30
    , snake = [ Position 1 1 ]
    , food = Position 8 8
    , direction = Right
    , speed = 150
    , state = Start
    }


type Msg
    = Tick Time.Time
    | KeyPress Int
    | Food Position
    | StartGame
    | HighScore Int


type State
    = Start
    | Play
    | GameOver


type alias Snake =
    List Position


type alias Grid =
    { rows : Int
    , columns : Int
    , size : Int
    }


type alias Position =
    { x : Int
    , y : Int
    }


type Direction
    = Up
    | Down
    | Left
    | Right


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            updateGame model

        KeyPress char ->
            handleKeyPress char model ! []

        StartGame ->
            { initialModel | state = Play } ! []

        Food position ->
            if List.member position model.snake then
                ( model, Random.generate Food (randomPosition model.grid) )
            else
                { model | food = position } ! []

        HighScore score ->
            { model | highScore = score } ! []


handleKeyPress : Int -> Model -> Model
handleKeyPress char model =
    case char of
        -- Left Arrow
        37 ->
            if model.direction /= Right then
                { model | direction = Left }
            else
                model

        -- Right Arrow
        39 ->
            if model.direction /= Left then
                { model | direction = Right }
            else
                model

        -- Up Arrow
        40 ->
            if model.direction /= Down then
                { model | direction = Up }
            else
                model

        -- Down Arrow
        38 ->
            if model.direction /= Up then
                { model | direction = Down }
            else
                model

        _ ->
            model


updateGame : Model -> ( Model, Cmd Msg )
updateGame model =
    let
        head =
            case List.head model.snake of
                Just pos ->
                    updatePosition model.direction pos

                Nothing ->
                    Position 29 29

        tail =
            if head == model.food then
                model.snake
            else
                List.take (List.length model.snake - 1) model.snake

        snake =
            head :: tail
    in
        if wallCollision head model.grid || tailCollision head tail then
            ( { model | state = GameOver }, Score.sendScore model.score )
        else if head == model.food then
            ( { model | snake = snake, score = model.score + 1 }
            , Random.generate Food (randomPosition model.grid)
            )
        else
            { model | snake = snake } ! []


tailCollision : Position -> List Position -> Bool
tailCollision position ls =
    List.any ((==) position) ls


wallCollision : Position -> Grid -> Bool
wallCollision { x, y } grid =
    x < 0 || x >= grid.columns || y < 0 || y >= grid.columns


updatePosition : Direction -> Position -> Position
updatePosition dir pos =
    case dir of
        Up ->
            { pos | y = pos.y + 1 }

        Down ->
            { pos | y = pos.y - 1 }

        Right ->
            { pos | x = pos.x + 1 }

        Left ->
            { pos | x = pos.x - 1 }


randomPosition : Grid -> Random.Generator Position
randomPosition { columns, rows } =
    Random.map2
        Position
        (Random.int 1 <| columns - 2)
        (Random.int 1 <| rows - 2)


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        Play ->
            Sub.batch
                [ Time.every (model.speed * Time.millisecond) Tick
                , Keyboard.downs (\keycode -> KeyPress keycode)
                , Score.getScore HighScore
                ]

        _ ->
            Sub.batch
                [ Keyboard.downs (\keycode -> KeyPress keycode)
                , Score.getScore HighScore
                ]


view : Model -> Html Msg
view model =
    viewGame model


viewGame : Model -> Html Msg
viewGame model =
    let
        width =
            toString (model.grid.columns * model.grid.size)

        height =
            toString (model.grid.rows * model.grid.size)
    in
        div
            [ H.class "game" ]
            [ svg
                [ S.width width
                , S.height height
                , S.viewBox <| "0 0 " ++ width ++ " " ++ height
                ]
                [ viewBackground width height
                , viewFood model
                , viewSnake model
                ]
            , viewStart model
            , viewGameOver model
            ]


viewGameOver : Model -> Html Msg
viewGameOver { state, score, highScore } =
    if state == GameOver then
        div [ H.class "gameover" ]
            [ h1 [ H.class "gameover__title" ]
                [ text "Game Over" ]
            , Score.view score highScore
            , button
                [ H.class "button gameover__button", onClick StartGame ]
                [ text "Play Again" ]
            ]
    else
        text ""


viewStart : Model -> Html Msg
viewStart { state } =
    if state == Start then
        div [ H.class "start" ]
            [ button
                [ H.class "button start__button", onClick StartGame ]
                [ text "Start Game" ]
            ]
    else
        text ""


viewFood : Model -> Svg Msg
viewFood { food, grid, state } =
    let
        radius =
            (toFloat grid.size) / 2
    in
        if state == Play then
            circle
                [ S.r <| toString radius
                , S.fill "#D2FB78"
                , S.cx <| toString <| food.x * grid.size + (round radius)
                , S.cy <| toString <| food.y * grid.size + (round radius)
                ]
                []
        else
            Svg.text ""


checkerboard : Svg Msg
checkerboard =
    let
        color1 =
            "#49496A"

        color2 =
            "#42425F"
    in
        pattern
            [ S.id "checkerboard"
            , S.x "0"
            , S.y "0"
            , S.width "60"
            , S.height "60"
            , S.patternUnits "userSpaceOnUse"
            ]
            [ rect
                [ S.x "0"
                , S.y "0"
                , S.width "30"
                , S.height "30"
                , S.fill color1
                ]
                []
            , rect
                [ S.x "30"
                , S.y "0"
                , S.width "30"
                , S.height "30"
                , S.fill color2
                ]
                []
            , rect
                [ S.x "30"
                , S.y "30"
                , S.width "30"
                , S.height "30"
                , S.fill color1
                ]
                []
            , rect
                [ S.x "0"
                , S.y "30"
                , S.width "30"
                , S.height "30"
                , S.fill color2
                ]
                []
            ]


viewBackground : String -> String -> Svg Msg
viewBackground width height =
    g []
        [ defs
            []
            [ checkerboard ]
        , rect
            [ S.width width
            , S.height height
            , S.fill "url(#checkerboard)"
            ]
            []
        ]


viewSnake : Model -> Svg Msg
viewSnake { snake, grid, state } =
    let
        viewRect { x, y } =
            rect
                [ S.x <| toString <| x * grid.size
                , S.y <| toString <| y * grid.size
                , S.width <| toString grid.size
                , S.height <| toString grid.size
                , S.fill "#49CDF6"
                ]
                []
    in
        if state == Play then
            g []
                (List.map viewRect snake)
        else
            Svg.text ""


main : Program Never Model Msg
main =
    program
        { init = ( initialModel, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
