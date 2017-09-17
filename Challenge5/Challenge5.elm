module Challenge5 exposing (..)

import Html exposing (..)
import Html.Attributes as H
import Svg exposing (Svg, svg, circle, rect, g, pattern, defs)
import Svg.Attributes as S
import Time
import Keyboard
import Random


type alias Model =
    { score : Int
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
    , grid = Grid 20 20 30
    , snake = [ Position 1 1 ]
    , food = Position 8 8
    , direction = Right
    , speed = 150
    , state = Pause
    }


type Msg
    = Tick Time.Time
    | KeyPress Int
    | Food Position


type State
    = Pause
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

        Food position ->
            if List.member position model.snake then
                ( model, Random.generate Food (randomPosition model.grid) )
            else
                { model | food = position } ! []


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

        -- Space
        32 ->
            let
                newState =
                    if model.state == Pause then
                        Play
                    else
                        Pause
            in
                { model | state = newState }

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
            { model | state = GameOver } ! []
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
                ]

        _ ->
            Keyboard.downs (\keycode -> KeyPress keycode)


view : Model -> Html Msg
view model =
    div
        [ H.style containerStyle ]
        [ viewGame model ]


containerStyle : List ( String, String )
containerStyle =
    [ ( "display", "flex" )
    , ( "justify-content", "center" )
    , ( "align-items", "center" )
    , ( "height", "100%" )
    ]


viewGame : Model -> Html Msg
viewGame model =
    let
        width =
            toString (model.grid.columns * model.grid.size)

        height =
            toString (model.grid.rows * model.grid.size)
    in
        div
            [ H.class "game", H.style [ ( "position", "relative" ) ] ]
            [ svg
                [ S.width width
                , S.height height
                , S.viewBox <| "0 0 " ++ width ++ " " ++ height
                ]
                [ viewBackground width height
                , viewFood model
                , viewSnake model.snake model.grid.size
                ]
            , viewScore model
            ]


viewScore : Model -> Html Msg
viewScore { score, state } =
    if state == GameOver then
        div [ H.style scoreStyle ]
            [ text <| toString score ]
    else
        text ""


scoreStyle : List ( String, String )
scoreStyle =
    [ ( "position", "absolute" )
    , ( "left", "10px" )
    , ( "top", "10px" )
    , ( "font", "21px Arial, sans-serif" )
    ]


viewFood : Model -> Svg Msg
viewFood { food, grid } =
    let
        radius =
            (toFloat grid.size) / 2
    in
        circle
            [ S.r <| toString radius
            , S.fill "#D2FB78"
            , S.cx <| toString <| food.x * grid.size + (round radius)
            , S.cy <| toString <| food.y * grid.size + (round radius)
            ]
            []


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


viewSnake : Snake -> Int -> Svg Msg
viewSnake positions size =
    let
        viewRect { x, y } =
            rect
                [ S.x <| toString <| x * size
                , S.y <| toString <| y * size
                , S.width <| toString size
                , S.height <| toString size
                , S.fill "#49CDF6"
                ]
                []
    in
        g []
            (List.map viewRect positions)


main : Program Never Model Msg
main =
    program
        { init = ( initialModel, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
