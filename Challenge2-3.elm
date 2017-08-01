module Challenge2 exposing (..)

import Html exposing (Html, program, text, div, button, input)
import Html.Attributes as HtmlAttr
import Html.Events exposing (onClick, onInput)
import Keyboard
import Random exposing (Generator)
import Svg exposing (Svg, svg, circle, rect, g)
import Svg.Attributes exposing (cx, cy, r, width, height, viewBox, fill)
import Task
import Time exposing (every, millisecond)
import Window exposing (Size)


-- Model


type alias Model =
    { circles : List Circle
    , windowSize : Size
    , paused : Bool
    , speed : Float
    }


type alias Circle =
    { cx : Int
    , cy : Int
    , radius : Int
    }


initialModel : Model
initialModel =
    { circles = []
    , windowSize = Size 0 0
    , paused = False
    , speed = 500
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Task.perform WindowResize Window.size )



-- Update


type Msg
    = Tick Time.Time
    | NewCircle Circle
    | WindowResize Size
    | PauseClick
    | ResetClick
    | KeyPress Int
    | ChangeSpeed String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResize size ->
            ( { model | windowSize = size }, Cmd.none )

        KeyPress char ->
            case char of
                112 ->
                    ( { model | paused = not model.paused }, Cmd.none )

                114 ->
                    ( { model | circles = [] }, Cmd.none )

                _ ->
                    model ! []

        Tick time ->
            ( model, Random.generate NewCircle (randomCircle model.windowSize) )

        NewCircle circle ->
            ( { model | circles = circle :: model.circles }, Cmd.none )

        PauseClick ->
            ( { model | paused = not model.paused }, Cmd.none )

        ResetClick ->
            ( { model | circles = [] }, Cmd.none )

        ChangeSpeed speed ->
            case String.toFloat speed of
                Err _ ->
                    ( { model | speed = 50 }, Cmd.none )

                Ok s ->
                    ( { model | speed = s }, Cmd.none )


randomCircle : Size -> Generator Circle
randomCircle window =
    Random.map3 Circle
        (Random.int 0 window.width)
        (Random.int 0 window.height)
        (Random.int 10 50)



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        timer =
            if model.paused then
                Sub.none
            else
                every (millisecond * model.speed) Tick
    in
        Sub.batch
            [ Window.resizes WindowResize
            , timer
            , Keyboard.presses (\keycode -> KeyPress keycode)
            ]



-- View


view : Model -> Html Msg
view model =
    div []
        [ viewSvg model
        , viewControls model
        ]


viewControls : Model -> Html Msg
viewControls model =
    div
        [ HtmlAttr.style
            [ ( "position", "fixed" )
            , ( "top", "0" )
            , ( "left", "0" )
            , ( "z-index", "1" )
            ]
        ]
        [ button [ onClick PauseClick ] [ text "Pause" ]
        , button [ onClick ResetClick ] [ text "Reset" ]
        , input
            [ HtmlAttr.type_ "range"
            , HtmlAttr.min "10"
            , HtmlAttr.max "1000"
            , HtmlAttr.step "10"
            , HtmlAttr.value (toString model.speed)
            , onInput ChangeSpeed
            ]
            []
        ]


viewSvg : Model -> Html Msg
viewSvg model =
    let
        w =
            toString model.windowSize.width

        h =
            toString model.windowSize.height
    in
        svg
            [ width w, height h, viewBox ("0 0 " ++ w ++ " " ++ h) ]
            [ rect
                [ width w, height h, fill "#efefef" ]
                []
            , (viewCircles model.circles)
            ]


viewCircle : Circle -> Svg Msg
viewCircle c =
    circle
        [ cx (toString c.cx)
        , cy (toString c.cy)
        , r (toString c.radius)
        , fill "#4404D4"
        ]
        []


viewCircles : List Circle -> Svg Msg
viewCircles circles =
    g []
        (List.map viewCircle circles)



-- Main


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
