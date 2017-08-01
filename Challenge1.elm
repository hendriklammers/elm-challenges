module Challenge1 exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, style)
import String exposing (toUpper)
import Mouse exposing (Position)
import Task
import Window exposing (Size)


-- Model


type alias Model =
    { label : String
    , windowSize : Size
    }


initialModel : Model
initialModel =
    { label = ""
    , windowSize = Size 0 0
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Task.perform WindowResize Window.size )



-- Update


type Msg
    = MouseMove Position
    | WindowResize Size


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResize size ->
            ( { model | windowSize = size }, Cmd.none )

        MouseMove pos ->
            let
                label =
                    if pos.x < round ((toFloat model.windowSize.width) / 2) then
                        "left"
                    else
                        "right"
            in
                ( { model | label = label }, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.moves MouseMove
        , Window.resizes WindowResize
        ]



-- View


view : Model -> Html Msg
view model =
    div []
        [ viewText model.label
        , div [ style dividerStyle ] []
        ]


dividerStyle : List ( String, String )
dividerStyle =
    [ ( "position", "fixed" )
    , ( "left", "50%" )
    , ( "top", "0" )
    , ( "width", "2px" )
    , ( "height", "100%" )
    , ( "background", "#000" )
    , ( "transform", "translateX(-50%)" )
    ]


containerStyle : List ( String, String )
containerStyle =
    [ ( "position", "absolute" )
    , ( "left", "50%" )
    , ( "top", "50%" )
    , ( "transform", "translate(-50%, -50%)" )
    ]


textStyle : List ( String, String )
textStyle =
    [ ( "margin", "0" )
    , ( "font", "bold 24px Helvetica, sans-serif" )
    ]


viewText : String -> Html msg
viewText str =
    div
        [ class "text-container", style containerStyle ]
        [ h1
            [ style textStyle ]
            [ toUpper str |> text ]
        ]


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
