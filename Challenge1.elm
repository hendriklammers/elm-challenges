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
    , background : String
    }


initialModel : Model
initialModel =
    { label = ""
    , windowSize = Size 0 0
    , background = "#000"
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

                background =
                    if label == "left" then
                        "#CE1C4F"
                    else
                        "#3E4DCF"
            in
                ( { model | label = label, background = background }, Cmd.none )



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
    div [ style [ ( "height", "100vh" ) ] ]
        [ viewBackground model
        , viewText model.label
        ]


viewBackground : Model -> Html Msg
viewBackground { background } =
    div
        [ style (( "background", background ) :: backgroundStyle) ]
        [ text "" ]


backgroundStyle : List ( String, String )
backgroundStyle =
    [ ( "position", "absolute" )
    , ( "width", "100%" )
    , ( "height", "100%" )
    , ( "transition", "background 0.2s ease-out" )
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
    , ( "font", "bold 48px Helvetica, sans-serif" )
    , ( "color", "#fff" )
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
