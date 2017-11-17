port module Score exposing (..)

import Html exposing (Html, div, text, span)
import Html.Attributes as H


port sendScore : Int -> Cmd msg


port getScore : (Int -> msg) -> Sub msg


type alias Score =
    { score : Int
    , highScore : Int
    }


view : Int -> Int -> Html msg
view score highScore =
    div [ H.class "score" ]
        [ div [ H.class "score__container" ]
            [ text "Your score: "
            , span
                [ H.class "score__value" ]
                [ text <| toString score ]
            ]
        , div [ H.class "score__container" ]
            [ text "Highscore: "
            , span
                [ H.class "score__value" ]
                [ text <| toString highScore ]
            ]
        ]
