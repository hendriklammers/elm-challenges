module Challenge4 exposing (..)

import Json.Decode as Decode
import Html
    exposing
        ( Html
        , a
        , div
        , img
        , input
        , label
        , program
        , span
        , text
        )
import Html.Attributes as H
import Html.Events exposing (onInput)
import Http
import Process
import Task
import Time
import String exposing (length)


-- Model


type UserData
    = NotAsked
    | Loading
    | Failure Http.Error
    | Success User


type alias Model =
    { term : String
    , debounceCount : Int
    , user : UserData
    }


type alias User =
    { login : String
    , avatarUrl : String
    , url : String
    , name : Maybe String
    , bio : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    ( Model "" 0 NotAsked, Cmd.none )



-- Update


type Msg
    = Timeout Int
    | Input String
    | NewData (Result Http.Error User)


debounceCmd : Int -> Cmd Msg
debounceCmd count =
    Process.sleep (1000 * Time.millisecond)
        |> Task.perform (\_ -> Timeout count)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input term ->
            let
                count =
                    model.debounceCount + 1
            in
                ( { model | term = term, debounceCount = count }
                , debounceCmd count
                )

        Timeout count ->
            if count == model.debounceCount then
                if length model.term < 1 then
                    { model | debounceCount = 0, user = NotAsked } ! []
                else
                    ( { model | debounceCount = 0, user = Loading }
                    , fetchUser model.term
                    )
            else
                model ! []

        NewData result ->
            case result of
                Err err ->
                    let
                        log =
                            Debug.log "result" err
                    in
                        { model | user = Failure err } ! []

                Ok user ->
                    { model | user = Success user } ! []


fetchUser : String -> Cmd Msg
fetchUser term =
    let
        url =
            "https://api.github.com/users/" ++ term

        request =
            Http.get url userDecoder
    in
        Http.send NewData request


userDecoder : Decode.Decoder User
userDecoder =
    Decode.map5
        User
        (Decode.field "login" Decode.string)
        (Decode.field "avatar_url" Decode.string)
        (Decode.field "html_url" Decode.string)
        (Decode.field "name" (Decode.maybe Decode.string))
        (Decode.field "bio" (Decode.maybe Decode.string))



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- View


view : Model -> Html Msg
view model =
    div [ H.style containerStyle ]
        [ viewSearch model
        , viewUserContainer model.user
        ]


viewUserContainer : UserData -> Html Msg
viewUserContainer data =
    div []
        [ case data of
            NotAsked ->
                text ""

            Loading ->
                text "Loading..."

            Failure err ->
                text "There was an error"

            Success user ->
                viewUser user
        ]


viewUser : User -> Html Msg
viewUser user =
    div [ H.style [ ( "display", "flex" ) ] ]
        [ img
            [ H.src user.avatarUrl
            , H.style
                [ ( "width", "96px" )
                , ( "height", "96px" )
                ]
            , H.alt "user avatar"
            ]
            []
        , div
            [ H.style [ ( "flex", "1" ), ( "padding-left", "15px" ) ] ]
            [ a
                [ H.href user.url ]
                [ text user.login ]
            , viewUserProp "User" user.name
            , viewUserProp "Bio" user.bio
            ]
        ]


viewLabel : String -> String -> Html Msg
viewLabel label value =
    div [ H.style [ ( "margin", "6px 0" ) ] ]
        [ span
            [ H.style [ ( "font-style", "italic" ), ( "margin-right", "5px" ) ] ]
            [ text (label ++ ":") ]
        , span
            []
            [ text value ]
        ]


viewUserProp : String -> Maybe String -> Html Msg
viewUserProp label value =
    case value of
        Nothing ->
            viewLabel label "-"

        Just str ->
            viewLabel label str


containerStyle : List ( String, String )
containerStyle =
    [ ( "margin", "24px auto" )
    , ( "width", "480px" )
    , ( "display", "flex" )
    , ( "flex-direction", "column" )
    , ( "font-family", "Helvetica, sans-serif" )
    ]


searchInputStyle : List ( String, String )
searchInputStyle =
    [ ( "display", "block" )
    , ( "margin", "12px auto" )
    , ( "width", "calc(100% - 14px)" )
    , ( "height", "30px" )
    , ( "padding", "5px" )
    , ( "border", "2px solid black" )
    , ( "font-size", "16px" )
    ]


viewSearch : Model -> Html Msg
viewSearch model =
    div [ H.style [ ( "flex", "1" ) ] ]
        [ input
            [ H.type_ "text"
            , H.placeholder "Search Github user"
            , H.style searchInputStyle
            , onInput Input
            ]
            []
        ]



-- Main


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
