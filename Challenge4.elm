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
        , ul
        , li
        )
import Html.Attributes as H
import Html.Events exposing (onInput)
import Http
import Process
import Task
import Time
import String exposing (length, join)
import Set exposing (Set)


-- Model


type UserData
    = NotAsked
    | Loading
    | Failure Http.Error
    | Success User


type alias Model =
    { username : String
    , debounceCount : Int
    , user : UserData
    }


type alias User =
    { login : String
    , avatarUrl : String
    , url : String
    , name : Maybe String
    , bio : Maybe String
    , languages : Set String
    }


type alias Repo =
    { language : Maybe String }


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
        Input username ->
            let
                count =
                    model.debounceCount + 1
            in
                ( { model | username = username, debounceCount = count }
                , debounceCmd count
                )

        Timeout count ->
            if count == model.debounceCount then
                if length model.username < 1 then
                    { model | debounceCount = 0, user = NotAsked } ! []
                else
                    ( { model | debounceCount = 0, user = Loading }
                    , fetchData model.username
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


fetchData : String -> Cmd Msg
fetchData username =
    let
        userUrl =
            "https://api.github.com/users/" ++ username

        reposUrl =
            "https://api.github.com/users/" ++ username ++ "/repos"
    in
        Task.attempt NewData <|
            Task.map2 applyLanguages
                (Http.get userUrl userDecoder |> Http.toTask)
                (Http.get reposUrl repoListDecoder |> Http.toTask)


applyLanguages : User -> List Repo -> User
applyLanguages user repos =
    let
        reducer repo ls =
            case repo.language of
                Nothing ->
                    ls

                Just language ->
                    language :: ls

        languages =
            List.foldl reducer [] repos
    in
        { user | languages = Set.fromList languages }


userDecoder : Decode.Decoder User
userDecoder =
    Decode.map6
        User
        (Decode.field "login" Decode.string)
        (Decode.field "avatar_url" Decode.string)
        (Decode.field "html_url" Decode.string)
        (Decode.field "name" (Decode.maybe Decode.string))
        (Decode.field "bio" (Decode.maybe Decode.string))
        (Decode.succeed Set.empty)


repoDecoder : Decode.Decoder Repo
repoDecoder =
    Decode.map Repo (Decode.field "language" (Decode.maybe Decode.string))


repoListDecoder : Decode.Decoder (List Repo)
repoListDecoder =
    Decode.list repoDecoder



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
            , viewLanguages user.languages
            ]
        ]


viewLanguages : Set String -> Html Msg
viewLanguages languages =
    let
        items =
            Set.foldl (\curr acc -> (li [] [ text curr ]) :: acc) [] languages
    in
        div [ H.style [ ( "margin", "6px 0" ) ] ]
            [ span
                [ H.style
                    [ ( "font-style", "italic" )
                    , ( "margin-right", "5px" )
                    ]
                ]
                [ text "Languages:" ]
            , ul
                [ H.style
                    [ ( "display", "inline-block" )
                    , ( "list-style", "none" )
                    , ( "margin", "0 0 0 2px" )
                    , ( "padding", "0" )
                    , ( "vertical-align", "top" )
                    ]
                ]
                items
            ]


viewLabel : String -> String -> Html Msg
viewLabel label value =
    div [ H.style [ ( "margin", "6px 0" ) ] ]
        [ span
            [ H.style
                [ ( "font-style", "italic" )
                , ( "margin-right", "5px" )
                ]
            ]
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
