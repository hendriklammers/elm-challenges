module Main exposing (..)

import Html exposing (Html, text, program)
import Snake exposing (Msg)


-- Model


type alias Model =
    { snake : Snake.Model }


initialModel : Model
initialModel =
    { snake = Snake.initialModel }



-- Update


type Msg
    = NoOp
    | SetSnakeState Snake.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetSnakeState snakeMsg ->
            updateSnake model (Snake.update snakeMsg model.snake)


updateSnake : Model -> ( Snake.Model, Cmd Snake.Msg ) -> ( Model, Cmd Msg )
updateSnake model ( snake, msg ) =
    ( { model | snake = snake }, Cmd.map SetSnakeState msg )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map SetSnakeState (Snake.subscriptions model.snake)



-- View


view : Model -> Html Msg
view model =
    viewSnake model.snake


viewSnake : Snake.Model -> Html Msg
viewSnake snake =
    Html.map SetSnakeState (Snake.view snake)


main : Program Never Model Msg
main =
    program
        { init = ( initialModel, Cmd.none )
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
