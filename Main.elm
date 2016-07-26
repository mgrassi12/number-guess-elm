module Main exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { secretNumber : Int
    , playerGuess : String
    , guessCounter : Int
    , isDone : Bool
    , status : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model 3 "" 0 False "", Cmd.none )



-- UPDATE


type Msg
    = PlayerGuess String
    | SubmitGuess
    | Error String
    | WrongGuess String
    | Success
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlayerGuess guessValue ->
            { model | playerGuess = guessValue } ! []

        SubmitGuess ->
            ( model, compareGuess model.secretNumber model.guessValue )

        Error errorMessage ->
            { model | status = errorMessage } ! []

        WrongGuess guessInfo ->
            { model | status = guessInfo, guessCounter = (model.guessCounter + 1) } ! []

        Success ->
            ( Model model.secretNumber model.playerGuess (model.guessCounter + 1) True "Well done. You got it!", Cmd.none )

        NoOp ->
            model ! []



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "I am thinking of a number..." ]
        , h3 [] [ text "Between 1 and 10" ]
        , p [] [ text "Your guess: " ]
        , input [ placeholder "Goes here", type' "number", onInput PlayerGuess ] []
        , p [] [ text model.status ]
        , button [ onClick SubmitGuess ] [ text "Submit" ]
        , button [] [ text "Reset" ]
        , br [] []
        , p [] [ text ("Number of guesses: " ++ (toString model.guessCounter)) ]
        ]



-- OTHER FUNCTIONS


compareGuess : Int -> String -> Cmd Msg
compareGuess secretNumber guessValue =
    case String.toInt guessValue of
        Ok integer ->
            withinBounds integer secretNumber

        Err a ->
            Error "Please enter a number!"


withinBounds : Int -> Int -> Cmd Msg
withinBounds integer secretNumber =
    if 10 >= integer && integer >= 1 then
        guessMatch integer secretNumber
    else
        Error "Please enter a number between one and ten!"


guessMatch : Int -> Int -> Cmd Msg
guessMatch integer secretNumber =
    if integer == secretNumber then
        Success
    else if integer > secretNumber then
        WrongGuess "Too high!"
    else
        WrongGuess "Too low!"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
