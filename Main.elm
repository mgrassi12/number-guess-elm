module Main exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String
import Random

main : Program Never
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
  ( Model 1 "" 0 False ""
  , generateSecretNumber
  )

generateSecretNumber : Cmd Msg
generateSecretNumber =
  Random.generate SetSecretNumber (Random.int 1 10)


-- UPDATE


type Msg
    = PlayerGuess String
    | SubmitGuess
    | ResetGame
    | SetSecretNumber Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlayerGuess guessValue ->
          { model | playerGuess = guessValue } ! []

        SubmitGuess ->
          ( compareGuess model, Cmd.none )

        ResetGame ->
          init

        SetSecretNumber generatedNumber ->
          { model | secretNumber = generatedNumber } ! []




-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "I am thinking of a number..." ]
        , h3 [] [ text "Between 1 and 10" ]
        , p [] [ text "Your guess: " ]
        , input
          [ value model.playerGuess
          , placeholder "Goes here"
          , type' "number"
          , onInput PlayerGuess ]
          []
        , p [] [ text model.status ]
        , button [ onClick SubmitGuess ] [ text "Submit" ]
        , button [ onClick ResetGame ] [ text "Reset" ]
        , br [] []
        , p [] [ text ("Number of guesses: " ++ (toString model.guessCounter)) ]
        ]



-- OTHER FUNCTIONS


compareGuess : Model -> Model
compareGuess model =
    case String.toInt model.playerGuess of
        Ok integer ->
            withinBounds model integer

        Err a ->
            { model | status = "Please enter a number!" }


withinBounds : Model -> Int -> Model
withinBounds model integer =
    if 10 >= integer && integer >= 1 then
        guessMatch model integer
    else
        { model | status = "Please enter a number between one and ten!" }


guessMatch : Model -> Int -> Model
guessMatch model integer =
    if integer == model.secretNumber then
        Model model.secretNumber model.playerGuess (model.guessCounter + 1) True "Well done. You got it!"
    else if integer > model.secretNumber then
        { model | status = "Too high!", guessCounter = (model.guessCounter + 1) }
    else
        { model | status = "Too low!", guessCounter = (model.guessCounter + 1) }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
