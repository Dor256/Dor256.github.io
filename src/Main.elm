module Main exposing (..)

import Browser exposing (element)
import Time
import Html exposing (div, h1, text, main_, span, Html)
import Html.Attributes exposing (class)
import Task

main : Program () Model Msg
main =
  element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



type alias Model =
  { time : Time.Posix }


init : () -> (Model, Cmd Msg)
init _ =
  ( { time = (Time.millisToPosix 0) }
  , Task.perform GetTime Time.now
  )



type Msg
  = Tick
  | GetTime Time.Posix



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick ->
      ( { model | time = Time.millisToPosix (Time.posixToMillis model.time - 1000 ) }
      , Cmd.none
      )

    GetTime newTime ->
      ( { model | time = (adjustTime newTime) }
      , Cmd.none
      )

flightTime : Time.Posix
flightTime =
  Time.millisToPosix 1650059400000

adjustTime : Time.Posix -> Time.Posix
adjustTime currentTime =
  (Time.posixToMillis flightTime) - (Time.posixToMillis currentTime)
    |> Time.millisToPosix


subscriptions : Model -> Sub Msg
subscriptions _ =
  Time.every 1000 (always Tick)


calculateSeconds : Time.Posix -> Int
calculateSeconds time =
  let
    seconds = (Time.posixToMillis time) // 1000
  in
  if seconds >= 60 then modBy 60 seconds else seconds

calculateMinutes : Time.Posix -> Int
calculateMinutes time =
  let
    minutes = (Time.posixToMillis time) // 60000
  in
  if minutes >= 60 then modBy 60 minutes else minutes

calculateHours : Time.Posix -> Int
calculateHours time =
  let
    hours = (Time.posixToMillis time) // 3600000
  in
  if hours >= 24 then modBy 24 hours else hours

calculateDays : Time.Posix -> Int
calculateDays time =
 (Time.posixToMillis time) // 3600000 // 24


view : Model -> Html Msg
view model =
  let
    day = calculateDays model.time
    hour = calculateHours model.time
    minute = calculateMinutes model.time
    second = calculateSeconds model.time
  in
  main_ [] 
    [ h1 []
        [ text "New York" ]
    , div [ class "clock" ]
        [ div [ class "time-section" ]
            [ span [ class "time-count" ] [ text (renderClock day) ]
            , span [ class "time-text" ] [ text "days" ]
            ]
          , div [ class "time-section" ]
            [ span [ class "time-count" ] [ text (renderClock hour) ]
            , span [ class "time-text" ] [ text "hours" ]
            ]
          , div [ class "time-section" ]
            [ span [ class "time-count" ] [ text (renderClock minute) ]
            , span [ class "time-text" ] [ text "minutes" ]
            ]
          , div [ class "time-section" ]
            [ span [ class "time-count" ] [ text (renderClock second) ]
            , span [ class "time-text" ] [ text "seconds" ]
            ]
        ]
    ]

renderClock : Int -> String
renderClock section =
  if section <= 0 then
    "0" ++ String.fromInt 0
  else if section >= 10 then
    String.fromInt section
  else "0" ++ String.fromInt section
