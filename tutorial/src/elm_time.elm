import Browser
import Html exposing (..)
import Task
import Time
import Html.Events exposing (onInput, onClick)

main = 
  Browser.element {
    init = init,
    subscriptions = subscriptions,
    update = update,
    view = view
  }

type alias Model = {
    zone : Time.Zone,
    time : Time.Posix,
    isWorking : Bool
  }

init : () -> (Model, Cmd Msg)
init _ = 
  ( Model Time.utc (Time.millisToPosix 0) True
  , Task.perform AdjustTimeZone Time.here
  )

type Msg
  = Tick Time.Posix
  | AdjustTimeZone Time.Zone
  | Switch

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    Tick newTime -> (
      if model.isWorking then
        { model | time = newTime }
      else 
        model
      , Cmd.none
     )
    AdjustTimeZone newZone -> (
      { model | zone = newZone }
      , Cmd.none
      )
    Switch -> 
      ( { model | isWorking = not model.isWorking }
      , Cmd.none
      )
    

subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick

view : Model -> Html Msg
view model = 
  div [] 
    [ timer model
    , button [ onClick Switch ] [ text "Start/Stop" ] 
    ]
          
timer : Model -> Html Msg
timer model = 
  let
    hour   = String.fromInt (Time.toHour   model.zone model.time)
    minute = String.fromInt (Time.toMinute model.zone model.time)
    second = String.fromInt (Time.toSecond model.zone model.time)
  in
    h1 [] [ text (hour ++ ":" ++ minute ++ ":" ++ second) ]