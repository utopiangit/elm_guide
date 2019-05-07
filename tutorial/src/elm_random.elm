import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Random

main = 
  Browser.element {
    init = init,
    subscriptions = subscriptions,
    update = update,
    view = view
  }

type alias Model = {
  diceFace : Int
  }

init : () -> (Model, Cmd Msg)
init _ = (Model 1, Cmd.none)

type Msg
  = Roll
  | NewFace Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    -- generate : (a -> msg) -> Generator a -> Cmd msg
    -- NewFace : Int -> Msg
    Roll -> (model, Random.generate NewFace (Random.int 1 6))
    NewFace newFace -> ( { model | diceFace = newFace }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

view : Model -> Html Msg
view model = 
  div []
    [
      h1 [] [ text (String.fromInt model.diceFace) ],
      button [ onClick Roll ] [ text "Roll" ]
    ]