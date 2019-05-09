import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, string)


main = 
  Browser.element {
    init = init,
    subscriptions = subscriptions,
    update = update,
    view = view
  }

type Model
  = Failure
  | Loading
  | Success String


init : () -> (Model, Cmd Msg)
init _ = 
  (Loading, getRandomCatGif)

type Msg 
  = MorePlease
  | GotGif (Result Http.Error String)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MorePlease ->
      (Loading, getRandomCatGif)

    GotGif result ->
      case result of
        Ok url ->
          (Success url, Cmd.none)

        Err _ ->
          (Failure, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


view : Model -> Html Msg
view model =
  div []
    [ 
      h2 [] [ text "Random Cats" ],
      viewGif model
    ]


viewGif : Model -> Html Msg
viewGif model =
  case model of
    Failure ->
      div []
        [ 
          text "I could not load a random cat for some reason. ",
          button [ onClick MorePlease ] [ text "Try Again!" ]
        ]

    Loading ->
      text "Loading..."

    Success url ->
      div []
        [ 
          button [ onClick MorePlease, style "display" "block" ] [ text "More Please!" ],
          img [ src url ] []
        ]



getRandomCatGif : Cmd Msg
getRandomCatGif =
  Http.get
    { 
      url = "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=cat",
      expect = Http.expectJson GotGif gifDecoder
    }

-- 例：filed "data" int 
-- JSONにdataというフィールドがあるか確認、あればintでパースするデコーダー
-- int : Decoder Int
-- 以下は再帰的なデコーダーで、再帰的なJSONの中のimage_urlをデコードする
gifDecoder : Decoder String
gifDecoder =
  field "data" (field "image_url" string)