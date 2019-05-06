import Browser
import Html exposing (Html, Attribute, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

main = 
  Browser.sandbox {init = init, update = update, view = view}

type alias Model = 
  { content : String}

-- モデルはレコード構文
-- Stringしか持っていないのでModel = Stringと同じだが、あとでフィールドを追加する場合にレコードにしておいたほうが楽になる
init : Model
init = 
  { content = "" }

-- Change Stringで一つの型
-- Change : String -> Msg
type Msg = 
  Change String

update : Msg -> Model -> Model
update msg model = 
  case msg of
    Change newContent -> { model | content = newContent } -- レコードのうちcontentだけ更新
-- 下記でもOK
-- update (Change content) _ = {content = content }

view : Model -> Html Msg
view model = 
  div []
    [ 
      input [ placeholder "Text to reverse", value model.content, onInput Change ] [],
      div [] [ text (String.reverse model.content) ] 
    ]