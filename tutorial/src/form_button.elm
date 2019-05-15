import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

main =
  Browser.sandbox { init = init, update = update, view = view }

type alias Model = 
  {
    name : String,
    password : String,
    passwordAgain : String,
    age : String,
    showWarning : Bool
  }

init : Model
init = 
  Model "" "" "" "" False

type Msg 
  = Name String
  | Password String
  | PasswordAgain String
  | Age String
  | Submit

update : Msg -> Model -> Model
update msg model
  = case msg of 
    Name name -> { model | name = name }
    Password password -> { model | password = password }
    PasswordAgain passwordAgain -> { model | passwordAgain = passwordAgain}
    Age age -> { model | age = age } -- Stringで受け取って、あとで数値か判定？
    Submit -> { model | showWarning = True }

view : Model -> Html Msg
view model = 
  div [] [
    viewInput "text" "Name" model.name Name,
    viewInput "text" "Age" model.age Age,
    showIfPressed model viewValidationAge,
    viewInput "password" "Password" model.password Password,
    viewInput "password" "Re-enter password" model.passwordAgain PasswordAgain,
    showIfPressed model viewValidation,
    button [ onClick Submit ] [ text "submit" ]
  ]

viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []

viewValidation : Model -> Html Msg
viewValidation model =
  if String.length model.password < 8 then 
    div [ style "color" "red" ] [ text "Password must be longer than 8!" ]
  else if String.toUpper model.password == model.password 
    || String.toLower model.password == model.password then 
    div [ style "color" "red" ] [ text "Password must include both upper and lower cases!" ]
  else if model.password /= model.passwordAgain then
    div [ style "color" "red" ] [ text "Passwords do not match!" ]
  else
    div [ style "color" "green" ] [ text "OK" ]

viewValidationAge : Model -> Html Msg
viewValidationAge model = 
  if String.all Char.isDigit model.age then
    div [ style "color" "green" ] [ text "OK" ]
  else
    div [ style "color" "red" ] [ text "Age must be a digit!" ]

showIfPressed : Model -> (Model -> Html Msg) -> Html Msg
showIfPressed model toMsg =
  if model.showWarning then
    toMsg model
  else 
    div [] []
