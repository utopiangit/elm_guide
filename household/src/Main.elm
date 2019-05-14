import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time

main =
  Browser.sandbox 
    { init = init
    , update = update
    , view = view 
    }

type Category 
  = Food
  | EatOut
  | Transportation
  | Fashon
  | Other
  | Unknown

type alias Shopping
  = { user : String
    , date : String
    , category : String
    , amount : String
  }

type alias Model 
  = { user : String
    , date : String
    , category : String
    , amount : String
    , memos : List Shopping
    }

init : Model
init = 
  Model "Yuucha" "" "" "0" []

type Msg 
  = InputUser String
  | InputDate String
  | InputCategory String
  | InputAmount String
  | Submit

update : Msg -> Model -> Model
update msg model =
  case msg of
    InputUser user ->
      { model | user = user }

    InputDate date ->
      { model | date = date }

    InputCategory category ->
      { model | category = category }

    InputAmount amount ->
      { model | amount = amount }

    Submit -> 
      Model "Yuucha" "" "" "0" 
        <| (Shopping model.user model.date model.category model.amount) :: model.memos    

view : Model -> Html Msg
view model = 
  div [] 
    [ Html.form 
        [ onSubmit Submit ]
        [ div [] 
          [ text "User"
          , select [ onInput InputUser ]
              [ option [ value "Yuucha" ] [ text "Yuucha" ]
              , option [ value "Naocha" ] [ text "Naocha" ]
              ]
          ]
        , div [] [ viewForm "Date" InputDate model.date ]
        , div [] 
          [ text "Category"
          , select [ onInput InputCategory ]
              [ option [ value "Food" ] [ text "Food" ]
              , option [ value "EatOut" ] [ text "EatOut" ]
              , option [ value "Transportation" ] [ text "Transportation" ]
              , option [ value "Fashon" ] [ text "Fashon" ]
              , option [ value "Other" ] [ text "Other" ]
              ]
          ]
        , div [] [ viewForm "Amount" InputAmount model.amount ]
        , div [] 
            [ button 
                [ disabled ( String.length model.amount < 1) ] 
                [ text "submit" ]
            ]
        ]
    , ul [] <| List.map showShopping model.memos
    ]    

viewForm txt ctor val = 
  div [] 
    [ text txt
    , input [ onInput ctor, value val ] []
    ]

showShopping : Shopping -> Html Msg
showShopping memo = li [] [ text <| memo.user ++ memo.date ++ memo.category ++ memo.amount ]

toCategory : String -> Category
toCategory category = 
  case category of 
    "Food" -> Food
    "EatOut" -> EatOut
    "Transportation" -> Transportation
    "Fashon" -> Fashon
    "Other" -> Other
    _ -> Unknown