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
init = initialModel []

initialModel : List Shopping -> Model 
initialModel = Model "Yuucha" "" "Food" "0"

type Msg 
  = InputUser String
  | InputDate String
  | InputCategory String
  | InputAmount String
  | Submit
  | Delete Int

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
      initialModel 
        <| (Shopping model.user model.date model.category model.amount) :: model.memos    

    Delete n ->
      let
        t = model.memos
      in
        { model | memos = List.take n t ++ List.drop (n + 1) t }

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
        , div [] 
            [ text "Date"
            , input 
                [ type_ "date"
                , placeholder "Date"
                , onInput InputDate
                , value model.date
                ] [] 
            ]
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
    -- , ul [] <| List.map showShopping model.memos
    , table [] <| viewHeader :: (List.map viewShopping <| List.indexedMap Tuple.pair model.memos)
    ]    

viewForm txt ctor val = 
  div [] 
    [ text txt
    , input [ placeholder txt, onInput ctor, value val ] []
    ]

viewHeader  = 
  thead [] 
    [ th [] [text "user"]
    , th [] [text "date"]
    , th [] [text "category"]
    , th [] [text "amount"]
    ] 

viewShopping (n, shopping) = 
  tr [] 
    [ td [] [ text shopping.user ]
    , td [] [ text shopping.date ]
    , td [] [ text shopping.category ]
    , td [] [ text shopping.amount ]
    , button [ onClick (Delete n) ] [ text "x"]
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