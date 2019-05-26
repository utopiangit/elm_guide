module Example exposing (..)

import Browser
import Bulma.CDN exposing (..)
import Bulma.Modifiers exposing (..)
import Bulma.Modifiers.Typography exposing (textCentered)
import Bulma.Form exposing (..)
import Bulma.Elements exposing (..)
import Bulma.Components exposing (..)
import Bulma.Columns as Columns exposing (..)
import Bulma.Layout exposing (..)

import Html exposing ( Html, Attribute, main_, span, a, p, img ,br, text, strong, option, small, input, i )
import Html.Attributes exposing ( attribute, style, src, placeholder, type_, href, rel, class, value )
import Html.Events exposing (..)

-- TODO
-- Set today as an initial value in Date
-- Store the shopping history into DB
-- Statistics tab


type alias Shopping
  = { user : String
    , date : String
    , category : String
    , amount : String
    , memo : String
  }

type alias Model 
  = { user : String
    , date : String
    , category : String
    , amount : String
    , memo : String
    , shoppings : List Shopping
    , tab : TabState
    }


init : Model
init = initialShopping [] Input

initialShopping : List Shopping -> TabState -> Model 
initialShopping = Model "Yuucha" "" "Food" "" ""

type TabState 
  = Input
  | History

type Msg 
  = InputUser String
  | InputDate String
  | InputCategory String
  | InputAmount String
  | InputMemo String
  | Submit
  | Delete Int
  | InputTab
  | HistoryTab


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

    InputMemo memo ->
      { model | memo = memo }

    Submit -> 
      -- { model | memos = (Shopping model.user model.date model.category model.amount) :: model.memos }
      initialShopping 
        ((Shopping model.user model.date model.category model.amount model.memo) :: model.shoppings)
        Input

    Delete n ->
      let
        t = model.shoppings
      in
        { model | shoppings = List.take n t ++ List.drop (n + 1) t }

    InputTab -> 
      { model | tab = Input }

    HistoryTab -> 
      { model | tab = History }

main : Program () Model Msg
main
  = Browser.sandbox
    { init = init
    , view = view
    , update = update
    }

fontAwesomeCDN
  = Html.node "link"
    [ rel "stylesheet"
    , href "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"
    ]
    []

view : Model -> Html Msg
view model
  = main_ []
    [ stylesheet
    , fontAwesomeCDN
    , topHero
    , tabsView model
    , contentView model
    ]

topHero : Html Msg
topHero
  = hero { heroModifiers | color = Info, size = Small } []
      [ heroBody []
        [ container []
          [ title H3 [] [ text "HouseHold app" ] ]
        ]
      ]

tabsView : Model -> Html Msg
tabsView model = 
  tabs { tabsModifiers | style = Minimal, alignment = Centered } [] []
      [ tab (model.tab == Input) [ onClick InputTab ] [] 
          [ icon Standard [] [ i [ class "fa fa-pencil" ] [] ], text "Input" ]
      , tab (model.tab == History) [ onClick HistoryTab ] [] 
          [ icon Standard [] [ i [ class "fa fa-history" ] [] ], text "History" ]
      ]

contentView : Model -> Html Msg
contentView model = 
  case model.tab of
          Input -> inputForm model
          History -> historyView model

historyView : Model -> Html Msg
historyView model = section NotSpaced []
    [ panel []
        [ demoSection "Shopping History" []
          [ field [] [ controlLabel [] [ text "Recent expenses.." ] ]
          , tableShopping model.shoppings
          ]
        ]
    ]

rowShopping : (Int, Shopping) -> TablePartition Msg
rowShopping (n, shopping) = 
  tableRow False [] 
    [ tableCell [] [ text shopping.date ]
    , tableCell [] [ text shopping.user ]
    , tableCell [] [ text shopping.category ]
    , tableCell [] [ text shopping.amount ]
    , tableCell [] [ text shopping.memo ]
    , tableCell [] 
      [ controlButton { buttonModifiers | color = Light, size = Small } 
        [ onClick (Delete n) ] 
        [] 
        [ text "x" ] 
      ]
    ]

tableShopping : List Shopping -> Table Msg
tableShopping hist = 
  table tableModifiers [] 
    [ tableHead [] 
      [ tableRow False [] 
          [ tableCell [] [ text "Date" ]
          , tableCell [] [ text "User"]
          , tableCell [] [ text "Category"]
          , tableCell [] [ text "Amount"]
          , tableCell [] [ text "Memo"]
          , tableCell [] [ text "Delete"]
          ]
      ]
    , tableBody [] << List.map rowShopping <| List.indexedMap Tuple.pair hist
    ]

inputForm : Model -> Html Msg
inputForm model = section NotSpaced []
    [ container []
      [ demoSection "Register.." []
        [ field []
          [ controlLabel [] [ text "Who you are" ]
          , controlSelect controlSelectModifiers [ onInput InputUser ] [ value model.user ]
            [ option [] [ text "Yuucha" ]
            , option [] [ text "Naocha" ]
            ]
          ]
        , field []
          [ controlLabel [] [ text "When" ]
          , controlInput controlInputModifiers [ onInput InputDate ] 
              [ placeholder "Date", type_ "date", value model.date ] [] 
          ]
        -- , field []
        --   [ controlLabel [] [ text "Where you bought" ]
        --   , controlInput controlInputModifiers [] [ placeholder "Shop name" ] [] 
        --   ]
        -- , field []
        --   [ controlLabel [] [ text "What you bought" ]
        --   , controlInput controlInputModifiers [] [ placeholder "Item" ] [] 
        --   , controlHelp Default [] [ text "This field isn't required!" ]
        --   ]
        , field []
          [ controlLabel [] [ text "Category" ]
          , controlSelect controlSelectModifiers [ onInput InputCategory ] [ value model.category ]
            [ option [] [ text "Food" ]
            , option [] [ text "EatOut" ]
            , option [] [ text "Livingware" ]
            , option [] [ text "Transportation" ]
            , option [] [ text "Fashon" ]
            , option [] [ text "Leisure" ]
            , option [] [ text "Hobby" ]
            , option [] [ text "Gas, Water, Electric, e.t.c." ]
            , option [] [ text "Rent" ]
            , option [] [ text "Medical" ]
            , option [] [ text "Other" ]
            ]
          ]
        , field []
          [ controlLabel [] [ text "How much it is" ]
          , controlInput controlInputModifiers [ onInput InputAmount ] 
            [ placeholder "price", value model.amount ] [] 
          ]        
        , field []
          [ controlLabel [] [ text "Memo" ]
          , controlInput controlInputModifiers [ onInput InputMemo ] 
            [ placeholder "What you bought / Where you bought it e.t.c.", value model.memo ] [] 
          ]        
        , field []
          [ controlButton { buttonModifiers | color = Link } [ onClick Submit ] []
            [ text "Register"
            ]
          ]
        ]
      ]
    ]


myColumnModifiers : Width -> Maybe Width -> ColumnModifiers
myColumnModifiers offset width
  = let widths : Devices (Maybe Width)
        widths = columnModifiers.widths
    in { columnModifiers
         | offset
           = offset
         , widths
           = { widths
               | tablet     = width
               , desktop    = width
               , widescreen = width
               , fullHD     = width
             }
       }

demoSection : String -> List (Attribute Msg) -> List (Html Msg) -> Html Msg
demoSection aSubtitle someAttrs someHtmls
  = columns columnsModifiers someAttrs
    [ column (myColumnModifiers Auto (Just Width3)) []
      [ subtitle H4 [] [ text aSubtitle ]
      ]
    , column (myColumnModifiers Auto (Just Auto)) []
      someHtmls
    ]

