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

import Html exposing ( Html, Attribute, main_, span, a, p, img ,br, text, strong, option, small, input, i, thead, tr, td, th, a, div)
import Html.Attributes exposing ( attribute, style, src, placeholder, type_, href, rel, class, value )
import Html.Events exposing (..)

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
    , tab : TabState
    }


init : Model
init = initialModel [] History

initialModel : List Shopping -> TabState -> Model 
initialModel = Model "Yuucha" "" "Food" "0"

type TabState 
  = Input
  | History

type Msg 
  = InputUser String
  | InputDate String
  | InputCategory String
  | InputAmount String
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

    Submit -> 
      initialModel 
        ((Shopping model.user model.date model.category model.amount) :: model.memos)
        Input

    Delete n ->
      let
        t = model.memos
      in
        { model | memos = List.take n t ++ List.drop (n + 1) t }

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
    -- , exampleElementsAndComponents
    , topHero
    , tabsView model
    , contentView model
    ]

topHero : Html Msg
topHero
  = hero { heroModifiers | color = Info, size = Small } []
      [ heroBody []
        [ container []
          [ title H2 [] [ text "HouseHold app" ] ]
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

contentView model = 
  case model.tab of
          Input -> inputForm model
          History -> historyView model

historyView : Model -> Html Msg
historyView model = section NotSpaced []
    [ panel []
        [ demoSection "Shoppings" []
          ([ field []
            [ controlLabel [] [ text "Recent items you bought.." ]
            ]
          ] ++ (viewShopping (List.indexedMap Tuple.pair model.memos)))
        , demoSection "" [] 
            [ table tableModifiers [] 
                [ tableHead [] 
                  [ tableRow False [] 
                      [ tableCell [] [ text "Date" ]
                      , tableCell [] [ text "User"]
                      , tableCell [] [ text "Category"]
                      , tableCell [] [ text "Amount"]
                      ]
                  ]
                , tableBody [] 
                  [ tableRow False [] 
                      [ tableCell [] [ text "1, 1" ]
                      , tableCell [] [ text "1, 2"]
                      , tableCell [] [ text "1, 3"]
                      ]
                  , tableRow False [] 
                      [ tableCell [] [ text "2, 1" ]
                      , tableCell [] [ text "2, 2"]
                      , tableCell [] [ text "2, 3"]
                      ]
                  ]
                ] 
            ]
        ]
    ]

viewShopping : List (Int, Shopping) -> List (Html Msg)
viewShopping hist = 
  let
    viewOne : (Int, Shopping) -> Html Msg
    viewOne (n, shopping) = container [] 
      [ text <| shopping.date ++ shopping.user ++ shopping.category ++ shopping.amount 
      , controlButton { buttonModifiers | size = Small, color = Light } 
          [ onClick (Delete n) ]
          []
          [ text "x" ]
      ]
  in List.map viewOne hist

inputForm : Model -> Html Msg
inputForm model = section NotSpaced []
    [ container [ onSubmit Submit ]
      [ demoSection "Register.." [ onSubmit Submit ]
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
            , option [] [ text "Transportation" ]
            , option [] [ text "Fashon" ]
            , option [] [ text "Medical" ]
            , option [] [ text "Other" ]
            ]
          ]
        , field []
          [ controlLabel [] [ text "How much is it" ]
          , controlInput controlInputModifiers [ onInput InputAmount ] [ placeholder "price", value model.amount ] [] 
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

demoArticle : String -> List (Html Msg) -> Html Msg
demoArticle aTitle someHtmls
  = columns columnsModifiers []
    [ column (myColumnModifiers Auto (Just Width2)) []
      [ title H4 [] [ strong [] [ text aTitle ] ]
      ]
    , column (myColumnModifiers Auto (Just Auto)) []
      someHtmls
    ]

demoSection : String -> List (Attribute Msg) -> List (Html Msg) -> Html Msg
demoSection aSubtitle someAttrs someHtmls
  = columns columnsModifiers someAttrs
    [ column (myColumnModifiers Auto (Just Width3)) []
      [ subtitle H4 [] [ text aSubtitle ]
      ]
    , column (myColumnModifiers Auto (Just Auto)) []
      someHtmls
    ]

