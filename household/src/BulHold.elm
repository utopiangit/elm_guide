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

import Html exposing ( Html, Attribute, main_, span, a, p, img ,br, text, strong, option, small, input, i, thead, tr, td, th, a)
import Html.Attributes exposing ( attribute, style, src, placeholder, type_, href, rel, class )
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

main : Program () Model Msg
main
  = Browser.sandbox
    { init = init
    , view = view
    , update = \msg -> \model -> model
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
    , inputForm
    ]

inputForm : Html Msg
inputForm = section NotSpaced []
    [ container []
      [ demoSection "Shopping.." [ onSubmit Submit ]
        [ field []
          [ controlLabel [] [ text "Who you are" ]
          , controlSelect controlSelectModifiers [ onInput InputUser ] []
            [ option [] [ text "Yuucha" ]
            , option [] [ text "Naocha" ]
            ]
          ]
        , field []
          [ controlLabel [] [ text "When" ]
          , controlInput controlInputModifiers [ type_ "date" ] [ placeholder "Date" ] [] 
          ]
        , field []
          [ controlLabel [] [ text "What you bought" ]
          , controlInput controlInputModifiers [] [ placeholder "Item" ] [] 
          , controlHelp Default [] [ text "This field isn't required!" ]
          ]
        , field []
          [ controlLabel [] [ text "Category" ]
          , controlSelect controlSelectModifiers [] []
            [ option [] [ text "Food" ]
            , option [] [ text "EatOut" ]
            , option [] [ text "Transportation" ]
            , option [] [ text "Fashon" ]
            , option [] [ text "Medical" ]
            , option [] [ text "Other" ]
            ]
          ]
        , field []
          [ controlLabel [] [ text "Where you bought" ]
          , controlInput controlInputModifiers [] [ placeholder "Shop name" ] [] 
          ]
        , field []
          [ controlLabel [] [ text "How much is it" ]
          , controlInput controlInputModifiers [] [ placeholder "price" ] [] 
          ]        
        , field []
          [ controlButton { buttonModifiers | color = Link } [] []
            [ text "Button"
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

