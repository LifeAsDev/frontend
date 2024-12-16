module Main exposing (..)
import Browser
import Html exposing (Html, div, text,li,ul,h1)
import Data.Product exposing (Product, Image, products)
import List.Extra exposing (getAt)


-- UPDATE
type Msg
    = NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
         model

-- MAIN

main =
  Browser.sandbox { init = init,update = update , view = view }


type alias Item =
  { id : Int
  , quantity : Int
  }

getProductById : Int -> Maybe Product
getProductById id =
  List.Extra.getAt id products
    
-- MODEL

type alias Model =    { 
     error : String
    ,items : List Item
    }

init : Model
init =  { 
     error = ""
    , items = [{id=1,quantity=1}]
    }
   


  -- VIEW

view : Model -> Html msg
view model =
    div []
        [ h1 [] [ text "Lista de asdada" ]
        , ul []
            (List.map viewItem model.items)
        ]


viewItem : Item -> Html msg
viewItem item =   
 case getProductById item.id of
        Just product ->
            li [] [ text product.name ]

        Nothing ->
            li [] [ text "Product not found" ]