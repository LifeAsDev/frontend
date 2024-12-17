module Main exposing (..)

import Browser
import Data.Product exposing (Image, Product, products)
import Html exposing (Html, button, div, h1, img, li, main_, p, text, ul)
import Html.Attributes exposing (alt, class, for, height, src, width)
import List.Extra exposing (getAt)
import String exposing (fromFloat)



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
    Browser.sandbox { init = init, update = update, view = view }


type alias Item =
    { id : Int
    , quantity : Int
    }


getProductById : Int -> Maybe Product
getProductById id =
    List.Extra.getAt id products


formatFloat : Float -> String
formatFloat number =
    let
        -- Multiplica el número por 100 y redondea al entero más cercano
        rounded =
            round (number * 100)

        -- Convierte el número redondeado a una cadena
        asString =
            String.fromInt rounded

        -- Inserta el punto decimal en la posición correcta
        length =
            String.length asString
    in
    if length > 2 then
        String.slice 0 (length - 2) asString
            ++ "."
            ++ String.slice (length - 2) length asString

    else if length == 2 then
        "0." ++ asString

    else
        "0.0" ++ asString



-- MODEL


type alias Model =
    { error : String
    , items : List Item
    }


init : Model
init =
    { error = ""
    , items = []
    }



-- VIEW


view : Model -> Html msg
view model =
    main_ [ class "container" ]
        [ div [ class "productBox" ]
            [ h1 []
                [ text "Desserts" ]
            , ul
                [ class "productGrid" ]
                (List.map viewProduct products)
            ]
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


viewProduct : Product -> Html msg
viewProduct product =
    li [ class "productItem" ]
        [ img
            [ src product.image.desktop
            , alt product.name
            , class "productImg"
            ]
            []
        , div [ class "productMainBoxData" ]
            [ p [ class "productCategory" ] [ text product.category ]
            , p [ class "productName" ] [ text product.name ]
            , p [ class "productPrice" ] [ text ("$" ++ formatFloat product.price) ]
            ]
        , button [ class "addToCartBtn" ]
            [ img
                [ src "/assets/images/icon-add-to-cart.svg"
                , alt "add to cart"
                , class ""
                ]
                []
            , text "Add to Cart"
            ]
        ]
