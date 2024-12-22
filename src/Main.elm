module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, img, li, main_, p, span, text, ul)
import Html.Attributes exposing (alt, class, classList, for, height, src, width)
import Html.Events exposing (onClick)
import List.Extra exposing (getAt)
import Product.Product exposing (Image, Product, products)
import String exposing (fromFloat)
import Svg exposing (svg)
import Svg.Attributes exposing (d, fill, height, viewBox, width)



-- UPDATE


type Msg
    = Increment Int
    | Decrement Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment productIndex ->
            { model | productSelections = List.Extra.setAt productIndex (Maybe.map ((+) 1) (List.Extra.getAt productIndex model.productSelections) |> Maybe.withDefault 0) model.productSelections }

        Decrement productIndex ->
            let
                currentQuantity =
                    List.Extra.getAt productIndex model.productSelections |> Maybe.withDefault 0

                newQuantity =
                    if currentQuantity > 0 then
                        currentQuantity - 1

                    else
                        0
            in
            { model | productSelections = List.Extra.setAt productIndex newQuantity model.productSelections }



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
    , productSelections : List Int
    }


init : Model
init =
    { error = ""
    , items = []
    , productSelections = List.repeat (List.length products) 0
    }



-- VIEW


view : Model -> Html Msg
view model =
    main_ [ class "container" ]
        [ div [ class "productBox" ]
            [ h1 []
                [ text "Desserts" ]
            , ul
                [ class "productGrid" ]
                (List.indexedMap (viewProduct model) products)
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


viewProduct : Model -> Int -> Product -> Html Msg
viewProduct model index product =
    li [ class "productItem" ]
        [ img
            [ src product.image.desktop
            , alt product.name
            , classList
                [ ( "productImg", True )
                , ( "productImgHighlight"
                  , (List.Extra.getAt index model.productSelections |> Maybe.withDefault 0) > 0
                  )
                ]
            ]
            []
        , div [ class "productMainBoxData" ]
            [ p [ class "productCategory" ] [ text product.category ]
            , p [ class "productName" ] [ text product.name ]
            , p [ class "productPrice" ] [ text ("$" ++ formatFloat product.price) ]
            ]
        , viewAddToCartBtn (List.Extra.getAt index model.productSelections |> Maybe.withDefault 0) index
        ]


viewAddToCartBtn : Int -> Int -> Html Msg
viewAddToCartBtn quantity index =
    if quantity == 0 then
        button [ onClick (Increment index), class "addToCartBtn" ]
            [ img
                [ src "/assets/images/icon-add-to-cart.svg"
                , alt "check"
                , class ""
                ]
                []
            , text "Add to Cart"
            ]

    else
        button
            [ classList
                [ ( "addToCartBtn", True )
                , ( "highlight", True )
                ]
            ]
            [ div
                [ onClick (Decrement index)
                , alt "decrement"
                , class "quantityBtn"
                ]
                [ svg [ Svg.Attributes.width "10", Svg.Attributes.height "2", Svg.Attributes.fill "none", Svg.Attributes.viewBox "0 0 10 2" ]
                    [ Svg.path [ Svg.Attributes.fill "#fff", Svg.Attributes.d "M0 .375h10v1.25H0V.375Z" ] []
                    ]
                ]
            , span [ class "quantityText" ] [ text (String.fromInt quantity) ]
            , div
                [ onClick (Increment index)
                , alt "increment"
                , class "quantityBtn"
                ]
                [ svg [ Svg.Attributes.width "10", Svg.Attributes.height "10", Svg.Attributes.fill "none", Svg.Attributes.viewBox "0 0 10 10" ]
                    [ Svg.path [ Svg.Attributes.fill "#fff", Svg.Attributes.d "M10 4.375H5.625V0h-1.25v4.375H0v1.25h4.375V10h1.25V5.625H10v-1.25Z" ] []
                    ]
                ]
            ]
