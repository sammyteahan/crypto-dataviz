module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import WebSocket exposing (listen)
import Json.Encode as Encode exposing (..)
import Json.Decode as JsonDecoder exposing (..)
import Json.Decode.Pipeline as Pipeline exposing (decode, required)


---- ¡MODEL ----


type alias SubscriptionMessage =
    { type_ : String
    , product_ids : List String
    , channels : List String
    }


{-| Responses are always in strings. Would be nice to parse them
into floats as they should be represented. Is the decoder the
best place to do that?
-}
type alias GdaxResponse =
    { type_ : String
    , product_id : String
    , price : String
    , volume_24 : String
    , high_24h : String
    }


type alias Model =
    { subscribed : Bool
    , responses : List SubscriptionMessage
    , prices : List GdaxResponse
    }


init : ( Model, Cmd Msg )
init =
    { subscribed = False, responses = [], prices = [] } ! [ WebSocket.send socketUrl subscriptionMsg ]



---- ¡UPDATE ----


type Msg
    = NoOp
    | Echo GdaxResponse
    | ErrorHandler String
    | Unsubscribe


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        Echo message ->
            let
                newPrices =
                    message :: model.prices
            in
                { model | prices = newPrices } ! []

        ErrorHandler error ->
            Debug.log error
                model
                ! []

        Unsubscribe ->
            { model | subscribed = False } ! [ WebSocket.send socketUrl unsubMessage ]



---- ¡VIEW ----


renderSampleMessage : Html Msg
renderSampleMessage =
    let
        decodedMsg =
            JsonDecoder.decodeString messageDecoder subscriptionMsg
    in
        case decodedMsg of
            Ok msg ->
                div []
                    [ p [] [ text <| toString <| decodedMsg ]
                    ]

            Err errMsg ->
                div []
                    [ p [] [ text "something went way wrong" ]
                    , p [] [ text errMsg ]
                    ]


renderStatus : Model -> Html Msg
renderStatus model =
    case model.subscribed of
        True ->
            div [] [ h1 [] [ text "You are subscribed!" ] ]

        False ->
            div [] [ h1 [] [ text "Not connected to socket stream" ] ]


renderPrices : Model -> Html Msg
renderPrices model =
    model.prices
        |> List.map (renderPrice)
        |> ul
            [ style
                [ ( "height", "50vh" )
                , ( "overflow", "hidden" )
                , ( "padding", "0" )
                ]
            ]


renderPrice : GdaxResponse -> Html Msg
renderPrice price =
    li
        [ style
            [ ( "color", "#15232c" )
            , ( "list-style", "none" )
            ]
        ]
        [ text price.price ]


renderActions : Html Msg
renderActions =
    div []
        [ button [ onClick Unsubscribe ] [ text "unsubscribe" ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ renderStatus model
        , renderSampleMessage
        , renderActions
        , div [] [ renderPrices model ]
        ]



---- ¡SUBSCRIPTIONS ----


socketUrl : String
socketUrl =
    "wss://ws-feed.gdax.com"


{-| When we send a message to unsubscribe, we get back a response
that looks like the SubscriptionMessage type alias. Therefore,
I may need a type alias that combines those two types so I can
decide which one I need to use here. Either that or I need to
handle the websocket responses in entirely different functions
-}
decodeResponse : String -> Msg
decodeResponse message =
    let
        decodedMessage =
            JsonDecoder.decodeString responseDecoder message
    in
        case decodedMessage of
            Ok message ->
                Echo message

            Err error ->
                ErrorHandler error


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen socketUrl decodeResponse



---- ¡PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }


responseDecoder : JsonDecoder.Decoder GdaxResponse
responseDecoder =
    Pipeline.decode GdaxResponse
        |> Pipeline.required "type" JsonDecoder.string
        |> Pipeline.required "product_id" JsonDecoder.string
        |> Pipeline.required "price" JsonDecoder.string
        |> Pipeline.required "volume_24h" JsonDecoder.string
        |> Pipeline.required "high_24h" JsonDecoder.string


messageDecoder : JsonDecoder.Decoder SubscriptionMessage
messageDecoder =
    Pipeline.decode SubscriptionMessage
        |> Pipeline.required "type" JsonDecoder.string
        |> Pipeline.required "product_ids" (JsonDecoder.list JsonDecoder.string)
        |> Pipeline.required "channels" (JsonDecoder.list JsonDecoder.string)


subMsg : SubscriptionMessage
subMsg =
    { type_ = "subscribe"
    , product_ids = [ "ETH-USD", "BTC-USD" ]
    , channels = [ "heartbeat", "level2" ]
    }


subscriptionProducts : List String
subscriptionProducts =
    [ "ETH-USD", "BTC-USD" ]


subscriptionChannels : List String
subscriptionChannels =
    [ "heartbeat", "level2" ]


subscriptionMessage : Encode.Value
subscriptionMessage =
    Encode.object
        [ ( "type", Encode.string "subscribe" )
        , ( "product_ids", Encode.list (List.map Encode.string subscriptionProducts) )
        , ( "channels", Encode.list (List.map Encode.string subscriptionChannels) )
        ]


subscriptionMsg : String
subscriptionMsg =
    """
    {
        "type": "subscribe",
        "product_ids": [
            "BTC-USD"
        ],
        "channels": [
            "ticker"
        ]
    }
    """


unsubMessage : String
unsubMessage =
    """
    {
        "type": "unsubscribe",
        "product_ids": [
            "BTC-USD"
        ],
        "channels": [
            "ticker"
        ]
    }
    """
