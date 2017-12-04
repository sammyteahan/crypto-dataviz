module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import WebSocket exposing (listen)
import Json.Encode as Encode exposing (..)
import Json.Decode as JsonDecoder exposing (..)
import Json.Decode.Pipeline as Pipeline exposing (decode, required)


---- MODEL ----


type alias SubscriptionMessage =
    { type_ : String
    , product_ids : List String
    , channels : List String
    }


type alias Model =
    { subscribed : Bool }


init : ( Model, Cmd Msg )
init =
    { subscribed = False } ! []



---- UPDATE ----


type Msg
    = NoOp
    | Echo
    | Subscribe
    | Unsubscribe


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        Echo ->
            model ! []

        Subscribe ->
            { model | subscribed = True } ! [ WebSocket.send socketUrl subscriptionMsg ]

        Unsubscribe ->
            { model | subscribed = False } ! [ WebSocket.send socketUrl unsubMessage ]



---- VIEW ----


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


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , renderStatus model
        , button [ onClick Subscribe ] [ text "send subscription message" ]
        , button [ onClick Unsubscribe ] [ text "unsubscribe" ]
        , div []
            [ renderSampleMessage ]
        ]



---- SUBSCRIPTIONS ----


socketUrl : String
socketUrl =
    "wss://ws-feed.gdax.com"


{-| @todo create a type alias for socket responses, add something to the model,
then create necessary decoders and watch this baby fly
-}
decodeResponse : String -> Msg
decodeResponse message =
    Debug.log message
        Echo


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen socketUrl decodeResponse



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }


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
