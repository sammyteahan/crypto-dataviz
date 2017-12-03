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
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | Echo
    | Subscribe


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        Echo ->
            model ! []

        Subscribe ->
            model ! [ WebSocket.send socketUrl subMessage ]



---- VIEW ----


renderSampleMessage : Html Msg
renderSampleMessage =
    let
        decodedMsg =
            JsonDecoder.decodeString messageDecoder subMessage
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


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        , button [ onClick Subscribe ] [ text "send subscription message" ]
        , div []
            [ renderSampleMessage
            ]
        ]



---- SUBSCRIPTIONS ----


socketUrl : String
socketUrl =
    "wss://ws-feed-public.sandbox.gdax.com"


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


{-| crap I have to json encode this whole thing
-}
sampleSubMsg : String
sampleSubMsg =
    """
    {
        "type": "subscribe",
        "product_ids": [
            "ETH-USD",
            "ETH-EUR"
        ],
        "channels": [
            "level2",
            "heartbeat",
            {
                "name": "ticker",
                "product_ids": [
                    "ETH-BTC",
                    "ETH-GBP"
                ]
            },
        ]
    }
    """


subMessage : String
subMessage =
    """
    {
        "type": "subscribe",
        "product_ids": [
            "ETH-USD",
            "ETH-EUR"
        ],
        "channels": [
            "level2",
            "heartbeat"
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
    , channels = [ "level2" ]
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
