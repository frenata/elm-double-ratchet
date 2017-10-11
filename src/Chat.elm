module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import WebSocket


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



--MODEL


type alias Model =
    { input : String
    , messages : List String
    }


init : ( Model, Cmd Msg )
init =
    ( Model "" [], Cmd.none )



--UDPATE


type Msg
    = Input String
    | Send
    | NewMessage String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg { input, messages } =
    case msg of
        Input newInput ->
            ( Model newInput messages, Cmd.none )

        Send ->
            ( Model "" messages
            , WebSocket.send
                "wss://chat-ratchet.herokuapp.com/wss"
                input
            )

        NewMessage str ->
            ( Model input (str :: messages), Cmd.none )



-- SUBS


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen "wss://chat-ratchet.herokuapp.com/wss" NewMessage



--VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [] (List.map viewMessage model.messages)
        , input [ onInput Input ] []
        , button [ onClick Send ] [ text "Send" ]
        ]


viewMessage : String -> Html msg
viewMessage msg =
    div [] [ text msg ]
