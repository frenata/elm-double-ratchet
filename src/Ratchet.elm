port module Ratchet exposing (..)

import Html exposing (..)
import Html.Attributes
import Html.Events exposing (..)
import Json.Decode exposing (string, bool, Decoder)
import Json.Encode
import Json.Decode.Pipeline exposing (decode, required)
import String
import Update.Extra exposing (andThen)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias BitArray =
    List Int


type alias KeyPair =
    { public : PublicKey
    , private : PrivateKey
    }


type alias PublicKey =
    { type_ : String
    , secretKey : Bool
    , point : String
    , curve : String
    }


blankPubKey =
    (PublicKey "" False "" "")


type alias PrivateKey =
    { type_ : String
    , secretKey : Bool
    , exponent : String
    , curve : String
    }


blankPriKey =
    (PrivateKey "" True "" "")


type alias Model =
    { keypair : KeyPair
    , foreignKey : String
    , hash : BitArray
    , rootKey : BitArray
    , receiveKey : BitArray
    , sendKey : BitArray
    }


init : ( Model, Cmd Msg )
init =
    ( (Model (KeyPair blankPubKey blankPriKey)
        ""
        []
        [ 1, 1, 1, 1, 1, 1, 1, 1 ]
        []
        []
      )
    , Cmd.none
    )


type Msg
    = GenerateDH
    | HashKeys
    | NewHashedKeys (List Int)
    | NewKeyPair Json.Decode.Value
    | NewForeignKey String
    | UpdateChain String
    | NewChain ( BitArray, String, BitArray )



--| ReceiveFK
--| Init


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateDH ->
            ( model, generate_keypair (True) )

        HashKeys ->
            ( model
            , hash_keys
                (encodeKeyPair
                    model.foreignKey
                    model.keypair.private
                )
            )

        NewKeyPair value ->
            case getKeyPair value of
                Ok pair ->
                    ( { model | keypair = pair }, Cmd.none )

                err ->
                    ( model, Cmd.none )

        NewForeignKey fk ->
            ( { model | foreignKey = fk }, Cmd.none )

        NewHashedKeys xs ->
            ( { model | hash = xs }, Cmd.none )

        UpdateChain chain ->
            ( model, update_chain ( model.hash, chain, model.rootKey ) )

        NewChain ( root, which, chain ) ->
            case which of
                "receive" ->
                    ( { model | rootKey = root, receiveKey = chain }, Cmd.none )

                "send" ->
                    ( { model | rootKey = root, sendKey = chain }, Cmd.none )

                _ ->
                    ( model, Cmd.none )



{--
        ReceiveFK ->
            ( model, Cmd.none )
                |> andThen update HashKeys
                |> andThen update (UpdateChain "receive")
                |> andThen update GenerateDH
                |> andThen update HashKeys
                |> andThen update (UpdateChain "send")

        Init ->
            ( model, Cmd.none )
                |> andThen update HashKeys
                |> andThen update (UpdateChain "send")
--}


getKeyPair : Json.Decode.Value -> Result String KeyPair
getKeyPair value =
    Json.Decode.decodeValue decodeKeyPair value


decodeKeyPair : Decoder KeyPair
decodeKeyPair =
    decode KeyPair
        |> required "pub" decodePublicKey
        |> required "sec" decodePrivateKey


decodePublicKey : Decoder PublicKey
decodePublicKey =
    decode PublicKey
        |> required "type" string
        |> required "secretKey" bool
        |> required "point" string
        |> required "curve" string


decodePrivateKey : Decoder PrivateKey
decodePrivateKey =
    decode PrivateKey
        |> required "type" string
        |> required "secretKey" bool
        |> required "exponent" string
        |> required "curve" string


encodeKeyPair : String -> PrivateKey -> String
encodeKeyPair public private =
    let
        sec =
            encodePrivateKey private

        pub =
            encodeForeignKey public

        obj =
            Json.Encode.object
                [ ( "pub", pub )
                , ( "sec", sec )
                ]
    in
        Json.Encode.encode 0 obj


encodeForeignKey : String -> Json.Encode.Value
encodeForeignKey point =
    Json.Encode.object
        [ ( "type", Json.Encode.string "elGamal" )
        , ( "secretKey", Json.Encode.bool False )
        , ( "point", Json.Encode.string point )
        , ( "curve", Json.Encode.string "c192" )
        ]


encodePublicKey : PublicKey -> String
encodePublicKey pk =
    let
        obj =
            Json.Encode.object
                [ ( "type", Json.Encode.string pk.type_ )
                , ( "secretKey", Json.Encode.bool pk.secretKey )
                , ( "point", Json.Encode.string pk.point )
                , ( "curve", Json.Encode.string pk.curve )
                ]
    in
        Json.Encode.encode 0 obj


encodePrivateKey : PrivateKey -> Json.Encode.Value
encodePrivateKey pk =
    Json.Encode.object
        [ ( "type", Json.Encode.string pk.type_ )
        , ( "secretKey", Json.Encode.bool pk.secretKey )
        , ( "exponent", Json.Encode.string pk.exponent )
        , ( "curve", Json.Encode.string pk.curve )
        ]



{- split : String -> Point
   split pub =
       let
           n =
               Debug.log "public: " pub
                   |> String.length
                   |> flip (//) 2
       in
           (Point (String.left n pub) (String.dropLeft n pub))
-}


port generate_keypair : Bool -> Cmd msg


port generated_keypair : (Json.Decode.Value -> msg) -> Sub msg


port hash_keys : String -> Cmd msg


port hashed_keys : (BitArray -> msg) -> Sub msg


port update_chain : ( BitArray, String, BitArray ) -> Cmd msg


port new_chain : (( BitArray, String, BitArray ) -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ generated_keypair NewKeyPair
        , hashed_keys NewHashedKeys
        , new_chain NewChain
        ]


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick GenerateDH ] [ text "Generate Keys" ]
        , button [ onClick HashKeys ] [ text "Hash Keys" ]
        , button [ onClick (UpdateChain "receive") ] [ text "Update Receive Chain" ]
        , button [ onClick (UpdateChain "send") ] [ text "Update Send Chain" ]

        --, button [ onClick Init ] [ text "Init" ]
        --, button [ onClick ReceiveFK ] [ text "ReceiveFK" ]
        , input [ onInput NewForeignKey ] []
        , viewInternals model
        ]


viewInternals : Model -> Html Msg
viewInternals model =
    ul []
        [ li [] [ text <| "Public: " ++ model.keypair.public.point ]
        , li [] [ text <| "Secret: " ++ model.keypair.private.exponent ]
        , li [] [ text <| "Hash:   " ++ toString model.hash ]
        , li [] [ text <| "Root:   " ++ toString model.rootKey ]
        , li [] [ text <| "Receive:" ++ toString model.receiveKey ]
        , li [] [ text <| "Send:   " ++ toString model.sendKey ]
        ]
