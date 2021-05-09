port module Main exposing (..)

import Browser
import Html
import Html.Attributes
import Html.Events
import Json.Encode
import Json.Decode


-- MAIN


main : Program Json.Decode.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


-- PORTS


port sendMessage : Json.Encode.Value -> Cmd msg
port messageReceiver : (Json.Decode.Value -> msg) -> Sub msg


-- MODEL


type Model =
    Error String
    | Waiting { mypeerid : String , theirpeerid : String }
    | Connected { mypeerid : String , messages : List ChatMessage , draft : String }

type alias ChatMessage = { user : User , message : String }

type User = Me | Peer String


init : Json.Decode.Value -> (Model, Cmd Msg)
init val =
    case Json.Decode.decodeValue Json.Decode.string val of
         Ok peerid -> (Waiting { mypeerid = peerid , theirpeerid = "" }, Cmd.none)
         Err error -> (Error (Json.Decode.errorToString error), Cmd.none)


-- UPDATE


type Msg
    = Received Json.Decode.Value
    | SetTheirPeerId String
    | ConnectToPeer
    | SetDraft String
    | SendDraft
 

type ForeignMessage
    = ReceivedConnected
    | ReceivedMessage ChatMessage


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Error errormsg -> ( Error errormsg, Cmd.none )
        Waiting state -> case msg of
            SetTheirPeerId new -> ( Waiting { state | theirpeerid = new }, Cmd.none )
            ConnectToPeer ->
                ( Connected { mypeerid = state.mypeerid , messages = [] , draft = "" }
                , sendMessage 
                    ( Json.Encode.object
                        [ ( "type", Json.Encode.string "connect" )
                        , ( "peerid", Json.Encode.string state.theirpeerid )
                        ]
                    )
                )
            Received val -> case Json.Decode.decodeValue messageDecoder val of
                Ok ReceivedConnected ->
                    ( Connected { mypeerid = state.mypeerid , messages = [] , draft = "" }
                    , Cmd.none
                    )
                Ok (ReceivedMessage msg_) -> 
                    ( Connected { mypeerid = state.mypeerid , messages = msg_ :: [] , draft = "" }
                    , Cmd.none
                    )
                Err error -> ( Error (Json.Decode.errorToString error), Cmd.none )
            _ -> ( Waiting state, Cmd.none )
        Connected state -> case msg of
            SetDraft new -> ( Connected { state | draft = new }, Cmd.none )
            SendDraft ->
                ( Connected 
                    { state
                    | messages = { user = Me, message = state.draft } :: state.messages
                    , draft = ""
                    }
                , sendMessage
                    ( Json.Encode.object
                        [ ( "type", Json.Encode.string "broadcast" )
                        , ( "message", Json.Encode.object
                            [ ( "peerid", Json.Encode.string state.mypeerid )
                            , ( "message", Json.Encode.string state.draft )
                            ]
                          )
                        ]
                    )
                )
            Received val -> case Json.Decode.decodeValue messageDecoder val of
                Ok ReceivedConnected -> ( Connected state, Cmd.none )
                Ok (ReceivedMessage msg_) -> ( Connected { state | messages = msg_ :: state.messages }, Cmd.none )
                Err error -> ( Error (Json.Decode.errorToString error), Cmd.none )
            _ -> ( Connected state , Cmd.none )
 

messageDecoder: Json.Decode.Decoder ForeignMessage
messageDecoder
    = Json.Decode.field "type" Json.Decode.string
        |> Json.Decode.andThen 
            ( \msgtype ->
                case msgtype of
                    "connected" -> Json.Decode.succeed ReceivedConnected
                    "broadcast" -> Json.Decode.map ReceivedMessage
                        ( Json.Decode.field "message"
                            ( Json.Decode.map2 ChatMessage
                                (Json.Decode.map Peer (Json.Decode.field "peerid" Json.Decode.string))
                                (Json.Decode.field "message" Json.Decode.string)
                            )
                        )
                    _ -> Json.Decode.fail ("unknown type: " ++ msgtype)
            )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ = messageReceiver Received


-- VIEW


view : Model -> Html.Html Msg
view model = 
    case model of
        Error msg -> Html.text ("An error occurred: " ++ msg)
        Waiting state -> Html.div []
            [ Html.h1 [] [ Html.text "WebRTC Chat" ]
            , Html.text ("Your peer id is: " ++ state.mypeerid)
            , Html.input
                [ Html.Attributes.type_ "text"
                , Html.Attributes.placeholder "Their peer id"
                , Html.Events.onInput SetTheirPeerId
                , Html.Attributes.value state.theirpeerid
                ]
                []
            , Html.button [ Html.Events.onClick ConnectToPeer ] [ Html.text "Connect" ]
            ]
        Connected state -> Html.div []
            [ Html.h1 [] [ Html.text "WebRTC Chat" ]
            , Html.input
                [ Html.Attributes.type_ "text"
                , Html.Attributes.placeholder "Write a message"
                , Html.Events.onInput SetDraft
                , Html.Attributes.value state.draft
                ]
                []
            , Html.button [ Html.Events.onClick SendDraft ] [ Html.text "Send" ]
            , Html.ul [] 
                ( List.map 
                    ( \msg -> case msg.user of 
                        Me -> Html.li [] [ Html.text ("You:" ++ msg.message) ]
                        Peer peerid -> Html.li [] [ Html.text ("Peer " ++ peerid ++ ": " ++ msg.message) ]
                    )
                    state.messages
                )
            ]
