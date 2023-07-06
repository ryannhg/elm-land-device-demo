module Shared exposing
    ( Flags, decoder
    , Model, Msg
    , init, update, subscriptions
    )

{-|

@docs Flags, decoder
@docs Model, Msg
@docs init, update, subscriptions

-}

import Browser.Events
import Effect exposing (Effect)
import Element
import Json.Decode
import Route exposing (Route)
import Route.Path
import Shared.Model
import Shared.Msg



-- FLAGS


type alias Flags =
    { window : WindowSize
    }


decoder : Json.Decode.Decoder Flags
decoder =
    Json.Decode.map Flags
        (Json.Decode.field "window" windowSizeDecoder)


type alias WindowSize =
    { width : Int
    , height : Int
    }


windowSizeDecoder : Json.Decode.Decoder WindowSize
windowSizeDecoder =
    Json.Decode.map2 WindowSize
        (Json.Decode.field "width" Json.Decode.float
            |> Json.Decode.map round
        )
        (Json.Decode.field "height" Json.Decode.float
            |> Json.Decode.map round
        )



-- INIT


type alias Model =
    Shared.Model.Model


init : Result Json.Decode.Error Flags -> Route () -> ( Model, Effect Msg )
init flagsResult route =
    let
        flags =
            flagsResult
                |> Result.withDefault
                    { window =
                        { width = 0
                        , height = 0
                        }
                    }
    in
    ( { device = Element.classifyDevice flags.window }
    , Effect.none
    )



-- UPDATE


type alias Msg =
    Shared.Msg.Msg


update : Route () -> Msg -> Model -> ( Model, Effect Msg )
update route msg model =
    case msg of
        Shared.Msg.BrowserResized width height ->
            ( { model
                | device =
                    Element.classifyDevice
                        { width = width
                        , height = height
                        }
              }
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Route () -> Model -> Sub Msg
subscriptions route model =
    Browser.Events.onResize Shared.Msg.BrowserResized
