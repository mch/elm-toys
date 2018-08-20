module Model exposing (..)


type alias Overlaps =
    List ( EntityId, EntityId )


type alias Model =
    { nextEntityId : EntityId
    , pings : Dict.Dict EntityId Ping
    , targets : Dict.Dict EntityId Target
    , fades : Dict.Dict EntityId FadeableIntensity
    , score : Int
    , previousTick : Time
    , overlaps : Overlaps
    , newOverlaps : Overlaps
    }


init : ( Model, Cmd Msg )
init =
    let
        initialEntityId =
            0

        initialTarget =
            Target blue ( 0, 0 ) 20 100

        targetFade =
            FadeableIntensity 0.0 (Tween 0.0 0.0 0.0 0.0 Ease.linear)
    in
        ( Model
            1
            Dict.empty
            (Dict.singleton 0 initialTarget)
            (Dict.singleton 0 targetFade)
            0
            0
            []
            []
        , Cmd.none
        )
