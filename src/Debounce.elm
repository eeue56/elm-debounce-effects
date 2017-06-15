effect module Debounce where { command = MyCmd, subscription = MySub } exposing (..)


debounce : Int -> msg -> Cmd msg
debounce timeout msg =
    command (Queue timeout msg)


type MyCmd msg
    = Queue Int msg


cmdMap : (a -> b) -> MyCmd a -> MyCmd b
cmdMap f cmd =
    case cmd of
        Queue timeout msg ->
            Queue timeout (f msg)


type MySub msg
    = All


subMap : (a -> b) -> MySub a -> MySub b
subMap func sub =
    case sub of
        All ->
            All


type TimeoutFunction = 
    TimeoutFunction 


type alias State msg =
    { msgs : List msg
    , 
    }


init : Task Never (State msg)
init =
    Task.succeed (State { msgs = [] })


onEffects :
    Platform.Router msg Msg
    -> List (MyCmd msg)
    -> List (MySub msg)
    -> State msg
    -> Task Never (State msg)
onEffects router cmds subs state =
    let
        sendMessagesGetNewQueues =
            sendMessagesHelp router cmds

        subsFromCmds =
            getNewSubs cmds

        newSubs =
            buildSubDict (subs ++ subsFromCmds) Dict.empty
    in
        sendMessagesGetNewQueues
            |> Task.map (\nothing -> { subs = newSubs })


type Msg
    = Receive ( String, String ) Json.Value


onSelfMsg : Platform.Router msg Msg -> Msg -> State msg -> Task x (State msg)
onSelfMsg router selfMsg state =
    case selfMsg of
        Receive ( method, url ) msg ->
            let
                sends =
                    Dict.get ( method, url ) state.subs
                        |> Maybe.withDefault []
                        |> List.map (\tagger -> Platform.sendToApp router (tagger msg))
            in
                Task.sequence sends &> Task.succeed state
