module State exposing (..)

import Array
import Json.Decode as D
import Ports


type HostMsg
    = ShowHostOptions
    | SetGameText String
    | StartGame


type GuestMsg
    = InitGuestGame
    | ConnectedToHost GameId


type Msg
    = HostMsg HostMsg
    | GuestMsg GuestMsg
    | ResetToIntro
    | ClearResetModal
    | SetHostIdForGuest GameId
    | SetUserName PlayerName
    | ReceivedGameMessage (Result D.Error GameMessage)
    | SetTokenState TokenPosition TokenState
    | SetGameAction GameAction



-- MODEL


type TokenState
    = Default
    | Circled
    | Obscured


type alias TokenPosition =
    -- lineIndex, tokenIndex
    ( Int, Int )


type alias Token =
    { content : String
    , state : TokenState
    }


type alias TextLine =
    Array.Array Token


type alias Poem =
    Array.Array TextLine


actionsPerTurn : PlayerActions
actionsPerTurn =
    RemainingActions 3


type PlayerActions
    = RemainingActions Int
    | Passed


type alias GameId =
    String


type alias ConnectionId =
    String


type alias PlayerName =
    String


type alias PlayerData =
    { name : PlayerName
    , actions : PlayerActions
    }


type alias OtherPlayersList =
    List Player


type alias AllPlayersList =
    List Player


type GameMessage
    = GuestJoined PlayerName
    | UpdatePlayerList AllPlayersList
    | GameAction Poem AllPlayersList
    | Disconnection (Maybe PlayerName)


type GameAction
    = ToggleCircled
    | ToggleObscured



-- TODO divide these by host/guest?


type GamePhase
    = NotStarted
    | ShowingHostOptions
    | ConnectingAsGuest
    | ConnectedAsGuest
    | InGame Poem
    | GameOver


type Player
    = Host PlayerData
    | Guest PlayerData (Maybe GameId)


type alias ConfirmResetFlag =
    Bool


type alias Model =
    { gameId : GameId
    , player : Player
    , otherPlayers : OtherPlayersList
    , gamePhase : GamePhase
    , gameAction : GameAction
    , textString : String
    , confirmReset : ConfirmResetFlag
    }



-- HELPERS


nameOfPlayer : Player -> String
nameOfPlayer =
    dataForPlayer >> .name


actionsForPlayer : Player -> PlayerActions
actionsForPlayer =
    dataForPlayer >> .actions


actionCountForPlayer : Player -> Int
actionCountForPlayer player =
    case actionsForPlayer player of
        Passed ->
            0

        RemainingActions actions ->
            actions


deductAction : Player -> Player
deductAction player =
    let
        newActions =
            case actionsForPlayer player of
                Passed ->
                    Passed

                RemainingActions actions ->
                    RemainingActions (actions - 1)
    in
    case player of
        Host data ->
            Host { data | actions = newActions }

        Guest data gameId ->
            Guest { data | actions = newActions } gameId


dataForPlayer : Player -> PlayerData
dataForPlayer player =
    case player of
        Host playerData ->
            playerData

        Guest playerData _ ->
            playerData


getOtherPlayers : AllPlayersList -> Player -> OtherPlayersList
getOtherPlayers allPlayers currentPlayer =
    List.filter (\player -> nameOfPlayer player /= nameOfPlayer currentPlayer) allPlayers



-- CONSTRUCTORS


startingPlayer : Player
startingPlayer =
    Guest { name = "", actions = actionsPerTurn } Nothing


init : String -> ( Model, Cmd Msg )
init gameId =
    ( { gameId = gameId
      , player = startingPlayer
      , otherPlayers = []
      , gamePhase = NotStarted
      , gameAction = ToggleObscured
      , textString = ""
      , confirmReset = False
      }
    , Ports.init gameId
    )