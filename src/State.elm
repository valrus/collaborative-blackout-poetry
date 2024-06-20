module State exposing (..)

import Animation
import Array
import Json.Decode as D
import List.Nonempty exposing (Nonempty)
import Ports
import Process
import Util exposing (IndexRange, IndexSlice)


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
    | EndGame
      -- Game actions
    | PassTurn
    | SetGameAction GameAction
    | SetToken TokenPosition Token
      -- Long press handling
    | StartLongPressTimer TokenSpec
    | SetLongPressTimerId Process.Id
    | HandleLongPress TokenSpec
    | CharSelectStart Int
    | CharSelectDrag Int
    | CharSelectEnd
    | CancelLongPressTimer
    | CancelZoom
      --
    | FlashMessage String
    | AnimateToast Animation.Msg



-- MODEL


type TokenState
    = Default
    | Circled
    | Obscured


type alias TokenPosition =
    -- lineIndex, tokenIndex
    ( Int, Int )


type alias Subtoken =
    { slice : IndexSlice
    , state : TokenState
    }


type alias TokenSpec =
    { token : Token
    , position : TokenPosition
    }



-- should this be a list of strings with states?
-- or a single string and a list of records with indexes and states?


type alias Token =
    { content : String
    , subtokens : Nonempty Subtoken
    }


type alias TextLine =
    Array.Array Token


type alias Poem =
    Array.Array TextLine


type TurnState
    = AllActionsDepleted
    | AllPlayersPassed
    | Ongoing


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
    | GameEnd Poem
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
    | GameOver Poem


type Player
    = Host PlayerData
    | Guest PlayerData (Maybe GameId)


type alias ConfirmResetFlag =
    Bool


type alias Toast =
    { message : String
    , style : Animation.State
    }


type alias Model =
    { gameId : GameId
    , player : Player
    , otherPlayers : OtherPlayersList
    , gamePhase : GamePhase
    , gameAction : GameAction
    , textString : String
    , confirmReset : ConfirmResetFlag
    , toast : Toast
    , longPressTimerId : Maybe Process.Id
    , zoomedToken : Maybe TokenSpec
    , zoomedTokenRange : Maybe IndexRange
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


subtokenize : Subtoken -> String -> String
subtokenize subtoken =
    String.slice subtoken.slice.start subtoken.slice.end


updateTokenState : GameAction -> TokenState -> TokenState
updateTokenState action state =
    case ( state, action ) of
        ( Circled, ToggleCircled ) ->
            Default

        ( Obscured, ToggleObscured ) ->
            Default

        ( _, ToggleCircled ) ->
            Circled

        ( _, ToggleObscured ) ->
            Obscured



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
      , toast =
            { message = ""
            , style =
                Animation.style
                    [ Animation.opacity 1.0
                    , Animation.display Animation.none
                    ]
            }
      , longPressTimerId = Nothing
      , zoomedToken = Nothing
      , zoomedTokenRange = Nothing
      }
    , Ports.init gameId
    )
