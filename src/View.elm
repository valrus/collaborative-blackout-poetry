module View exposing (..)

import Animation
import Array
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import State exposing (..)


sides =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


corners =
    { topLeft = 0
    , topRight = 0
    , bottomLeft = 0
    , bottomRight = 0
    }


buttonStyles : Bool -> List (Element.Attribute Msg)
buttonStyles isEnabled =
    [ Border.color (rgb 0 0 0)
    , Border.width 1
    ]
        ++ roundedBoxStyles
        ++ (if isEnabled then
                []

            else
                [ Font.color (rgb 0.5 0.5 0.5) ]
           )


roundedBoxStyles : List (Element.Attribute Msg)
roundedBoxStyles =
    [ padding 10
    , Border.solid
    , Border.rounded 5
    ]


conditionalButton : { isEnabled : Bool, msg : Msg, labelText : String } -> Element Msg
conditionalButton opts =
    Input.button
        (centerX :: buttonStyles opts.isEnabled)
        { onPress =
            if opts.isEnabled then
                Just opts.msg

            else
                Nothing
        , label = text opts.labelText
        }


gameIdStyles : List (Element.Attribute Msg)
gameIdStyles =
    [ centerX, Font.size 36, Font.family [ Font.monospace ] ]


userNameInput : String -> Element Msg
userNameInput userName =
    Input.text
        [ centerX, width (300 |> px) ]
        { label =
            Input.labelAbove
                [ centerX ]
                (text "Your name")
        , onChange = SetUserName
        , placeholder = Just (Input.placeholder [] (text "Enter name to start"))
        , text = userName
        }


resetToIntroButton : Element Msg
resetToIntroButton =
    Input.button (buttonStyles True) { onPress = Just ResetToIntro, label = text "Back" }


viewActions : Player -> String
viewActions player =
    case actionsForPlayer player of
        Passed ->
            "P"

        RemainingActions actionCount ->
            String.fromInt actionCount


viewPlayerCard : Player -> Element Msg
viewPlayerCard player =
    let
        textAttrs =
            case player of
                Host _ ->
                    [ Font.bold ]

                _ ->
                    []

        borderColor =
            Border.color (rgb 0.8 0.8 0.8)

        cellPadding =
            paddingXY 10 5
    in
    column
        [ width fill, height fill ]
        [ el
            ([ borderColor
             , Border.width 1
             , Border.roundEach { corners | topLeft = 5, topRight = 5 }
             , width fill
             , height shrink
             , cellPadding
             ]
                ++ textAttrs
            )
            (text <| nameOfPlayer player)
        , row
            [ width fill
            , height fill
            ]
            [ el
                [ borderColor
                , Border.widthEach { sides | left = 1, bottom = 1, right = 1 }
                , Border.roundEach { corners | bottomLeft = 5 }
                , cellPadding
                , width (fillPortion 80)
                ]
                (text <| "actions")
            , el
                [ borderColor
                , Border.widthEach { sides | right = 1, bottom = 1 }
                , Border.roundEach { corners | bottomRight = 5 }
                , cellPadding
                , width (fillPortion 20)
                ]
                (el [ centerX ] <| text <| viewActions player)
            ]
        ]


viewPlayerList : AllPlayersList -> Element Msg
viewPlayerList playerList =
    column [ spacing 5, alignLeft, width fill ]
        (el [ width fill ] (text "Players")
            :: List.map viewPlayerCard playerList
        )


defaultFontStyles : List (Element.Attribute Msg)
defaultFontStyles =
    [ Font.size 20
    , Font.family
        [ Font.sansSerif ]
    , Font.color (rgb 0 0 0)
    ]


mainColumnStyles : List (Element.Attribute Msg)
mainColumnStyles =
    [ centerX
    , alignTop
    , spacing 20
    , width (px 600)
    ]


isValidGameId : Maybe GameId -> Bool
isValidGameId =
    Maybe.map (\id -> List.all Char.isLower (String.toList id) && (String.length id == 20))
        >> Maybe.withDefault False


viewIntro : Player -> Toast -> Html Msg
viewIntro player toast =
    let
        gameId =
            case player of
                Guest _ id ->
                    id

                Host _ ->
                    -- Shouldn't ever happen
                    Nothing

        validId =
            isValidGameId gameId
    in
    layout
        [ padding 20
        , viewToast toast
        ]
    <|
        column
            mainColumnStyles
            [ userNameInput <| nameOfPlayer player
            , conditionalButton
                { msg = HostMsg ShowHostOptions
                , isEnabled =
                    (&&)
                        (not (String.isEmpty <| nameOfPlayer player))
                        (String.isEmpty <| Maybe.withDefault "" gameId)
                , labelText = "Host game"
                }
            , Input.text
                gameIdStyles
                { label =
                    Input.labelBelow
                        (centerX :: defaultFontStyles)
                        (conditionalButton
                            { msg = GuestMsg InitGuestGame
                            , isEnabled = validId && not (String.isEmpty <| nameOfPlayer player)
                            , labelText = "Connect to game"
                            }
                        )
                , onChange = SetHostIdForGuest
                , placeholder = Just (Input.placeholder [] (text "Game ID from host"))
                , text = Maybe.withDefault "" gameId
                }
            ]


minTextWords : Int
minTextWords =
    100


isValidPoemString : String -> Bool
isValidPoemString s =
    (String.words s |> List.length) > minTextWords


viewLeftSidebar : AllPlayersList -> Element.Attribute Msg
viewLeftSidebar allPlayers =
    onLeft
        (column
            [ width (px 200), spacing 20, padding 20, Font.family [ Font.sansSerif ] ]
         <|
            [ resetToIntroButton ]
                ++ (if List.isEmpty allPlayers then
                        []

                    else
                        [ viewPlayerList allPlayers ]
                   )
        )


viewHostOptions : String -> GameId -> AllPlayersList -> Html Msg
viewHostOptions textString gameId allPlayers =
    let
        validPoemText =
            isValidPoemString textString
    in
    layout [ padding 20 ] <|
        column
            (mainColumnStyles
                ++ [ viewLeftSidebar allPlayers
                   , spacing 60
                   ]
            )
            [ column [ spacing 20, centerX ]
                [ el [ centerX ] (text "Game ID")
                , el gameIdStyles (text gameId)
                ]
            , column [ spacing 20 ]
                [ Input.multiline
                    [ centerX, width (px 600) ]
                    { onChange = SetGameText >> HostMsg
                    , placeholder = Nothing
                    , text = textString
                    , spellcheck = False
                    , label =
                        Input.labelAbove
                            [ centerX, width fill, paddingEach { sides | bottom = 10 } ]
                            (row [ spaceEvenly, width fill ]
                                [ el [ alignLeft ] (text "Poem starter text")
                                , el [ alignRight ] <|
                                    conditionalButton
                                        { isEnabled = isValidPoemString textString
                                        , msg = HostMsg StartGame
                                        , labelText = "Start game"
                                        }
                                ]
                            )
                    }
                ]
            ]


viewGuestLobby : GamePhase -> AllPlayersList -> Html Msg
viewGuestLobby gamePhase allPlayers =
    layout [ padding 20 ] <|
        column
            (viewLeftSidebar allPlayers
                :: mainColumnStyles
                ++ [ spacing 40 ]
            )
            [ el [ centerX ] <|
                case gamePhase of
                    ConnectedAsGuest ->
                        text "Waiting for host to begin..."

                    _ ->
                        text "Connecting..."
            ]


viewToken : Bool -> GameAction -> Int -> Int -> Token -> Element Msg
viewToken playerHasActions selectedAction lineIndex tokenIndex token =
    let
        ( textOuterAttributes, textAttributes ) =
            case token.state of
                Default ->
                    ( []
                    , []
                    )

                Circled ->
                    ( [ Border.glow (rgb 1.0 0.5 0.5) 2 ]
                    , [ Border.innerGlow (rgb 1.0 0.5 0.5) 2 ]
                    )

                Obscured ->
                    ( []
                    , [ Font.color (rgb 0.9 0.9 0.9) ]
                    )

        tokenStateAfterAction =
            case ( token.state, selectedAction ) of
                ( Circled, ToggleCircled ) ->
                    Default

                ( Obscured, ToggleObscured ) ->
                    Default

                ( _, ToggleCircled ) ->
                    Circled

                ( _, ToggleObscured ) ->
                    Obscured

        clickMsg =
            case playerHasActions of
                True ->
                    SetTokenState ( lineIndex, tokenIndex ) tokenStateAfterAction

                False ->
                    FlashMessage "Out of actions!"
    in
    el
        -- We need an extra wrapper for both outer and inner glow; see
        -- https://github.com/mdgriffith/elm-ui/issues/18
        textOuterAttributes
        (el
            (Events.onClick clickMsg
                :: pointer
                :: textAttributes
            )
            (text token.content)
        )


viewPoemLine : Bool -> GameAction -> Int -> TextLine -> List (Element Msg)
viewPoemLine playerHasActions gameAction lineIndex line =
    List.intersperse (el [] (text " ")) <|
        List.indexedMap
            (viewToken playerHasActions gameAction lineIndex)
            (Array.toList line)


viewEndToken : Token -> Element Msg
viewEndToken token =
    let
        ( textOuterAttributes, textAttributes ) =
            case token.state of
                Default ->
                    ( []
                    , []
                    )

                Circled ->
                    ( [ Border.glow (rgb 1.0 0.5 0.5) 2 ]
                    , [ Border.innerGlow (rgb 1.0 0.5 0.5) 2 ]
                    )

                Obscured ->
                    ( []
                    , [ transparent True ]
                    )
    in
    el
        -- We need an extra wrapper for both outer and inner glow; see
        -- https://github.com/mdgriffith/elm-ui/issues/18
        textOuterAttributes
        (el
            textAttributes
            (text token.content)
        )


viewEndPoemLine : TextLine -> Element Msg
viewEndPoemLine line =
    paragraph [] <|
        List.intersperse (el [] (text " ")) <|
            List.map
                viewEndToken
                (Array.toList line)


boxShadowStyles : List (Element.Attribute Msg)
boxShadowStyles =
    [ Border.shadow { offset = ( 1.0, 2.0 ), blur = 2.0, size = 2.0, color = rgba 0 0 0 0.333 }
    , Border.shadow { offset = ( 2.0, 4.0 ), blur = 4.0, size = 4.0, color = rgba 0 0 0 0.333 }
    , Border.shadow { offset = ( 3.0, 6.0 ), blur = 6.0, size = 6.0, color = rgba 0 0 0 0.333 }
    ]


viewConfirmModal : Player -> Element.Attribute Msg
viewConfirmModal player =
    let
        warningText =
            case player of
                Host _ ->
                    "Are you sure? This will end the game and disconnect all players."

                Guest _ _ ->
                    "Are you sure? This will leave the game."
    in
    inFront <|
        el [ centerX, centerY, width (px 400), height shrink ] <|
            column
                (roundedBoxStyles
                    ++ boxShadowStyles
                    ++ [ centerX, centerY, Background.color (rgb 1.0 1.0 1.0), spacing 20, padding 30 ]
                )
                [ paragraph [] [ text warningText ]
                , row [ width fill, spaceEvenly ]
                    [ Input.button
                        (buttonStyles True ++ [ Border.color (rgb 1.0 0.2 0.2) ])
                        { onPress = Just ResetToIntro, label = text "Yes, quit" }
                    , Input.button
                        (buttonStyles True)
                        { onPress = Just ClearResetModal, label = text "No, stay" }
                    ]
                ]


flashMessageOnChange : String -> GameAction -> Msg
flashMessageOnChange message gameAction =
    FlashMessage message


viewRightSidebar : GameAction -> Bool -> Element.Attribute Msg
viewRightSidebar selectedAction hasActions =
    let
        baseButtonStyles =
            case hasActions of
                False ->
                    alpha 0.5 :: Border.width 2 :: roundedBoxStyles

                True ->
                    Border.width 2 :: roundedBoxStyles

        actionButtonStyles optionState =
            case optionState of
                Input.Idle ->
                    baseButtonStyles ++ [ Border.color (rgb 0.8 0.8 0.8) ]

                Input.Focused ->
                    baseButtonStyles ++ boxShadowStyles

                Input.Selected ->
                    baseButtonStyles ++ [ Border.color (rgb 0 0 0) ]
    in
    onRight
        (column
            [ padding 20 ]
            [ el [ Font.family [ Font.sansSerif ] ] (text "Actions")
            , row
                [ width (px 200), spacing 10 ]
                [ Input.radioRow
                    [ width shrink, spacing 10, paddingXY 0 20 ]
                    { onChange =
                        if hasActions then
                            SetGameAction

                        else
                            flashMessageOnChange "Out of actions!"
                    , selected = Just selectedAction
                    , label = Input.labelHidden "Word modifiers"
                    , options =
                        [ Input.optionWith
                            ToggleObscured
                            (\optionState -> el (alignLeft :: actionButtonStyles optionState) (text "⬛"))
                        , Input.optionWith
                            ToggleCircled
                            (\optionState -> el (alignLeft :: actionButtonStyles optionState) (text "⭕"))
                        ]
                    }
                , el
                    [ paddingXY 0 20 ]
                    (Input.button (actionButtonStyles Input.Idle)
                        { onPress =
                            if hasActions then
                                Just PassTurn

                            else
                                Nothing
                        , label = text "❌"
                        }
                    )
                ]
            ]
        )


animateElement : Animation.State -> List (Attribute Msg)
animateElement style =
    List.map htmlAttribute (Animation.render style)


toastContainerStyles : List (Attribute Msg)
toastContainerStyles =
    [ centerX
    , alignTop
    , padding 40
    , width fill
    ]


toastStyles : List (Attribute Msg)
toastStyles =
    roundedBoxStyles
        ++ [ centerX
           , alignTop
           , Background.color (rgb 0.1 0.1 0.1)
           , Font.color (rgb 1 1 1)
           , width (fill |> maximum 500)
           , padding 20
           ]


viewToast : Toast -> Attribute Msg
viewToast toast =
    inFront <|
        column
            (animateElement toast.style ++ toastContainerStyles)
            [ el
                toastStyles
                (paragraph [] [ text toast.message ])
            ]


poemStyles : List (Attribute Msg)
poemStyles =
    [ spacing 10
    , padding 10
    , Font.family [ Font.typeface "Georgia", Font.serif ]
    ]


viewGame : Poem -> Model -> Html Msg
viewGame poem model =
    let
        playerHasActions =
            actionCountForPlayer model.player > 0
    in
    layout
        ([ padding 20
         ]
            ++ (if model.confirmReset then
                    [ viewConfirmModal model.player ]

                else
                    [ viewToast model.toast ]
               )
        )
    <|
        Element.textColumn
            (viewLeftSidebar (getAllPlayers model)
                :: viewRightSidebar model.gameAction playerHasActions
                :: mainColumnStyles
                ++ poemStyles
            )
            (List.indexedMap
                (\i line ->
                    paragraph
                        []
                        (viewPoemLine playerHasActions model.gameAction i line)
                )
                (Array.toList poem)
            )


getAllPlayers : Model -> AllPlayersList
getAllPlayers model =
    model.player :: model.otherPlayers


viewGameEnd : Poem -> Html Msg
viewGameEnd poem =
    layout
        []
    <|
        Element.textColumn
            (viewLeftSidebar []
                :: mainColumnStyles
                ++ poemStyles
            )
            (List.map viewEndPoemLine
                (Array.toList poem)
            )
