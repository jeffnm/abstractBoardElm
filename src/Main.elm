module Main exposing (main)

import Browser
import Bulma.Columns exposing (..)
import Bulma.Components exposing (..)
import Bulma.Elements exposing (..)
import Bulma.Form exposing (..)
import Bulma.Layout exposing (..)
import Bulma.Modifiers exposing (..)
import Canvas exposing (fill, lineTo, path, rect, shapes)
import Color
import Css
import Html exposing (Html, main_, node, p, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Events.Extra.Pointer as Pointer
import Validate exposing (Validator)



-- MAIN


main =
    Browser.document
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { pageTitle : String
    , username : Maybe String
    , formProfileIsOpen : Bool
    , formPieceIsOpen : Bool
    , formPieceSizeSelectedOption : Int
    , formPieceColorChoice : String
    , formRemovePieceIsOpen : Bool
    , gameIsActive : Bool
    , gameHasEnded : Bool
    , boardType : Maybe BoardType
    , pieces : List Pieces
    , pointer : ( Float, Float )
    }


type BoardType
    = Board1
    | Board2


type alias Pieces =
    { id : Int
    , x : Float
    , y : Float
    , r : Float
    , baseColor : Color.Color
    , displayColor : Color.Color
    , isMoving : Bool
    }


type Form
    = ProfileForm
    | AddPieceForm
    | RemovePieceForm


init : () -> ( Model, Cmd Msg )
init flags =
    ( { pageTitle = "Abstract Board"
      , username = Nothing
      , formProfileIsOpen = False
      , formPieceIsOpen = False
      , formPieceSizeSelectedOption = 10
      , formRemovePieceIsOpen = False

      -- , formPieceSizeOptions = [ "small", "medium", "large" ]
      , formPieceColorChoice = "black"
      , gameIsActive = False
      , gameHasEnded = False
      , boardType = Nothing
      , pieces = []
      , pointer = ( 0, 0 )
      }
    , Cmd.none
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- UPDATE


type Msg
    = ToggledEditProfileForm
    | UpdatedUsername String
    | StartedGame
    | EndedGame
    | ChangedBoard (Maybe BoardType)
    | ToggledAddPieceForm
    | ToggleRemovePieceForm
    | UpdatedAddPieceFormSizeChoice String
    | UpdatedAddPieceFormColorChoice String
    | CreatedPiece
    | CanvasPointerDown ( Float, Float )
    | CanvasPointerUp ( Float, Float )
    | CanvasPointerMove ( Float, Float )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggledEditProfileForm ->
            case model.username of
                Just string ->
                    if string == "" then
                        ( { model | username = Nothing }, Cmd.none )

                    else
                        toggleForm ProfileForm model

                Nothing ->
                    toggleForm ProfileForm model

        ToggledAddPieceForm ->
            toggleForm AddPieceForm model

        ToggleRemovePieceForm ->
            toggleForm RemovePieceForm model

        UpdatedAddPieceFormSizeChoice number ->
            case String.toInt number of
                Just val ->
                    ( { model | formPieceSizeSelectedOption = val }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        UpdatedAddPieceFormColorChoice color ->
            ( { model | formPieceColorChoice = color }, Cmd.none )

        UpdatedUsername newName ->
            ( { model | username = Just newName }, Cmd.none )

        CreatedPiece ->
            createPiece model

        StartedGame ->
            case model.boardType of
                Just boardType ->
                    ( { model | gameIsActive = True }, Cmd.none )

                -- Should Cmd tell view there is an error? Or is that a model update?
                Nothing ->
                    ( { model | gameIsActive = False }, Cmd.none )

        EndedGame ->
            -- Make all model changes needed when a game ends
            ( { model | gameIsActive = False, gameHasEnded = True, pieces = [], formPieceIsOpen = False }, Cmd.none )

        ChangedBoard newBoard ->
            ( { model | boardType = newBoard }, Cmd.none )

        CanvasPointerDown event ->
            selectPiece event model

        CanvasPointerUp event ->
            deselectPiece event model

        CanvasPointerMove event ->
            moveSelectedPiece event model


createPiece : Model -> ( Model, Cmd Msg )
createPiece model =
    let
        id =
            List.length model.pieces

        radius =
            toFloat model.formPieceSizeSelectedOption

        color =
            case model.formPieceColorChoice of
                "white" ->
                    Color.white

                _ ->
                    Color.black

        piece =
            Pieces id 500 250 radius color color False

        pieces =
            List.append model.pieces [ piece ]
    in
    ( { model | pieces = pieces }, Cmd.none )


toggleForm : Form -> Model -> ( Model, Cmd Msg )
toggleForm formToToggle model =
    case formToToggle of
        ProfileForm ->
            ( { model | formProfileIsOpen = not model.formProfileIsOpen }, Cmd.none )

        AddPieceForm ->
            ( { model | formPieceIsOpen = not model.formPieceIsOpen }, Cmd.none )

        RemovePieceForm ->
            ( { model | formRemovePieceIsOpen = not model.formRemovePieceIsOpen }, Cmd.none )


updatePiece : Model -> Pieces -> ( Float, Float ) -> ( Model, Cmd Msg )
updatePiece model piece event =
    let
        pieces =
            List.map
                (\n ->
                    if n.id == piece.id then
                        piece

                    else
                        n
                )
                model.pieces
    in
    ( { model | pieces = pieces, pointer = event }, Cmd.none )


togglePieceDisplayColor : Pieces -> Pieces
togglePieceDisplayColor piece =
    if piece.baseColor == Color.black && piece.displayColor == Color.black then
        { piece | displayColor = Color.grey }

    else if piece.baseColor == Color.black && piece.displayColor == Color.grey then
        { piece | displayColor = Color.black }

    else if piece.baseColor == Color.white && piece.displayColor == Color.white then
        { piece | displayColor = Color.grey }

    else if piece.baseColor == Color.white && piece.displayColor == Color.grey then
        { piece | displayColor = Color.white }

    else
        { piece | displayColor = Color.black }


movePiece : Pieces -> ( Float, Float ) -> Pieces
movePiece piece ( newX, newY ) =
    { piece | x = newX, y = newY }


moveSelectedPiece : ( Float, Float ) -> Model -> ( Model, Cmd Msg )
moveSelectedPiece event model =
    let
        selectedPiece =
            List.head
                (List.filter
                    (\piece ->
                        if piece.isMoving then
                            True

                        else
                            False
                    )
                    model.pieces
                )
    in
    case selectedPiece of
        Just a ->
            updatePiece model (movePiece a event) event

        Nothing ->
            ( { model | pointer = event }, Cmd.none )


deselectPiece : ( Float, Float ) -> Model -> ( Model, Cmd Msg )
deselectPiece event model =
    let
        selectedPiece =
            List.head
                (List.filter
                    (\piece ->
                        if piece.isMoving then
                            True

                        else
                            False
                    )
                    model.pieces
                )
    in
    case selectedPiece of
        Just a ->
            updatePiece model (togglePieceDisplayColor { a | isMoving = False }) event

        Nothing ->
            ( model, Cmd.none )


selectPiece : ( Float, Float ) -> Model -> ( Model, Cmd Msg )
selectPiece event model =
    let
        pointerX =
            Tuple.first event

        pointerY =
            Tuple.second event

        selectedPiece =
            List.head
                (List.filter
                    (\piece ->
                        if
                            (piece.x > pointerX - piece.r)
                                && (piece.x < pointerX + piece.r)
                                && (piece.y > pointerY - piece.r)
                                && (piece.y < pointerY + piece.r)
                        then
                            True

                        else
                            False
                    )
                    model.pieces
                )
    in
    case selectedPiece of
        Just a ->
            case model.formRemovePieceIsOpen of
                True ->
                    removePiece event model a

                False ->
                    updatePiece model (togglePieceDisplayColor { a | isMoving = True }) event

        Nothing ->
            ( model, Cmd.none )


removePiece : ( Float, Float ) -> Model -> Pieces -> ( Model, Cmd Msg )
removePiece event model piece =
    let
        pieces =
            List.filter
                (\n ->
                    if n.id == piece.id then
                        False

                    else
                        True
                )
                model.pieces
    in
    ( { model | pieces = pieces, pointer = event }, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = model.pageTitle
    , body =
        [ viewHeader model
        , viewMenu model
        , viewMainContent model
        , viewFooter model
        ]
    }


inputModifier : ControlInputModifiers msg
inputModifier =
    { size = Standard
    , state = Active
    , color = Default
    , expanded = False
    , rounded = False
    , readonly = False
    , disabled = False
    , iconLeft = Nothing
    , iconRight = Nothing
    }


viewHeader : Model -> Section Msg
viewHeader model =
    container
        []
        [ node "link" [ rel "stylesheet", href "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.1/css/bulma.min.css" ] []
        , node "link" [ rel "stylesheet", href "https://cdn.jsdelivr.net/npm/bulma-slider@2.0.0/dist/css/bulma-slider.min.css" ] []
        , container []
            [ Bulma.Elements.title H1
                []
                [ text model.pageTitle ]
            ]
        ]


viewMenu : Model -> Html Msg
viewMenu model =
    -- This navbar needs a bunch of work to make it properly responsive.
    -- The management of whether something is expanded or not (other than for hoverableNavbarItemDropdown) is going to need Msg and Update to the Model
    navbar { color = Dark, transparent = False }
        []
        [ navbarBrand [] (navbarBurger False [] [ span [] [], span [] [], span [] [] ]) []
        , navbarMenu True
            []
            [ navbarStart []
                [ if model.gameIsActive then
                    navbarItem False [ disableWhileEditing model EndedGame ] [ text "End Game" ]

                  else
                    navbarItem False [ disableWhileEditing model StartedGame ] [ text "Start Game" ]
                , if model.gameIsActive then
                    navbarItem False [ disabled True, class "disabled" ] [ viewBoardType model ]

                  else
                    hoverableNavbarItemDropdown Down
                        []
                        (navbarLink
                            [ onClick (ChangedBoard Nothing) ]
                            [ viewBoardType model ]
                        )
                        [ navbarDropdown True
                            Left
                            []
                            [ navbarItem False
                                [ onClick (ChangedBoard (Just Board1)) ]
                                [ text "Board 1" ]
                            , navbarItem False
                                [ onClick (ChangedBoard (Just Board2)) ]
                                [ text "Board 2" ]
                            ]
                        ]
                , case model.gameIsActive && not model.formRemovePieceIsOpen of
                    True ->
                        case model.formPieceIsOpen of
                            True ->
                                navbarItem True [ onClick ToggledAddPieceForm ] [ text "Close Pieces Form" ]

                            False ->
                                navbarItem False [ onClick ToggledAddPieceForm ] [ text "Create Pieces" ]

                    False ->
                        navbarItem False [ disabled True ] [ text "" ]
                , case model.gameIsActive && not model.formPieceIsOpen of
                    True ->
                        case model.formRemovePieceIsOpen of
                            True ->
                                navbarItem True [ onClick ToggleRemovePieceForm, style "background-color" "red" ] [ text "Stop Removing Pieces" ]

                            False ->
                                navbarItem False [ onClick ToggleRemovePieceForm ] [ text "Remove Pieces" ]

                    False ->
                        navbarItem False [ disabled True ] [ text "" ]
                ]
            ]
        , navbarEnd []
            [ if model.formProfileIsOpen then
                navbarItem False [ onClick ToggledEditProfileForm ] [ text "Done Editing" ]

              else
                hoverableNavbarItemDropdown Down
                    []
                    (navbarLink [] [ text (getUsername model) ])
                    [ navbarDropdown True Centered [] [ navbarItem True [ onClick ToggledEditProfileForm ] [ text "Edit Profile" ] ] ]
            ]
        ]



-- TODO: Make this function take a form type and use a case pattern match to determine which of the forms to check before doing the if expressions


disableWhileEditing : Model -> Msg -> Html.Attribute Msg
disableWhileEditing model msg =
    if not model.formProfileIsOpen then
        onClick msg

    else
        class "disabled"


viewMainContent : Model -> Html Msg
viewMainContent model =
    Html.div
        []
        [ if model.formProfileIsOpen then
            viewProfileEditForm model

          else
            viewEmptyDiv
        , if model.formPieceIsOpen then
            viewNewPieceForm model

          else
            viewEmptyDiv
        , container [ style "display" "flex", style "justify-content" "center", style "align-items" "center" ] [ viewCanvas model ]
        ]


viewProfileEditForm : Model -> Control Msg
viewProfileEditForm model =
    Html.div [ id "top-form" ]
        [ Html.div [ class "validation-messages" ]
            [ case model.username of
                Just string ->
                    if string == "" then
                        Html.div [] [ box [ style "color" "red" ] [ text "Username is blank - you'll be Anonymous" ] ]

                    else
                        text ""

                Nothing ->
                    Html.div [] [ box [ style "color" "red" ] [ text "Username is blank - you'll be Anonymous" ] ]
            ]
        , fieldBody []
            [ field []
                [ controlLabel [ for "username" ] [ text "Username" ]
                , controlInput inputModifier [] [ id "username", value (getUsername model), onInput UpdatedUsername ] [ text (getUsername model) ]
                , controlHelp Default [] []
                ]
            ]
        ]



-- viewNewPieceFormSizeOptions : Model -> List (Html Msg)
-- viewNewPieceFormSizeOptions model =
-- let
--     options =
--         model.formPieceSizeOptions
-- in
-- List.map
--     (\o ->
--         if model.formPieceSizeSelectedOption == o then
--             Html.option [ value o, selected True ] [ text o ]
--
--         else
--             Html.option [ value o ] [ text o ]
--     )
--     options


viewNewPieceForm : Model -> Html Msg
viewNewPieceForm model =
    Html.div [ id "top-form" ]
        [ Html.div [ class "validation-messages" ]
            []
        , fieldBody
            [ id "add-piece-form" ]
            [ field [ class "is-narrow", class "has-addons" ]
                [ controlLabel [ for "piece-size-slider" ] [ text ("Size: " ++ String.fromInt model.formPieceSizeSelectedOption) ]
                , controlInput inputModifier
                    []
                    [ id "piece-size-slider"
                    , type_ "range"
                    , step "10"
                    , Html.Attributes.min "10"
                    , Html.Attributes.max "50"
                    , class "slider"
                    , class "is-info"
                    , value (String.fromInt model.formPieceSizeSelectedOption)
                    , onInput UpdatedAddPieceFormSizeChoice
                    ]
                    []
                , controlSelect controlSelectModifiers
                    [ id "color" ]
                    [ class "piece-size-select", onInput UpdatedAddPieceFormColorChoice ]
                    [ Html.option
                        [ value "white"
                        , if model.formPieceColorChoice == "white" then
                            selected True

                          else
                            selected
                                False
                        ]
                        [ text "white" ]
                    , Html.option
                        [ value "black"
                        , if model.formPieceColorChoice == "black" then
                            selected True

                          else
                            selected
                                False
                        ]
                        [ text "black" ]
                    ]
                , controlButton buttonModifiers [ id "create-piece" ] [ onClick CreatedPiece ] [ text "Create Piece" ]
                ]
            ]
        ]


viewRemovePieceForm : Model -> Html Msg
viewRemovePieceForm model =
    Html.div [ id "top-form" ]
        [ Html.div [ class "validation-messages" ]
            []
        ]


viewEmptyDiv : Html Msg
viewEmptyDiv =
    Html.div [] []


viewBoardType : Model -> Html Msg
viewBoardType model =
    case model.boardType of
        Just Board1 ->
            text "Board One"

        Just Board2 ->
            text "Board Two"

        Nothing ->
            text "Choose Board"


viewFooter : Model -> Html Msg
viewFooter model =
    footer [ style "padding" "1em", style "text-align" "center" ] [ text "By Jeff" ]



-- Canvas Management


viewCanvas : Model -> Html Msg
viewCanvas model =
    let
        w =
            1000

        h =
            600
    in
    Canvas.toHtml ( w, h )
        [ Pointer.onDown (\event -> CanvasPointerDown event.pointer.offsetPos)
        , Pointer.onUp (\event -> CanvasPointerUp event.pointer.offsetPos)
        , Pointer.onMove (\event -> CanvasPointerMove event.pointer.offsetPos)
        , style "width" (String.fromInt w ++ "px")
        , style "height" (String.fromInt h ++ "px")
        ]
        (currentCanvas model w h)


currentCanvas : Model -> Int -> Int -> List Canvas.Renderable
currentCanvas model width height =
    if model.gameIsActive then
        gameCanvas model width height

    else
        welcomeCanvas model width height


debugMousePointer : ( Float, Float ) -> Int -> Int -> Canvas.Renderable
debugMousePointer event width height =
    Canvas.text [ Canvas.font { size = 24, family = "serif" }, Canvas.fill Color.white, Canvas.stroke Color.black, Canvas.align Canvas.Center ] ( toFloat width - 100, toFloat height - 500 ) (String.fromFloat (Tuple.first event) ++ ", " ++ String.fromFloat (Tuple.second event))


welcomeCanvas : Model -> Int -> Int -> List Canvas.Renderable
welcomeCanvas model width height =
    [ shapes [ fill Color.black ] [ rect ( 0, 0 ) (toFloat width) (toFloat height) ]
    , renderGreeting (getUsername model) model.gameHasEnded width height

    -- , debugMousePointer model.pointer width height
    ]


gameCanvas : Model -> Int -> Int -> List Canvas.Renderable
gameCanvas model width height =
    -- make it white
    -- render the currently selected boardType
    [ shapes [ fill Color.white ] [ rect ( 0, 0 ) (toFloat width) (toFloat height) ]
    ]
        ++ renderBoard model.boardType width height
        ++ List.concat (renderPieces model.pieces)



-- ++ [ debugMousePointer model.pointer width height ]
-- Game Boards


drawTriangle1 : ( Float, Float, Float ) -> Canvas.Shape
drawTriangle1 ( x, y, length ) =
    let
        l =
            length
    in
    path ( x, y ) [ lineTo ( x + l, y ), lineTo ( x + l / 2, y + l ), lineTo ( x, y ) ]


drawTriangle2 : ( Float, Float, Float ) -> Canvas.Shape
drawTriangle2 ( x, y, length ) =
    let
        l =
            length
    in
    path ( x, y ) [ lineTo ( x + l, y ), lineTo ( x + l / 2, y - l ), lineTo ( x, y ) ]


gameBoardOne : Int -> Int -> List Canvas.Renderable
gameBoardOne width height =
    let
        tSize =
            50

        offset =
            tSize * 1.5

        hcount =
            round ((toFloat width / tSize) * 1.25)

        vcount =
            round (toFloat height / tSize)

        columns =
            List.repeat
                hcount
                (negate
                    2
                    * tSize
                )

        rows =
            List.repeat
                vcount
                0

        drawrow y row =
            List.indexedMap
                (\i n ->
                    if modBy 2 row == 0 then
                        ( toFloat (n + i * tSize), y, tSize )

                    else
                        ( (toFloat n + toFloat i * tSize) + offset, y, tSize )
                 -- ( toFloat (n + i * tSize)
                 -- , y
                 -- , tSize
                 -- )
                )
                columns

        triangles =
            List.concat (List.indexedMap (\i n -> drawrow (toFloat (n + (i * tSize))) i) rows)

        -- [ ( 50, y, tSize ), ( 100, y, tSize ), ( 150, y, tSize ), ( 200, y, tSize ) ]
    in
    [ shapes [ fill Color.white, Canvas.stroke Color.black ]
        (List.map
            drawTriangle1
            triangles
         -- ++ List.map drawTriangle2 triangles
        )
    ]


gameBoardTwo : Int -> Int -> List Canvas.Renderable
gameBoardTwo width height =
    let
        tSize =
            200

        offset =
            tSize * 1.5

        hcount =
            round ((toFloat width / tSize) * 2)

        vcount =
            round (toFloat height / tSize)

        columns =
            List.repeat
                hcount
                (negate
                    2
                    * tSize
                )

        rows =
            List.repeat
                vcount
                0

        drawrow y row =
            List.indexedMap
                (\i n ->
                    if modBy 2 row == 0 then
                        ( toFloat (n + i * tSize), y, tSize )

                    else
                        ( (toFloat n + toFloat i * tSize) + offset, y, tSize )
                 -- ( toFloat (n + i * tSize)
                 -- , y
                 -- , tSize
                 -- )
                )
                columns

        triangles =
            List.concat (List.indexedMap (\i n -> drawrow (toFloat (n + (i * tSize))) i) rows)

        -- [ ( 50, y, tSize ), ( 100, y, tSize ), ( 150, y, tSize ), ( 200, y, tSize ) ]
    in
    [ shapes [ fill Color.white, Canvas.stroke Color.black ]
        (List.map
            drawTriangle1
            triangles
         -- ++ List.map drawTriangle2 triangles
        )
    ]


getUsername : Model -> String
getUsername model =
    case model.username of
        Just username ->
            username

        Nothing ->
            "Anonymous"


renderGreeting : String -> Bool -> Int -> Int -> Canvas.Renderable
renderGreeting username gameHasEnded width height =
    let
        greeting =
            if gameHasEnded then
                "Play again, " ++ username ++ "?"

            else
                "Welcome " ++ username ++ "!"
    in
    Canvas.text [ Canvas.font { size = 24, family = "serif" }, fill Color.white, Canvas.align Canvas.Center ] ( toFloat width / 2, toFloat height / 2 ) greeting


renderBoard : Maybe BoardType -> Int -> Int -> List Canvas.Renderable
renderBoard boardType width height =
    case boardType of
        Just Board1 ->
            gameBoardOne width height

        Just Board2 ->
            gameBoardTwo width height

        Nothing ->
            -- default to game board one if this unwante state occurs
            gameBoardOne width height


renderPieces : List Pieces -> List (List Canvas.Renderable)
renderPieces pieces =
    List.map
        (\p ->
            [ Canvas.shapes [ fill p.displayColor, Canvas.stroke Color.black ] [ Canvas.circle ( p.x, p.y ) p.r ]

            -- , Canvas.text [ Canvas.font { size = 14, family = "serif" }, fill Color.black, Canvas.align Canvas.Center ] ( p.x, p.y + 10 + p.r * 2 ) (String.fromFloat p.x ++ ", " ++ String.fromFloat p.y)
            ]
        )
        pieces
