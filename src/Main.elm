module Main exposing (main)

--import Html exposing (..)
--post bulema Html

import Browser
import Bulma.CDN exposing (..)
import Bulma.Columns exposing (..)
import Bulma.Components exposing (..)
import Bulma.Elements exposing (..)
import Bulma.Form exposing (..)
import Bulma.Layout exposing (..)
import Bulma.Modifiers exposing (..)
import Canvas exposing (fill, rect, shapes)
import Color
import Css
import Html exposing (Html, main_, p, span, text)
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


type BoardType
    = Board1
    | Board2


type alias Pieces =
    { id : Int
    , x : Float
    , y : Float
    , r : Float
    , color : Color.Color
    , isMoving : Bool
    }


type alias Model =
    { pageTitle : String
    , username : Maybe String
    , formProfileIsOpen : Bool
    , formPieceIsOpen : Bool
    , formPieceSizeSelectedOption : String
    , pieceSizeOptions : List String
    , pieceColorChoice : String
    , gameIsActive : Bool
    , gameHasEnded : Bool
    , boardType : Maybe BoardType
    , pieces : List Pieces
    , pointer : ( Float, Float )
    }


init : () -> ( Model, Cmd Msg )
init flags =
    ( Model "Abstract Board" Nothing False False "small" [ "small", "medium", "large" ] "black" False False Nothing [] ( 0, 0 ), Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- UPDATE


type Msg
    = FormEditProfile
    | UpdateName String
    | StartGame
    | QuitGame
    | ChangeBoard (Maybe BoardType)
    | FormAddPiece
    | FormUpdatePieceSizeChoice String
    | FormUpdatePieceColorChoice String
    | CreatePiece
    | CanvasPointerDown ( Float, Float )
    | CanvasPointerUp ( Float, Float )
    | CanvasPointerMove ( Float, Float )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FormEditProfile ->
            case model.username of
                Just string ->
                    if string == "" then
                        ( { model | username = Nothing }, Cmd.none )

                    else
                        toggleFormProfile model

                Nothing ->
                    toggleFormProfile model

        FormUpdatePieceSizeChoice string ->
            case string of
                "small" ->
                    ( { model | formPieceSizeSelectedOption = string }, Cmd.none )

                "medium" ->
                    ( { model | formPieceSizeSelectedOption = string }, Cmd.none )

                "large" ->
                    ( { model | formPieceSizeSelectedOption = string }, Cmd.none )

                _ ->
                    ( { model | formPieceSizeSelectedOption = "small" }, Cmd.none )

        FormUpdatePieceColorChoice color ->
            ( { model | pieceColorChoice = color }, Cmd.none )

        UpdateName newName ->
            ( { model | username = Just newName }, Cmd.none )

        CreatePiece ->
            createPiece model

        StartGame ->
            if model.boardType == Nothing then
                ( { model | gameIsActive = False }, Cmd.none )
                -- Cmd should tell view there is an error

            else
                ( { model | gameIsActive = True }, Cmd.none )

        QuitGame ->
            ( { model | gameIsActive = False, gameHasEnded = True, pieces = [], formPieceIsOpen = False }, Cmd.none )

        ChangeBoard newBoard ->
            ( { model | boardType = newBoard }, Cmd.none )

        FormAddPiece ->
            ( { model | formPieceIsOpen = True }, Cmd.none )

        CanvasPointerDown event ->
            -- Function to take the event and model and return a model
            selectPiece event model

        CanvasPointerUp event ->
            deselectPiece event model

        CanvasPointerMove event ->
            moveSelectedPiece event model



-- ( { model | pointer = event }, Cmd.none )


createPiece : Model -> ( Model, Cmd Msg )
createPiece model =
    let
        id =
            List.length model.pieces - 1

        size =
            case model.formPieceSizeSelectedOption of
                "medium" ->
                    10

                "large" ->
                    15

                _ ->
                    5

        color =
            case model.pieceColorChoice of
                "white" ->
                    Color.white

                _ ->
                    Color.black

        piece =
            Pieces id 500 500 size color False

        pieces =
            List.append model.pieces [ piece ]
    in
    ( { model | pieces = pieces }, Cmd.none )


toggleFormProfile : Model -> ( Model, Cmd Msg )
toggleFormProfile model =
    ( { model | formProfileIsOpen = not model.formProfileIsOpen }, Cmd.none )


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


togglePieceColor : Pieces -> Pieces
togglePieceColor piece =
    if piece.color == Color.black then
        { piece | color = Color.gray }

    else if piece.color == Color.gray then
        { piece | color = Color.black }

    else
        { piece | color = Color.black }


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
            updatePiece model (movePiece a model.pointer) event

        --updatePiece model (togglePieceColor { a | isMoving = True })
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
            --else if a.isMoving then
            --    updatePiece model (movePiece a event)
            --else
            updatePiece model (togglePieceColor { a | isMoving = False }) event

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
            --updatePiece model (movePiece a event)
            updatePiece model (togglePieceColor { a | isMoving = True }) event

        Nothing ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = model.pageTitle
    , body =
        [ viewHeader model
        , viewMenu model
        , viewBody model
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
        [ stylesheet
        , container []
            [ Bulma.Elements.title H1
                []
                [ text model.pageTitle ]
            ]
        ]



-- viewMenu elements
-- board/game selection
-- profile
-- ???


viewMenu : Model -> Html Msg
viewMenu model =
    navbar { color = Dark, transparent = False }
        []
        [ navbarBrand [] (navbarBurger False [] [ span [] [], span [] [], span [] [] ]) []
        , navbarMenu True
            []
            [ navbarStart []
                [ if model.gameIsActive then
                    navbarItem False [ disableWhileEditing model QuitGame ] [ text "End Game" ]

                  else
                    navbarItem False [ disableWhileEditing model StartGame ] [ text "Start Game" ]
                , if model.gameIsActive then
                    navbarItem False [ disabled True ] [ viewBoardType model ]

                  else
                    hoverableNavbarItemDropdown Down
                        []
                        (navbarLink
                            [ onClick (ChangeBoard Nothing) ]
                            [ viewBoardType model ]
                        )
                        [ navbarDropdown True
                            Centered
                            []
                            [ navbarItem False
                                [ onClick (ChangeBoard (Just Board1)) ]
                                [ text "Board 1" ]
                            , navbarItem False
                                [ onClick (ChangeBoard (Just Board2)) ]
                                [ text "Board 2" ]
                            ]
                        ]
                , if model.gameIsActive then
                    navbarItem False [ onClick FormAddPiece ] [ text "Add Piece" ]

                  else
                    navbarItem False [ disabled True ] [ text "Add Piece" ]
                ]
            ]
        , navbarEnd []
            [ if model.formProfileIsOpen then
                navbarItem False [ onClick FormEditProfile ] [ text "Done Editing" ]

              else
                hoverableNavbarItemDropdown Down
                    []
                    (navbarLink [] [ text (getUsername model) ])
                    [ navbarDropdown True Centered [] [ navbarItem True [ onClick FormEditProfile ] [ text "Edit Profile" ] ] ]
            ]
        ]


disableWhileEditing : Model -> Msg -> Html.Attribute Msg
disableWhileEditing model msg =
    if not model.formProfileIsOpen then
        onClick msg

    else
        class "disabled"


viewBody : Model -> Html Msg
viewBody model =
    container
        []
        [ if model.formProfileIsOpen then
            viewProfileEditForm model

          else
            viewEmptyContainer
        , if model.formPieceIsOpen then
            viewNewPieceForm model

          else
            viewEmptyContainer
        , container [ style "display" "flex", style "justify-content" "center", style "align-items" "center" ] [ viewCanvas model ]
        ]


viewProfileEditForm : Model -> Control Msg
viewProfileEditForm model =
    container []
        [ container [ class "validation-messages" ]
            [ case model.username of
                Just string ->
                    if string == "" then
                        container [] [ box [ style "color" "red" ] [ text "Username is blank - you'll be Anonymous" ] ]

                    else
                        text ""

                Nothing ->
                    container [] [ box [ style "color" "red" ] [ text "Username is blank - you'll be Anonymous" ] ]
            ]
        , fieldBody []
            [ field []
                [ controlLabel [ for "username" ] [ text "Username" ]
                , controlInput inputModifier [] [ id "username", value (getUsername model), onInput UpdateName ] [ text (getUsername model) ]
                , controlHelp Default [] []
                ]
            ]
        ]


viewNewPieceFormSizeOptions : Model -> List (Html Msg)
viewNewPieceFormSizeOptions model =
    let
        options =
            model.pieceSizeOptions
    in
    List.map
        (\o ->
            if model.formPieceSizeSelectedOption == o then
                Html.option [ value o, selected True ] [ text o ]

            else
                Html.option [ value o ] [ text o ]
        )
        options


viewNewPieceForm : Model -> Html Msg
viewNewPieceForm model =
    container []
        [ container [ class "validation-messages" ]
            []
        , fieldBody
            [ id "add-piece-form" ]
            [ field [ class "is-narrow" ] [ controlSelect controlSelectModifiers [ id "size" ] [ class "piece-size-select", onInput FormUpdatePieceSizeChoice ] (viewNewPieceFormSizeOptions model) ]
            , field [ class "is-narrow" ]
                [ controlSelect controlSelectModifiers
                    [ id "color" ]
                    [ class "piece-size-select", onInput FormUpdatePieceColorChoice ]
                    [ Html.option
                        [ value "white"
                        , if model.pieceColorChoice == "white" then
                            selected True

                          else
                            selected
                                False
                        ]
                        [ text "white" ]
                    , Html.option
                        [ value "black"
                        , if model.pieceColorChoice == "black" then
                            selected True

                          else
                            selected
                                False
                        ]
                        [ text "black" ]
                    ]
                ]
            , field [ class "is-narrow" ]
                [ controlButton buttonModifiers [ id "create-piece" ] [ onClick CreatePiece ] [ text "Create Piece" ]
                ]
            ]
        ]


viewEmptyContainer : Html Msg
viewEmptyContainer =
    container [] []


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
    footer [ style "padding" "1em", style "border" "1px solid green" ] [ text "By Jeff" ]



-- Canvas Management


viewCanvas : Model -> Html Msg
viewCanvas model =
    let
        width =
            1000

        height =
            600
    in
    Canvas.toHtml ( width, height )
        [ Pointer.onDown (\event -> CanvasPointerDown event.pointer.offsetPos), Pointer.onUp (\event -> CanvasPointerUp event.pointer.offsetPos), Pointer.onMove (\event -> CanvasPointerMove event.pointer.offsetPos), style "width" (String.fromInt width ++ "px"), style "height" (String.fromInt height ++ "px") ]
        (currentCanvas model width height)


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
    , debugMousePointer model.pointer width height
    ]


gameCanvas : Model -> Int -> Int -> List Canvas.Renderable
gameCanvas model width height =
    -- make it white
    -- write boardType on the canvas
    [ shapes [ fill Color.white ] [ rect ( 0, 0 ) (toFloat width) (toFloat height) ]
    , renderBoard model.boardType width height
    , debugMousePointer model.pointer width height
    ]
        ++ List.concat (renderPieces model.pieces)


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


renderBoard : Maybe BoardType -> Int -> Int -> Canvas.Renderable
renderBoard boardType width height =
    let
        board =
            case boardType of
                Just Board1 ->
                    "Board One"

                Just Board2 ->
                    "Board Two"

                Nothing ->
                    "Error! No Board Selected. We shouldn't have let you get here..."
    in
    -- Placeholder for actual board canvas drawing
    --Debug.log ("debug 507: " ++ String.fromInt width)
    --Canvas.text [ Canvas.font { size = 24, family = "serif" }, fill Color.black, Canvas.align Canvas.Center ] ( toFloat width / 2, toFloat height / 2 ) (String.fromInt width)
    Canvas.text [ Canvas.font { size = 24, family = "serif" }, fill Color.black, Canvas.align Canvas.Center ] ( toFloat width / 2, toFloat height / 2 ) board


renderPieces : List Pieces -> List (List Canvas.Renderable)
renderPieces pieces =
    List.map
        (\p ->
            [ Canvas.shapes [ fill p.color ] [ Canvas.circle ( p.x, p.y ) p.r ]
            , Canvas.text [ Canvas.font { size = 14, family = "serif" }, fill Color.black, Canvas.align Canvas.Center ] ( p.x, p.y + 10 + p.r * 2 ) (String.fromFloat p.x ++ ", " ++ String.fromFloat p.y)
            ]
        )
        pieces
