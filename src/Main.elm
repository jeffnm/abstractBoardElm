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
import Html exposing (Html, main_, p, text)
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
    , editing : Bool
    , gameActive : Bool
    , gameEnded : Bool
    , boardType : Maybe BoardType
    , pieces : List Pieces
    }


init : () -> ( Model, Cmd Msg )
init flags =
    ( Model "Abstract Board" Nothing False False False Nothing [ { id = 0, x = 25, y = 50, r = 5, color = Color.black, isMoving = False }, { id = 1, x = 50, y = 70, r = 10, color = Color.black, isMoving = False }, { id = 2, x = 100, y = 100, r = 20, color = Color.black, isMoving = False } ], Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- UPDATE


type Msg
    = EditForm
    | UpdateName String
    | StartGame
    | QuitGame
    | ChangeBoard (Maybe BoardType)
    | CanvasPointerDown ( Float, Float )
    | CanvasPointerUp ( Float, Float )
    | CanvasPointerMove ( Float, Float )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditForm ->
            case model.username of
                Just string ->
                    if string == "" then
                        ( { model | username = Nothing }, Cmd.none )

                    else
                        toggleForm model

                Nothing ->
                    toggleForm model

        UpdateName newName ->
            ( { model | username = Just newName }, Cmd.none )

        StartGame ->
            if model.boardType == Nothing then
                ( { model | gameActive = False }, Cmd.none )
                -- Cmd should tell view there is an error

            else
                ( { model | gameActive = True }, Cmd.none )

        QuitGame ->
            ( { model | gameActive = False, gameEnded = True }, Cmd.none )

        ChangeBoard newBoard ->
            ( { model | boardType = newBoard }, Cmd.none )

        CanvasPointerDown event ->
            -- Examples
            --( { model | username = String.fromFloat (Tuple.first event) ++ "  " ++ String.fromFloat (Tuple.second event) }, Cmd.none )
            --( { model | pieces = List.map (\p -> { x = p.x + 1, y = p.y + 1, r = p.r }) model.pieces }, Cmd.none )
            -- Function to take the event and model and return a model
            selectPiece event model

        CanvasPointerUp event ->
            -- Example
            --( { model | username = String.fromFloat (Tuple.first event) ++ "  " ++ String.fromFloat (Tuple.second event) }, Cmd.none )
            deselectPiece event model

        CanvasPointerMove event ->
            moveSelectedPiece event model


toggleForm : Model -> ( Model, Cmd Msg )
toggleForm model =
    ( { model | editing = not model.editing }, Cmd.none )


updatePiece : Model -> Pieces -> ( Model, Cmd Msg )
updatePiece model piece =
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
    ( { model | pieces = pieces }, Cmd.none )


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
            updatePiece model (movePiece a event)

        --updatePiece model (togglePieceColor { a | isMoving = True })
        Nothing ->
            ( model, Cmd.none )


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
            updatePiece model (togglePieceColor { a | isMoving = False })

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
            updatePiece model (togglePieceColor { a | isMoving = True })

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
        [ navbarMenu False
            []
            [ navbarStart []
                [ if model.gameActive then
                    navbarItem False [ disableWhileEditing model QuitGame ] [ text "End Game" ]

                  else
                    navbarItem False [ disableWhileEditing model StartGame ] [ text "Start Game" ]
                , if model.gameActive then
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
                ]
            ]
        , navbarEnd []
            [ if model.editing then
                navbarItem False [ onClick EditForm ] [ text "Done Editing" ]

              else
                hoverableNavbarItemDropdown Down
                    []
                    (navbarLink [] [ text (getUsername model) ])
                    [ navbarDropdown True Centered [] [ navbarItem True [ onClick EditForm ] [ text "Edit Profile" ] ] ]
            ]
        ]


disableWhileEditing : Model -> Msg -> Html.Attribute Msg
disableWhileEditing model msg =
    if not model.editing then
        onClick msg

    else
        class "disabled"


viewBody : Model -> Html Msg
viewBody model =
    container
        []
        [ if model.editing then
            viewProfileEditForm model

          else
            viewHideProfileEditForm model
        , container [ style "display" "flex", style "justify-content" "center", style "align-items" "center" ] [ viewCanvas model ]
        ]



--validateProfileForm : Model -> Control Msg


viewProfileEditForm : Model -> Control Msg
viewProfileEditForm model =
    container []
        [ container []
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


viewHideProfileEditForm : Model -> Html Msg
viewHideProfileEditForm model =
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
        [ Pointer.onDown (\event -> CanvasPointerDown event.pointer.offsetPos), Pointer.onUp (\event -> CanvasPointerUp event.pointer.offsetPos), Pointer.onMove (\event -> CanvasPointerMove event.pointer.offsetPos), style "border" "10px solid green", style "width" (String.fromInt width ++ "px"), style "height" (String.fromInt height ++ "px") ]
        (currentCanvas model width height)


currentCanvas : Model -> Int -> Int -> List Canvas.Renderable
currentCanvas model width height =
    if model.gameActive then
        gameCanvas model width height

    else
        welcomeCanvas model width height


welcomeCanvas : Model -> Int -> Int -> List Canvas.Renderable
welcomeCanvas model width height =
    [ shapes [ fill Color.black ] [ rect ( 0, 0 ) (toFloat width) (toFloat height) ]
    , renderGreeting (getUsername model) model.gameEnded width height
    ]


gameCanvas : Model -> Int -> Int -> List Canvas.Renderable
gameCanvas model width height =
    -- make it white
    -- write boardType on the canvas
    [ shapes [ fill Color.white ] [ rect ( 0, 0 ) (toFloat width) (toFloat height) ]
    , renderBoard model.boardType width height
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
renderGreeting username gameEnded width height =
    let
        greeting =
            if gameEnded then
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
