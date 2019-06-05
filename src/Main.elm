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


type alias Model =
    { username : String
    , email : String
    , editing : Bool
    , gameStarted : Bool
    , gameEnded : Bool
    , boardType : Maybe BoardType
    }


nameValidator : Validator String Model
nameValidator =
    Validate.ifBlank .username "Please enter a name "


emailValidator : Validator String Model
emailValidator =
    Validate.ifInvalidEmail .email (\value -> "Email is not valid ")


init : () -> ( Model, Cmd Msg )
init flags =
    ( Model "Anonymous User" "" False False False Nothing, Cmd.none )


type alias Document msg =
    { title : String
    , body : List (Html msg)
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- UPDATE


type Msg
    = EditForm
    | UpdateName String
    | UpdateEmail String -- Should be a custom type that validates email, right?
    | StartGame
    | QuitGame
    | ChangeBoard (Maybe BoardType)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditForm ->
            case Validate.validate nameValidator model of
                Ok notBlankName ->
                    case Validate.validate emailValidator model of
                        Ok validEmail ->
                            if model.editing then
                                ( { model | username = model.username, email = model.email, editing = not model.editing }, Cmd.none )

                            else
                                toggleForm model

                        Err validationErrors ->
                            if model.editing then
                                ( { model | username = model.username, email = "", editing = not model.editing }, Cmd.none )

                            else
                                toggleForm model

                Err validationErrors ->
                    if model.editing then
                        ( model, Cmd.none )

                    else
                        toggleForm model

        UpdateName newName ->
            ( { model | username = newName }, Cmd.none )

        UpdateEmail newEmail ->
            ( { model | email = newEmail }, Cmd.none )

        StartGame ->
            ( { model | gameStarted = True }, Cmd.none )

        QuitGame ->
            ( { model | gameStarted = False, gameEnded = True }, Cmd.none )

        ChangeBoard newBoard ->
            ( { model | boardType = newBoard }, Cmd.none )


toggleForm : Model -> ( Model, Cmd Msg )
toggleForm model =
    ( { model | editing = not model.editing }, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Abstract Board"
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
    section NotSpaced
        []
        [ stylesheet
        , container []
            [ Bulma.Elements.title H1
                []
                [ text "Abstract Board" ]
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
                [ if model.gameStarted then
                    navbarItem False [ disableWhileEditing model QuitGame ] [ text "End Game" ]

                  else
                    navbarItem False [ disableWhileEditing model StartGame ] [ text "Start Game" ]
                , if model.gameStarted then
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
                    (navbarLink [] [ text model.username ])
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
    section NotSpaced
        []
        [ if model.editing then
            viewProfileEditForm model

          else
            viewHideProfileEditForm model
        , container [] [ viewCanvas model ]
        ]



--validateProfileForm : Model -> Control Msg


viewProfileEditForm : Model -> Control Msg
viewProfileEditForm model =
    container []
        [ container []
            [ case Validate.validate nameValidator model of
                Ok validName ->
                    text ""

                Err validationErrors ->
                    container [] [ box [ style "color" "red" ] [ text (List.take 1 validationErrors |> String.concat) ] ]
            , case Validate.validate emailValidator model of
                Ok validEmail ->
                    text ""

                Err validationErrors ->
                    container [] [ box [ style "color" "orange" ] [ text ((List.take 1 validationErrors |> String.concat) ++ "and won't be saved ") ] ]
            ]
        , fieldBody []
            [ field []
                [ controlLabel [ for "username" ] [ text "Username" ]
                , controlInput inputModifier [] [ id "username", value model.username, onInput UpdateName ] [ text model.username ]
                , controlHelp Default [] []
                ]
            , field []
                [ controlLabel [ for "email" ] [ text "Email" ]
                , controlEmail inputModifier [] [ id "email", value model.email, onInput UpdateEmail ] [ text model.email ]
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
            500

        height =
            300
    in
    Canvas.toHtml ( width, height )
        [ style "border" "10px solid green" ]
        (currentCanvas model width height)


currentCanvas : Model -> Int -> Int -> List Canvas.Renderable
currentCanvas model width height =
    if model.gameStarted then
        gameCanvas model width height

    else
        welcomeCanvas model width height


welcomeCanvas : Model -> Int -> Int -> List Canvas.Renderable
welcomeCanvas model width height =
    [ shapes [ fill Color.black ] [ rect ( 0, 0 ) (toFloat width) (toFloat height) ]
    , renderName model width height
    ]


gameCanvas : Model -> Int -> Int -> List Canvas.Renderable
gameCanvas model width height =
    [ shapes [ fill Color.white ] [ rect ( 0, 0 ) (toFloat width) (toFloat height) ]
    ]


renderName : Model -> Int -> Int -> Canvas.Renderable
renderName model width height =
    let
        greeting =
            if model.gameEnded then
                "Play again, " ++ model.username ++ "?"

            else
                "Welcome " ++ model.username ++ "!"
    in
    Canvas.text [ Canvas.font { size = 48, family = "serif" }, fill Color.white, Canvas.align Canvas.Center ] ( toFloat width / 2, toFloat height / 2 ) greeting
