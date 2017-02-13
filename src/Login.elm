module Login exposing (Model, init, Msg, update, view, OutMsg(..))

import Html exposing (..)
import Html.Attributes exposing (class, style)
import Types exposing (Display(..))
import Material
import Material.Options as Options
import Material.Elevation as Elevation
import Material.Textfield as Textfield
import Material.Card as Card
import Material.Button as Button


-- MODEL


type alias Model =
    { mdl : Material.Model
    , email : String
    , password : String
    , display : Display
    }


init : Model
init =
    { mdl = Material.model
    , email = ""
    , display = Active
    , password = ""
    }



-- UPDATE


type Msg
    = Mdl (Material.Msg Msg)
    | Batch (List Msg)
    | UpdEmail String
    | UpdPassword String
    | Done


type OutMsg
    = Next
    | NoSignal


update : Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update msg model =
    case Debug.log "child login msg: " msg of
        Mdl msg_ ->
            let
                ( model_, cmd_ ) =
                    Material.update Mdl msg_ model
            in
                ( model_, cmd_, NoSignal )

        UpdEmail str ->
            ( { model | email = str }, Cmd.none, NoSignal )

        UpdPassword str ->
            ( { model | password = str }, Cmd.none, NoSignal )

        Done ->
            ( { model | display = Hidden }, Cmd.none, Next )

        Batch listOfMsg ->
            let
                ( finalModel, listOfFx ) =
                    List.foldl
                        (\msg ->
                            \( mdl, fxList ) ->
                                let
                                    ( newModel, newFx, outMsg ) =
                                        update msg mdl
                                in
                                    ( newModel, fxList ++ [ newFx ] )
                        )
                        ( model, [] )
                        listOfMsg
            in
                ( finalModel, Cmd.batch listOfFx, NoSignal )



-- View


type alias Mdl =
    Material.Model


view : Model -> Html Msg
view model =
    let
        style_ =
            if (model.display == Active) then
                ( "display", "block" )
            else
                ( "display", "none" )
    in
        div [ style [ style_ ] ]
            [ h2 [ style [ ( "text-align", "center" ) ] ]
                [ text "Welcome" ]
            , p [ style [ ( "text-align", "center" ) ] ]
                [ text "Amc's The walking Dead" ]
            , viewCard model
            ]


viewCard : Model -> Html Msg
viewCard model =
    Card.view
        [ Elevation.e2 ]
        [ Card.text
            [ Card.expand ]
            [ h5 [ style [ ( "text-align", "center" ) ] ] [ text "WRAPID" ]
            , Textfield.render Mdl
                [ 0, 0, 0 ]
                model.mdl
                [ Textfield.email
                , Textfield.label "Email"
                , Textfield.value model.email
                , Options.css "display" "block"
                , Options.onInput UpdEmail
                , Options.dispatch Batch
                ]
                []
            , Textfield.render Mdl
                [ 0, 0, 1 ]
                model.mdl
                [ Textfield.password
                , Textfield.label "Password"
                , Textfield.value model.password
                , Options.css "display" "block"
                , Options.onInput UpdPassword
                , Options.dispatch Batch
                ]
                []
            ]
        , Card.actions
            [ Options.css "text-align" "right"
            , Options.css "padding" "0 2rem 2rem"
            ]
            [ Button.render Mdl
                [ 10, 0 ]
                model.mdl
                [ Button.ripple
                , Button.accent
                , Options.onClick Done
                ]
                [ text "SIGN UP" ]
            , Button.render Mdl
                [ 10, 1 ]
                model.mdl
                [ Button.raised
                , Button.ripple
                , Button.colored
                , Options.onClick Done
                ]
                [ text "SIGN IN" ]
            ]
        ]
