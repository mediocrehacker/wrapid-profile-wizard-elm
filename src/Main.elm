module Main exposing (..)

import Html exposing (..)
import Login as Login
import Login exposing (OutMsg(..))
import Wizard as Wizard
import Material
import Material.Scheme as Scheme
import Material.Options as Options


-- MODEL


type alias Model =
    { loginModel : Login.Model
    , wizardModel : Wizard.Model
    }


model : Model
model =
    { loginModel = Login.init
    , wizardModel = Wizard.initEmpty
    }



-- ACTION, UPDATE


type Msg
    = LoginMsg Login.Msg
    | WizardMsg Wizard.Msg



-- qualify as Child.OutMsg when
-- multiple child components expose OutMsg


processSignal : Login.OutMsg -> Model -> ( Model, Cmd Msg )
processSignal signal model =
    case Debug.log "Child Message: " signal of
        NoSignal ->
            ( model, Cmd.none )

        Next ->
            ( { model | wizardModel = Wizard.init }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "model: " msg
    in
        case Debug.log "parent msg: " msg of
            LoginMsg subMsg ->
                let
                    ( updatedLoginModel, loginCmd, signalForParent ) =
                        Login.update subMsg model.loginModel

                    -- model should be up to date
                    ( newModel, cmdsFromSignal ) =
                        processSignal
                            signalForParent
                            { model | loginModel = updatedLoginModel }
                in
                    ( newModel
                    , Cmd.batch
                        [ Cmd.map LoginMsg loginCmd
                        , cmdsFromSignal
                        ]
                    )

            WizardMsg subMsg ->
                let
                    ( updatedWizardModel, wizardCmd ) =
                        Wizard.update subMsg model.wizardModel
                in
                    ( { model | wizardModel = updatedWizardModel }
                    , Cmd.map WizardMsg wizardCmd
                    )



-- View


view : Model -> Html Msg
view model =
    Scheme.top <|
        Options.div
            [ Options.center ]
            [ Html.map LoginMsg (Login.view model.loginModel)
            , Html.map WizardMsg (Wizard.view model.wizardModel)
            ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( model, Cmd.none )
        , view = view
        , subscriptions = always Sub.none
        , update = update
        }
