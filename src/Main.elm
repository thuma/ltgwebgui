module Main exposing (..)
import Browser
import Html exposing (Html, Attribute, button, div, text, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


main =
  Browser.element{
    init = init,
    view = view,
    update = update,
    subscriptions = subscriptions}

init : () ->( Model, Cmd Msg)
init _ = (
  { status = 0
  , email = ""
  }, 
  Cmd.none 
  )

type alias Model = 
  { status : Int
  , email : String
  }

type Msg 
  = Increment 
  | Decrement
  | EmailChange String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Increment ->
      ({ model | status  = model.status+1 }, Cmd.none)

    Decrement ->
      ({ model | status  = model.status-1 }, Cmd.none)

    EmailChange newEmail ->
      ({ model | email  = newEmail }, Cmd.none)

view : Model -> ( Html Msg)
view model = 
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (String.fromInt model.status) ]
    , div [] [ text (model.email) ]
    , input [ value model.email, onInput EmailChange] []
    , input [] []
    ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none