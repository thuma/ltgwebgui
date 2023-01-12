module Main exposing (..)
import Browser
import Html exposing (Html, Attribute, button, div, text, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Base64
import Json.Decode exposing (Decoder, map, map4, map5, field, int, string, list)

getNarvaroData : Model -> Cmd Msg
getNarvaroData model =
  Http.request
        { method = "GET"
        , headers = [Http.header "Authorization" ("Basic " ++ (Base64.encode (model.email ++ ":" ++ model.password) ))]
        , url = "https://api.ltgee.se/vklass/v1/narvaro"
        , body = Http.emptyBody
        , expect = Http.expectJson GotText eleverDecoder
        , timeout = Nothing
        , tracker = Nothing
        } 

main =
  Browser.element{
    init = init,
    view = view,
    update = update,
    subscriptions = subscriptions}

init : () ->( Model, Cmd Msg)
init _ = (
  { status = ""
  , email = ""
  , password = ""
  , elever = []
  }, 
  Cmd.none 
  )

type alias Model = 
  { status : String
  , email : String
  , password: String
  , elever: List Elev
  }

type alias Elever =
  { alla : List Elev }

type alias Elev =
  { namn : String
  , uuid : String
  , short_id : String
  , tider : List Lektionstid
  }

type alias Lektionstid =
  { start : String
  , end : String
  , lektion : String
  , status :  String
  , avvikelse : Int
  }

tidDecoder : Decoder Lektionstid
tidDecoder =
  map5 Lektionstid
  (field "start" string)
  (field "end" string)
  (field "lektion" string)
  (field "status" string)
  (field "avvikelse" int)

eleverDecoder : Decoder Elever
eleverDecoder =
  Json.Decode.map Elever  
    (field "klass" (Json.Decode.list elevDecoder))

elevDecoder : Decoder Elev
elevDecoder =
  map4 Elev
    (field "name" string)
    (field "uuid" string)
    (field "short_id" string)
    (field "tider" (Json.Decode.list tidDecoder))

type Msg 
  = Login 
  | PasswordChange String
  | EmailChange String
  | GotText (Result Http.Error Elever)

errorToString : Http.Error -> String
errorToString error =
    case error of
        Http.BadUrl url ->
            "The URL " ++ url ++ " was invalid"
        Http.Timeout ->
            "Unable to reach the server, try again"
        Http.NetworkError ->
            "Unable to reach the server, check your network connection"
        Http.BadStatus 500 ->
            "The server had a problem, try again later"
        Http.BadStatus 400 ->
            "Verify your information and try again"
        Http.BadStatus _ ->
            "Unknown error"
        Http.BadBody errorMessage ->
            errorMessage

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Login ->
      ({ model | status  = "Login" }, getNarvaroData model)

    EmailChange newEmail ->
      ({ model | email  = newEmail }, Cmd.none)
      
    PasswordChange newPassword ->
      ({ model | password  = newPassword }, Cmd.none)
    
    GotText result ->
      case result of
        Ok data ->
          ({ model | elever = data.alla }, Cmd.none)
        Err error ->
          ({ model | status = (errorToString error) } , Cmd.none)

rowElev : Elev -> Html Msg
rowElev elev =
    div []
        [ text elev.namn ]

view : Model -> ( Html Msg)
view model = 
  div []
    [ div [] [ text model.status ]
    , div [] ( List.map rowElev model.elever )
    , input [ value model.email, onInput EmailChange] [ ]
    , input [ type_ "password", value model.password, onInput PasswordChange] [ ]
    , button [ onClick Login ] [ text "Login" ]
    ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none