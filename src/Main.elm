module Main exposing (..)
import Browser
import Html exposing (Html, Attribute, img, button, div, text, input, table, tr, th, td, span, footer)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Base64
import Json.Decode exposing (Decoder, map, map4, map5, field, int, string, list)
import DateFormat exposing (format)
import Time exposing (Posix, Zone, utc)
import Iso8601


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

type alias ExtendedElev =
  { namn : String
  , uuid : String
  , short_id : String
  , tider : List Lektionstid
  , veckor: List Int
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
          ({ model | elever = data.alla, status = "Laddad" }, Cmd.none)
        Err error ->
          ({ model | status = (errorToString error) } , Cmd.none)


classBystatus : String -> Html.Attribute msg
classBystatus status =
  class (
    if status == "Närvarande" then
      "green"
    else if status == "Giltigt frånvarande" then
      "gul"
    else if status == "Annan aktivitet" then
      "blue"
    else if status == "Ogiltig frånvarande" then
      "red"
    else
      ""
  )

extendedElev : Elev -> ExtendedElev 
extendedElev elev = 
  { namn = elev.namn
  , uuid = elev.uuid
  , short_id  = elev.short_id
  , tider = elev.tider
  , veckor = List.foldr getWeeks [] elev.tider
  }

getWeekIntFromDate : String -> Int
getWeekIntFromDate datum = 
  case (Iso8601.toTime datum) of
    Ok time ->
      case String.toInt ( format [ DateFormat.weekOfYearNumber] utc time) of
        Just week ->
          week
        Nothing ->
          0
    Err _ ->
      0

getDayIntFromDate : String -> Int
getDayIntFromDate datum = 
  case (Iso8601.toTime datum) of
    Ok time ->
      case String.toInt ( format [ DateFormat.dayOfWeekNumber] utc time) of
        Just day -> 
          day
        Nothing ->
          0
    Err _ ->
      0

getWeeks : Lektionstid -> List Int -> List Int
getWeeks lektion veckor = 
  if List.member (getWeekIntFromDate lektion.start) veckor then
    veckor
  else
    List.append [getWeekIntFromDate lektion.start] veckor

olika : List String
olika = 
  [ "Närvarande"
  , "Giltigt frånvarande"
  , "Annan aktivitet"
  , "Ogiltig frånvarande"
  ]

exempel : String -> Html Msg
exempel status =
  span [ classBystatus status ] [ text status ]

wasLateMin : Lektionstid -> Html Msg
wasLateMin lektion =
  if lektion.avvikelse > 0 then
    span [ class "red" ]  [text (" Min: " ++ (String.fromInt lektion.avvikelse))]
  else
    span [] []

filerForDyW : Int -> Int -> Lektionstid -> Bool
filerForDyW dag vecka tid =
  (getDayIntFromDate tid.start) == dag && (getWeekIntFromDate tid.start) == vecka

enLektion : Lektionstid -> Html Msg
enLektion lektion = div [classBystatus lektion.status]
  [ text ((String.slice -8 -3 lektion.start) ++ " " ++ lektion.lektion)
  , (wasLateMin lektion) 
  ]

enDag : List (Lektionstid) -> Int -> Int -> Html Msg
enDag lektioner vecka dag = td [] (List.map enLektion (List.filter (filerForDyW dag vecka) lektioner))

enVecka : List (Lektionstid) -> Int -> Html Msg
enVecka lektioner vecka = tr [] (List.map (enDag lektioner vecka) [1, 2, 3, 4, 5] )

veckorD : ExtendedElev -> List (Html Msg)
veckorD elev = (List.map (enVecka elev.tider) elev.veckor) 

rowElev : Elev -> Html Msg
rowElev elev =
    div [ class "elev"]
        [ text elev.namn
--      , table [] ( List.map rowTid elev.tider )
        , table [ class "table table-sm table-bordered"] (List.append [ tr []
          [ th [] [text "Måndag"]
          , th [] [text "Tisdag"]
          , th [] [text "Onsdag"]
          , th [] [text "Torsdag"]
          , th [] [text "Fredag"]
          ]
          ] (veckorD (extendedElev elev)))
        , div [ class "sida" ] ( List.map exempel olika )
        ]

wasLate : Lektionstid -> Html Msg
wasLate lektion =
  if lektion.avvikelse > 0 then
    td [ class "red" ]  [text (String.fromInt lektion.avvikelse)]
  else
    td [] []

rowTid : Lektionstid -> Html Msg
rowTid lektion =
    tr [ classBystatus lektion.status
        ] 
        [ td [] [text (String.slice 0 -3 (String.map (\c -> if c == 'T' then ' ' else c) lektion.start))]
        , td [] [text (lektion.lektion ++ " ")]
        , wasLate lektion
        ]

view : Model -> ( Html Msg ) 
view model = 
  div []
    [ if (List.length model.elever) == 0 && model.status == "" then
        div [ class "container" ]
          [ text model.status
          , text "Användarnamn:"
          , input [ value model.email, onInput EmailChange, class "form-control"] [ ]
          , text "Lösenord:"
          , input [ type_ "password", value model.password, onInput PasswordChange, class "form-control"] [ ]
          , button [ class "btn btn-primary", onClick Login ] [ text "Login" ]
          ]
      else if model.status == "Login" then
        div [ class "text-center" ] 
          [ img
            [ src "img/Loading_icon.gif"
            , class "mx-auto d-block"] []
          ]
      else
        div [] ( List.map rowElev model.elever )
    ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none