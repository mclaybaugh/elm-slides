import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url
import Http
import Json.Decode as D

-- MAIN
main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }

-- MODEL
type alias Model =
  { key : Nav.Key
  , url : Url.Url
  , message : String
  }

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  ( Model key url "init"
  , Http.get
      { url = "http://slides.test/slides-backend/src/index.php?show=abcd"
      , expect = Http.expectString GotSummary
      }
  )

-- UPDATE
type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | GotSummary (Result Http.Error String)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.key (Url.toString url) )

        Browser.External href ->
          ( model, Nav.load href )

    UrlChanged url ->
      ( { model | url = url }
      , Cmd.none
      )

    GotSummary result ->
      case result of
        Ok goodString ->
          ( { model | message = goodString }
          , Cmd.none
          )
        Err error ->
          ( { model | message = httpErrorString error }
          , Cmd.none
          )

httpErrorString : Http.Error -> String
httpErrorString error =
  case error of
    Http.BadUrl text ->
      "Bad Url: " ++ text
    Http.Timeout ->
      "Http Timeout"
    Http.NetworkError ->
      "Network Error"
    Http.BadStatus response ->
      "Bad Http Status: " ++ String.fromInt response
    Http.BadBody message ->
      "Bad Http Payload: " ++ message

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

-- VIEW
view : Model -> Browser.Document Msg
view model =
  { title = "Elm Slides"
  , body =
    [ div [class "container__summary"] [text model.message]
    ]
  }