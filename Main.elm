import Pixiv exposing (Illust, User, Tag, Url, Token, withOptions, withProxy)
import Infix exposing (..)
import LocalStorage

import Html exposing (Html, main_, a, img, text, div, span, programWithFlags)
import Html.Attributes exposing (class, id, src, title)
import Html.Events exposing (onClick)
import Http
import Markdown


main =
  programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = always Sub.none
    }


type alias Model =
  { illust : Mode
  , more : Maybe Url
  , history : List (Mode, Maybe Url)
  , error : Maybe String
  , tokens : Maybe Token
  }


type Mode
  = ListMode (List Illust)
  | DetailMode Illust


type Msg
  = Query Pixiv.Request
  | Response (Result Http.Error (List Illust, Maybe Url))
  | More
  | MoreResp (Result Http.Error (List Illust, Maybe Url))
  | Detail Illust
  | Back
  | Dismiss
  | Login (Result Http.Error Token)


type alias Flags =
  { accessToken : Maybe String
  , refreshToken : Maybe String
  }


-- Main functions
init : Flags -> (Model, Cmd Msg)
init flags =
  let
    tokens = case (flags.accessToken, flags.refreshToken) of
      (Just a, Just t) -> Just (Token a t)
      (_, _) -> Nothing

    model =
      { illust = ListMode []
      , more = Nothing
      , history = []
      , error = Nothing
      , tokens = tokens
      }

    login =
        Cmd.none

    cmd =
      Pixiv.userIllusts 102267
        |> withProxy "http://localhost:9292/"
        |> Pixiv.send Response
  in
    model ! [ login, cmd ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Query request ->
      model ! [ query request ]

    Response (Ok (list, url)) ->
      let hist = case model.illust of
        ListMode [] -> model.history
        _ -> (model.illust, model.more) :: model.history
      in
        { model | illust = ListMode list, more = url, history = hist } ! []

    Response (Err mess) ->
      { model | error = Just <| toString mess } ! []

    More ->
      let cmd =
        case model.more of
          Just url -> [ Pixiv.more MoreResp (proxy url) ]
          Nothing -> []
      in
        model ! cmd

    MoreResp (Ok (list, url)) ->
      let illust = case model.illust of
        ListMode ls -> ListMode (ls ++ list)
        x -> x
      in
        { model | illust = illust, more = url } ! []

    MoreResp (Err mess) ->
      { model | error = Just <| toString mess } ! []

    Detail illust ->
      { model
        | illust = DetailMode illust
        , history = (model.illust, model.more) :: model.history
        , more = Nothing
      } ! []

    Back ->
      case List.head model.history of
        Just (ListMode [], _) -> model ! []
        Nothing -> model ! []
        Just (last, u) ->
          { model
            | illust = last
            , more = u
            , history = List.tail model.history ?: []
          } ! []

    Dismiss ->
      { model | error = Nothing } ! []

    Login (Ok tokens) ->
      let
        cmds =
          [ LocalStorage.set ("accessToken", tokens.access)
          , LocalStorage.set ("refreshToken", tokens.refresh)
          ]
      in
        { model | tokens = Just tokens } ! cmds

    Login (Err mess) ->
      { model | error = Just <| toString mess } ! []



-- Utilities
query : Pixiv.Request -> Cmd Msg
query = withProxy "http://localhost:9292/" >> Pixiv.send Response


proxy : String -> Url
proxy = (++) "http://localhost:9292/"


type alias View = Html Msg


linkTo : Pixiv.Request -> Html.Attribute Msg
linkTo = onClick << Query


empty : View
empty = text ""


-- View
view : Model -> View
view model =
  let
    thumb : Illust -> View
    thumb illust =
      let count = span [ class "count" ] [ text <| toString illust.count ] in
        a [ class "thumbnail link", onClick <| Detail illust ]
          [ img [ src <| proxy illust.thumb ] []
          , if illust.count > 1 then count else empty
          ]

    tag : Tag -> View
    tag name =
      a [ class "tag link", linkTo <| Pixiv.search name ]
        [ text name ]

    pic : Url -> View
    pic url =
      div [ class "picture" ] [ img [ src <| proxy url ] [] ]

    error = case model.error of
      Just msg ->
        div [ id "error", class "link", onClick Dismiss ] [ text msg ]
      Nothing ->
        empty

    page = case model.illust of
      ListMode list ->
        div [ id "list" ] <| list <!> thumb
      DetailMode illust ->
        div [ id "detail" ] <| illust.urls <!> pic

    back = case model.history of
      [] -> empty
      _ -> a [ id "back", class "link", onClick Back ] [ text "back" ]

    more = case model.more of
      Just url ->
        div [ id "more" ]
          [ div [ class "cont", onClick More ]
            [ text "Load more" ]
          ]
      Nothing -> empty

    pageInfo = case model.illust of
      DetailMode illust ->
        div [ id "page-info", class "artist" ]
          [ img [ class "avatar", src <| proxy illust.user.avatar ] []
          , a [ class "name link", title illust.user.handle, linkTo <| Pixiv.userIllusts illust.user.id ]
            [ text illust.user.name ]
          ]
      _ ->
        empty

    info = case model.illust of
      ListMode _ ->
        empty
      DetailMode illust ->
        div [ id "detail-info", class "illust" ]
          [ div [ class "name" ] [ text illust.title ]
          , div [ class "tags" ] <| illust.tags <!> tag
          , Markdown.toHtml [ class "caption" ] illust.caption
          ]
  in
    main_ []
      [ back
      , pageInfo
      , page
      , more
      , info
      , error
      ]
