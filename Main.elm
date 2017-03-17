import Pixiv.Types exposing (..)
import Pixiv
import Pixiv.Endpoints as Endpoints

import Infix exposing (..)
--import LocalStorage

import Html exposing (Html, main_, a, img, text, div, span)
import Html.Attributes exposing (class, id, src, title)
import Html.Events exposing (onClick)
import Http
import Markdown


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = always Sub.none
    }


type alias Model =
  { page : (Page, PageInfo)
  , history : List (Page, PageInfo)
  , error : Maybe String
  , tokens : Maybe Tokens
  }


type Msg =
    Query Request
  | Response (Result Http.Error (Page, PageInfo))
  | More
  | MoreResp (Result Http.Error Page)
  | Detail Illust
  | Back


init : (Model, Cmd Msg)
init =
  let
    tokens = Nothing
    {-
    tokens = case (flags.accessToken, flags.refreshToken) of
      (Just a, Just t) -> Just (Tokens a t)
      _ -> Nothing
    -}

    model =
      { page = (EmptyPage, BasePage "")
      , history = []
      , error = Nothing
      , tokens = tokens
      }

    startPage = case tokens of
      Nothing -> Endpoints.ranking
      Just toks -> Endpoints.myFeed toks.accessToken

    cmd = startPage
      |> Pixiv.send Response
  in
    model ! [ cmd ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Query req ->
      model ! [ req |> Pixiv.send Response ]

    More ->
      let
        auth = model.tokens |> Maybe.map .accessToken

        url = case Tuple.first model.page of
          IllustList _ url -> url
          CommentList _ url -> url
          UserPreviews _ url -> url
          _ -> Nothing

        cmd = case url of
          Just url -> Pixiv.more MoreResp auth url
          Nothing -> Cmd.none
      in
        model ! [ cmd ]

    Detail illust ->
      let
        page = (IllustDetail illust, IllustPage "Detail" illust)

        new = { model | page = page, history = model.page :: model.history }
      in
        new ! []

    Response (Ok page) ->
      let
        history = case model.page of
          (EmptyPage, _) -> model.history
          p -> p :: model.history

        new = { model | page = page, history = history }
      in
        new ! []

    Response (Err message) ->
      let new = { model | error = Just <| toString message } in
        new ! []

    MoreResp (Err message) ->
      let new = { model | error = Just <| toString message } in
        new ! []

    MoreResp (Ok page) ->
      let
        newPage =
          case (Tuple.first model.page, page) of
            (IllustList list _, IllustList list2 newUrl) ->
              IllustList (list ++ list2) newUrl
            (CommentList list _, CommentList list2 newUrl) ->
              CommentList (list ++ list2) newUrl
            (UserPreviews list _, UserPreviews list2 newUrl) ->
              UserPreviews (list ++ list2) newUrl
            _ ->
              (Tuple.first model.page)

        new = { model | page = (newPage, Tuple.second model.page) }
      in
        new ! []

    Back ->
      let
        tail = List.tail model.history ?: []

        (page, history) =
          case List.head model.history of
            Just p ->  (p, tail)
            Nothing -> (model.page, model.history)

        new = { model | page = page, history = history }
      in
        new ! []


-- Utilities
view : Model -> Html Msg
view model =
  let
    proxy : String -> Url
    proxy = (++) "http://localhost:9292/"


    empty = text ""


    thumb illust =
      let count = span [ class "count" ] [ text <| toString illust.count ] in
        a [ class "thumbnail link", onClick <| Detail illust ]
          [ img [ src <| proxy illust.thumbnail ] []
          , if illust.count > 1 then count else empty
          ]


    tag name =
      a [ class "tag link" ]
        [ text name ]


    pic url =
      div [ class "picture" ] [ img [ src <| proxy url ] [] ]

    --


    error = case model.error of
      Just msg ->
        div [ id "error", class "link" ] [ text msg ]
      Nothing ->
        empty


    page = case Tuple.first model.page of
      IllustList illusts url ->
        div [ id "list" ] <| illusts <!> thumb
      IllustDetail illust ->
        div [ id "detail" ] <| illust.urls <!> pic
      _ ->
        empty


    pageInfo = case model.page of
      (EmptyPage, _) ->
        empty

      (_, BasePage name) ->
        div [ id "page-info", class "base" ]
          [ span [ class "name" ] [ text name ] ]

      (_, UserPage name user) ->
        div [ id "page-info", class "artist" ]
          [ text name
          , img [ class "avatar", src <| proxy user.avatar ] []
          , a [ class "name link", title user.nick ]
            [ text user.name ]
          ]

      (_, IllustPage name illust) ->
        div [ id "page-info", class "illust" ]
          [ span [ class "name" ] [ text name ] ]


    back = case model.history of
      [] -> empty
      _ ->
        a [ id "back", class "link", onClick Back ]
          [ text "back" ]


    more =
      let
        next = case Tuple.first model.page of
          IllustList _ url -> url
          CommentList _ url -> url
          UserPreviews _ url -> url
          _ -> Nothing
      in
        case next of
          Just _ -> 
            div [ id "more" ]
              [ div [ class "cont", onClick More ]
                [ text "Load more" ]
              ]
          Nothing -> empty


    {-
    info = case model.illust of
      DetailMode illust ->
        div [ id "detail-info", class "illust" ]
          [ div [ class "name" ] [ text illust.title ]
          , div [ class "tags" ] <| illust.tags <!> tag
          , Markdown.toHtml [ class "caption" ] illust.caption
          ]
    -}
  in
    main_ []
      [ back
      , pageInfo
      , page
      , more
      --, info
      , error
      ]
