module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Browser.Navigation as Navigation
import Browser exposing (UrlRequest)

import Url exposing (Url)
import Url.Parser as UrlParser exposing ((<?>), (</>), Parser, s, top, int)
import Url.Builder as Builder
import Url.Parser.Query as Query

import Bootstrap.Navbar as Navbar
import Bootstrap.Table as Table
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Button as Button
import Bootstrap.ListGroup as Listgroup
import Bootstrap.Modal as Modal
import Bootstrap.CDN as CDN
import Bootstrap.Text as Text
import Bootstrap.Utilities.Spacing as Spacing
import Bootstrap.Utilities.Border as Border
import Bootstrap.Utilities.Flex as Flex


-------------------------------------------------------------------------------
--                               Types
-------------------------------------------------------------------------------


--
-- Elements searchCategory and searchNumber are ranged over integers
--
type alias Model =
    { navKey : Navigation.Key
    , page : Page
    , pageUnit : Int
    , movie : Maybe MovieRecord
    , navState : Navbar.State
    , root : Url
    , modalVisibility : Modal.Visibility
    }


type alias Category = Int


type Page
    = Search (Maybe Category) (Maybe Int)
    | Movie (Maybe Int)
    | Policy
    | Inquiry
    | FAQ
    | NotFound


type Msg
    = UrlChange Url
    | ClickedLink UrlRequest
    | ClickedMovie MovieRecord
    | NavMsg Navbar.State
    | CloseModal
    | ShowModal


type alias MovieRecord =
    { movieTitle : String
    , movieId : Int
    , movieURL : String
    , categories : List Category
    , imgLink : String
    }


type alias Flags = {}


-------------------------------------------------------------------------------
--                               Main Functions
-------------------------------------------------------------------------------


main : Program Flags Model Msg
main =
  Browser.application {
    init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  , onUrlRequest = ClickedLink
  , onUrlChange = UrlChange
  }


init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( navState, navCmd ) = Navbar.initialState NavMsg

        ( model, urlCmd ) =
            urlUpdate url { navKey = key
                          , navState = navState
                          , page = Search Nothing Nothing
                          , pageUnit = 15
                          , movie = Nothing
                          , root = url
                          , modalVisibility= Modal.hidden
                          }
    in
        ( model, Cmd.batch [ urlCmd, navCmd ] )


--- XXX: We should remove loadCSS when building for production
view : Model -> Browser.Document Msg
view model =
    { title = "SamiDare"
    , body =
        [ div [ class "content" ]
            <| loadCSS ++
            [ menu model
            , mainContent model
            , modal model
            ]
        , footContent model
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink req ->
            -- Debug.log "ClickedLink" <|
            case req of
                Browser.Internal url ->
                    ( model
                    , Navigation.pushUrl model.navKey <| Url.toString url
                    )

                Browser.External href ->
                    ( model
                    , Navigation.load href )

        UrlChange url ->
            -- Debug.log "UrlChange" <|
            urlUpdate url model

        ClickedMovie rec ->
            -- Debug.log "ClickedMovie" <|
            let
                ref = mkMoviePageRef model rec.movieId
            in
                ( { model | movie = Just rec }
                , Navigation.pushUrl model.navKey ref
                )

        NavMsg state ->
            ( { model | navState = state }
            , Cmd.none
            )

        CloseModal ->
            ( { model | modalVisibility = Modal.hidden }
            , Cmd.none
            )

        ShowModal ->
            ( { model | modalVisibility = Modal.shown }
            , Cmd.none
            )


urlUpdate : Url -> Model -> ( Model, Cmd Msg )
urlUpdate url model =
    let
        place =
            { url | path = Maybe.withDefault "" url.fragment
                  , fragment = Nothing }
    in
        case UrlParser.parse routeParser place of
            Nothing ->
                ( { model | page = NotFound }, Cmd.none )

            Just route ->
                ( { model | page = route }, Cmd.none )


routeParser : Parser (Page -> a) a
routeParser =
    -- XXX:
    UrlParser.oneOf
        [ UrlParser.map Inquiry (top </> s "inquiry")
        , UrlParser.map FAQ (top </> s "faq")
        , UrlParser.map Policy (top </> s "policy")
        , UrlParser.map Search (top <?> Query.int categoryQ <?> Query.int pageNumQ)
        , UrlParser.map Movie (s "movie" <?> Query.int "id")
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Navbar.subscriptions model.navState NavMsg


-------------------------------------------------------------------------------
--                            Internal Functions
-------------------------------------------------------------------------------


menu : Model -> Html Msg
menu model =
    Navbar.config NavMsg
        |> Navbar.withAnimation
        |> Navbar.info
        |> Navbar.brand [ href <| model.root.path ] [ text "SamiDare" ]
        |> Navbar.items
            [ Navbar.itemLink [ href "http://elm-bootstrap.info/alert" ] [ text "Elm-Bootstrap ex" ]
            , Navbar.itemLink [ href "https://package.elm-lang.org/packages/rundis/elm-bootstrap/5.1.0" ]
                              [ text "Elm-Bootstrap doc" ]
            , Navbar.itemLink [ href "https://guide.elm-lang.jp" ] [ text "Elm intro" ]
            , Navbar.itemLink [ href "https://getbootstrap.com/docs/4.0/components/alerts" ] [ text "Bootstrap doc" ]
            ]
        |> Navbar.view model.navState


mainContent : Model -> Html Msg
mainContent model =
    Grid.container [] <|
        case model.page of
            Search _ _ ->
                pageSearch model

            Movie (Just movieId) ->
                pageMovie movieId model

            FAQ ->
                List.singleton <| text "F and Q"

            Inquiry ->
                List.singleton <| text "お問い合わせ"

            Policy ->
                List.singleton <| text "Site policy"

            _ ->
                pageNotFound


footContent : Model -> Html Msg
footContent model =
    let
        mkhref to = href <| model.root.path ++ to
    in
    footer
        [ class "footer font-small" ]
        [ Grid.row
            []
            [ Grid.col
                [ Col.textAlign Text.alignXsRight ]
                [ a
                    [ mkhref "#faq"
                    , style "font-size" "x-small"
                    , Spacing.ml2
                    ]
                    [ text "このページについて" ]
                ]
            , Grid.col
                [ Col.textAlign Text.alignXsCenter ]
                [ a
                    [ mkhref "#inquiry"
                    , style "font-size" "x-small" ]
                    [ text "お問い合わせ" ]
                ]
            , Grid.col
                [ Col.textAlign Text.alignXsLeft ]
                [ a
                    [ mkhref "#policy"
                    , style "font-size" "x-small" ]
                    [ text "サイトポリシー" ]
                ]
            ]
        , div
            [ class "text-center py-3" ]
            [ p [] [ text "(c) 2019- SamiDare" ] ]
        ]


--
-- Page views
--
pageNotFound : List (Html Msg)
pageNotFound =
    [ h1 [] [ text "Not found" ]
    , text "Sorry couldn't find that page"
    ]


pageSearch : Model -> List (Html Msg)
pageSearch model =
    case toSearchState model.page of
        (c, n) ->
            let
                (xs, numAllRecords) = getRecords c n model.pageUnit
            in
                [ h3 [] [ text "Top page of SamiDare" ] ]
                ++ genSearchItems model xs
                ++ [ mkPagination c n model.pageUnit numAllRecords ]


pageMovie : Int -> Model -> List (Html Msg)
pageMovie movieId model =
    case model.movie of
        Nothing ->
            displayMovie movieId

        Just rec ->
            if
                rec.movieId == movieId

            then
                [ mkKeyValueText "title" rec.movieTitle
                , mkKeyValueText "id" <| String.fromInt rec.movieId
                , mkKeyValueText "img" rec.imgLink
                , mkKeyValueText "url" rec.movieURL
                , mkKeyValueText "categories"
                    <| String.join ", "
                    <| List.map String.fromInt rec.categories
                , Button.button
                    [ Button.success
                    , Button.small
                    , Button.block
                    , Button.attrs [ onClick ShowModal ]
                    ]
                    [ text "Click me" ]
                ]

            else
                displayMovie movieId


genSearchItems : Model -> List MovieRecord -> List (Html Msg)
genSearchItems model records =
    List.map (genSearchItem model) records


genSearchItem : Model -> MovieRecord -> Html Msg
genSearchItem model rec =
    let
        imgArea =
            img
                [ src rec.imgLink
                , style "width" "100%"
                , onClick <| ClickedMovie rec
                ]
                []

        titleArea =
            div [ onClick <| ClickedMovie rec ] [ text rec.movieTitle ]
    in
        Grid.row
            [ Row.attrs [ Border.all] ]
            [ Grid.col [ Col.xs6, Col.sm5, Col.md3 ] [ imgArea ]
            , Grid.col [] [ titleArea, mkCategoryLinks rec ]
            ]


mkCategoryLinks : MovieRecord -> Html Msg
mkCategoryLinks rec =
    let
        mkLinks c = a [ mkPageLink c 1, Spacing.ml1 ] [ text <| fromCategory c ]
    in
        div [ Spacing.mt3 ] <| List.map mkLinks rec.categories


fromCategory : Category -> String
fromCategory x =
    case x of
        1 -> "one"
        2 -> "two"
        3 -> "three"
        _ -> "other"


toSearchState : Page -> (Category, Int)
toSearchState page =
    case page of
        Search (Just c2) (Just n2) -> (c2, n2)
        Search (Just c2) (Nothing) -> (c2, 1)
        Search (Nothing) (Just n2) -> (1, n2)
        _ -> (1, 1)


mkMoviePageRef : Model -> Int -> String
mkMoviePageRef model movieId =
    model.root.path ++ "?id=" ++ String.fromInt movieId ++ "#movie"


categoryQ = "c"
pageNumQ = "n"


mkKeyValueText : String -> String -> Html Msg
mkKeyValueText k v =
    p [] [ text <| k ++ " = " ++ v ]


displayMovie : Int -> List (Html Msg)
displayMovie movieId =
    case getRecord movieId of
        Nothing ->
            let
                errMsg =
                    "We should get model.movie from database where id = "
                    ++ String.fromInt movieId
                    ++ " ..."
            in
            [ h4 [] [ text errMsg ] ]

        Just rec ->
            [ h2 [] [ text "ok" ] ]


--
-- Pagination
--
mkPagination : Category -> Int -> Int -> Int -> Html Msg
mkPagination category curPage unit numAll =
    let
        seed = mkPageNumSeed curPage <| numOfPages unit numAll
    in
        ul
            [ class "pagination"
            , Flex.justifyCenter
            , Spacing.mt2
            , Spacing.mlAuto ]
        <| List.map (mkPagerItem category curPage) seed


mkPageNumSeed : Int -> Int -> List (Int, String)
mkPageNumSeed curPage maxPage =
    let
        back = [ (1, "<<") , (Basics.max 1 <| curPage - 1, "<") ]

        forward = [ (Basics.min maxPage <| curPage + 1, ">") , (maxPage, ">>") ]

        numBack = Basics.max 1 (curPage - 2)

        numForward = Basics.min maxPage (curPage + 2)

        listBackForward =
            List.map (\y -> (y, String.fromInt y))
            <| List.range numBack numForward
    in
        back ++ listBackForward ++ forward


numOfPages : Int -> Int -> Int
numOfPages unit numAll =
    toFloat numAll / toFloat unit |> ceiling


mkPagerItem : Category -> Int -> (Int, String) -> Html Msg
mkPagerItem category curPage (pageNum, str) =
    let
        classOfLi =
            if Just curPage == String.toInt str
            then [ class "page-item active" ]
            else [ class "page-item" ]
    in
        li classOfLi
            [ a [ class "page-link", mkPageLink category pageNum ] [ text str ] ]


mkPageLink : Category -> Int -> Attribute Msg
mkPageLink category pageNum =
    Builder.relative
        []
        [ Builder.int categoryQ category , Builder.int pageNumQ pageNum ]
    |> href


--
-- Call External APIs
--
getRecord : Int -> Maybe MovieRecord
getRecord movieId = Nothing


getRecords : Category -> Int -> Int -> (List MovieRecord, Int)
getRecords category pageNum unit =
    List.range 1 unit
    |> List.map
        (\i -> { movieTitle = "めっちゃあれなんだがwww"
               , movieId = i
               , movieURL = "aaa"
               , categories = [1, 2, 3]
               , imgLink = "https://img.animatetimes.com/news/visual/2018/1541568337/1542100549730.jpg"
               }
        )
    |> (\x -> (x, 150))


--
-- For advertisement
--
modal : Model -> Html Msg
modal model =
    Modal.config CloseModal
        |> Modal.small
        |> Modal.h4 [] [ text "Ad" ]
        |> Modal.body []
            [ h2 [] [ text "ad ad" ] ]
        |> Modal.view model.modalVisibility


--
-- Load CSS for booting with elm-reactor
--
loadCSS =
    [ CDN.stylesheet
    , node "link" [ rel "stylesheet", href "assets/style.css" ] []
    ] 
