module Main exposing (..)

import Browser
import Css exposing (..)
import Dict exposing (Dict)
import Html.Styled exposing (..)
import Html.Styled.Attributes as Ha exposing (css, placeholder, type_, value)
import Html.Styled.Events exposing (..)
import Http
import Json.Decode as Dec exposing (Decoder)
import List.Extra as Listx
import Url


main : Platform.Program () Model Msg
main =
    Browser.document
        { init = always ( newModel, Cmd.none )
        , view = \model -> { title = "Github Link Markdown Formatter", body = [ view model ] }
        , update = update
        , subscriptions = always Sub.none
        }



-- MODEL


type alias Model =
    { linksText : String
    , links : List String
    , vars : Dict Int MdLink
    , errs : List ( Http.Error, String )
    , fmtstr : String
    , ghToken : String
    }


newModel : Model
newModel =
    { linksText = ""
    , links = []
    , vars = Dict.empty
    , errs = []
    , fmtstr = "[{title} #{number}]({url})"
    , ghToken = ""
    }


type alias GhResp =
    { title : String
    , number : Int
    }


type alias MdLink =
    { url : String
    , title : String
    , repo : String
    , owner : String
    , number : Int
    }


type Msg
    = SetLinks String
    | Submit
    | Recv Int (Result Http.Error GhResp)
    | SetFmtstr String
    | SetToken String



-- UPDATE


update msg model =
    case msg of
        SetLinks links ->
            ( { model | linksText = links }, Cmd.none )

        Submit ->
            String.trim model.linksText
                |> String.lines
                |> List.filter (String.isEmpty >> not)
                |> (\ls ->
                        ( { model | links = ls, vars = Dict.empty, errs = [] }
                        , makeGhRequests model.ghToken ls
                        )
                   )

        Recv idx (Err err) ->
            Listx.getAt idx model.links
                |> Maybe.map (Tuple.pair err)
                |> Maybe.map (\x -> x :: model.errs)
                |> Maybe.withDefault model.errs
                |> (\es -> ( { model | errs = es }, Cmd.none ))

        Recv idx (Ok resp) ->
            case Listx.getAt idx model.links of
                Nothing ->
                    ( model, Cmd.none )

                Just link ->
                    Dict.insert idx (parseUrl resp link) model.vars
                        |> (\x -> ( { model | vars = x }, Cmd.none ))

        SetFmtstr f ->
            ( { model | fmtstr = f }, Cmd.none )

        SetToken f ->
            ( { model | ghToken = f }, Cmd.none )


makeApiUrl : String -> String
makeApiUrl link =
    Url.fromString link
        |> Maybe.map
            (\url ->
                { url
                    | host = "api.github.com"
                    , path = "/repos" ++ (String.replace "/pull/" "/issues/" url.path)
                }
            )
        |> Maybe.map Url.toString
        |> Maybe.withDefault link


makeGhRequests : String -> List String -> Cmd Msg
makeGhRequests ghToken =
    let
        mkReq idx link =
            Http.request
                { method = "GET"
                , headers =
                    if String.isEmpty ghToken then
                        []

                    else
                        [ Http.header "Authorization" ("Bearer " ++ ghToken) ]
                , url = makeApiUrl link
                , body = Http.emptyBody
                , expect = Http.expectJson (Recv idx) ghDecoder
                , timeout = Nothing
                , tracker = Nothing
                }
    in
    List.indexedMap mkReq >> Cmd.batch


ghDecoder : Decoder GhResp
ghDecoder =
    Dec.map2 GhResp
        (Dec.field "title" Dec.string)
        (Dec.field "number" Dec.int)


parseUrl : GhResp -> String -> MdLink
parseUrl ghResp url =
    let
        owner =
            String.split "/" url |> Listx.getAt 3 |> Maybe.withDefault ""

        repo =
            String.split "/" url |> Listx.getAt 4 |> Maybe.withDefault ""
    in
    { url = url
    , title = ghResp.title
    , number = ghResp.number
    , owner = owner
    , repo = repo
    }



-- VIEW


view model =
    div
        [ css
            [ backgroundColor theme.bg
            , height (vh 100)
            , padding2 zero (px 20)
            , paddingTop (px 40)
            , color theme.font
            , boxSizing contentBox
            ]
        , Ha.style "height" "calc(100vh - 40px)"
        ]
        [ view_ model ]
        |> toUnstyled


view_ : Model -> Html Msg
view_ model =
    div
        [ css
            [ maxWidth (px 750), displayFlex, flexDirection column, margin auto ]
        ]
        [ div [ css [ textAlign center ] ] [ h1 [] [ text "Github Links Markdown Formatter" ] ]
        , div []
            [ textarea
                [ onInput SetLinks
                , on "change" (Dec.succeed Submit)
                , value model.linksText
                , placeholder "Paste URLs to Github issues or pull-requests"
                , css
                    ([ width (pct 100), minHeight (px 150), backgroundColor theme.textIn ]
                        ++ textBorder
                    )
                ]
                []
            ]
        , div [ css marginInput ]
            [ input
                [ placeholder "(Optional) Github Personal Access Token"
                , type_ "password"
                , onInput SetToken
                , css
                    ([ width (pct 100), textAlign center, backgroundColor theme.textIn ]
                        ++ textBorder
                    )
                ]
                []
            ]
        , div [ css marginInput ]
            [ details []
                [ summary [ css [ cursor pointer ] ] [ text "Output Format" ]
                , text "Control the format of each link"
                , br [] []
                , text "Variables are:"
                , ul []
                    [ li [] [ strong [] [ text "{title}" ], text ": The issue or PR title" ]
                    , li [] [ strong [] [ text "{number}" ], text ": The issue number" ]
                    , li [] [ strong [] [ text "{repo}" ], text ": The repository name" ]
                    , li [] [ strong [] [ text "{owner}" ], text ": The owner of the repository" ]
                    , li [] [ strong [] [ text "{url}" ], text ": The link url" ]
                    ]
                ]
            ]
        , div []
            [ input
                [ onInput SetFmtstr
                , value model.fmtstr
                , css
                    ([ width (pct 100)
                     , textAlign center
                     , backgroundColor theme.textIn
                     , fontSize (Css.em 1.2)
                     ]
                        ++ textBorder
                    )
                ]
                []
            ]
        , div
            [ css <|
                [ textAlign center
                ]
                    ++ marginInput
            ]
            [ button
                [ onClick Submit
                , css
                    [ backgroundColor theme.btn
                    , border zero
                    , width (px 150)
                    , fontSize (Css.em 1.2)
                    , borderRadius (px 5)
                    , padding (px 5)
                    , cursor pointer
                    ]
                ]
                [ text "Submit" ]
            ]
        , div [] [ h3 [] [ text "Output" ] ]
        , div []
            [ Html.Styled.pre
                [ css
                    [ backgroundColor theme.textIn
                    , borderRadius (px 5)
                    , fontSize (Css.em 1.1)
                    , padding2 (px 10) (px 20)
                    , whiteSpace Css.pre
                    , overflowX auto
                    , cursor text_
                    ]
                ]
                [ code [] [ createMd model |> text ] ]
            ]
        , createErrors model.errs
        ]


createMd : Model -> String
createMd model =
    let
        fmt md =
            String.replace "{title}" md.title model.fmtstr
                |> String.replace "{number}" (String.fromInt md.number)
                |> String.replace "{repo}" md.repo
                |> String.replace "{owner}" md.owner
                |> String.replace "{url}" md.url
    in
    Dict.values model.vars
        |> List.map fmt
        |> String.join "\n"
        |> (\x -> x ++ "\n\n")


createErrors errs =
    let
        error e =
            case e of
                Http.BadUrl s ->
                    "Bad URL: " ++ s

                Http.BadStatus s ->
                    "Request failure with " ++ String.fromInt s

                _ ->
                    "Failed to query Github"

        createErr ( err, link ) =
            div []
                [ text link
                , br [] []
                , text (error err)
                , br [] []
                ]

        errs2 =
            List.map createErr errs
    in
    if List.isEmpty errs2 then
        div [] []

    else
        div
            [ css
                [ backgroundColor (hex "e3955b")
                , border3 (px 1) solid (hex "d1620f")
                , borderRadius (px 5)
                , width (pct 100)
                , padding (px 20)
                ]
            ]
            errs2


marginInput =
    [ marginTop (px 10) ]


textBorder =
    [ border zero, borderRadius (px 10) ]


theme =
    { bg = hex "D7D2CE"
    , textIn = hex "F4F2F0"
    , btn = hex "A99282"
    , font = hex "5c544e"
    }
