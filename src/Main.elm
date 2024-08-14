module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
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
                    , path = "/repos" ++ url.path
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


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ textarea
                [ onInput SetLinks
                , on "change" (Dec.succeed Submit)
                , value model.linksText
                ]
                []
            ]
        , div [ ] 
           [ input 
           [ placeholder "Github Personal Access Token" 
           , type_ "password"
           , onInput SetToken
           ]
           []
           ]
        , div []
            [ details []
                [ summary [] [ text "Output Format" ]
                , text "Control the format of each link"
                , br [] []
                , text "Variables are:"
                , ul []
                    [ li [] [ strong [] [ text "{title}" ] , text ": The issue or PR title" ]
                    , li [] [ strong [] [ text "{number}" ] , text ": The issue number" ]
                    , li [] [ strong [] [ text "{repo}" ] , text ": The repository name" ]
                    , li [] [ strong [] [ text "{owner}" ] , text ": The owner of the repository" ]
                    , li [] [ strong [] [ text "{url}" ] , text ": The link url" ]
                    ]
                , input [ onInput SetFmtstr, value model.fmtstr ] []
                ]
            ]
        , div [] [ button [ onClick Submit ] [ text "Submit" ] ]
        , div [] [ pre [] [ code [] [ createMd model |> text ] ] ]
        , div [] (createErrors model.errs)
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


createErrors =
    let
        createErr ( err, link ) =
            div [] [ text link ]
    in
    List.map createErr
