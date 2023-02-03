module Main exposing (..)

import Browser
import Html exposing (..)
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, field, list, string)

-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

-- INIT

init : () -> ( Model, Cmd Msg )
init _ =
    ( { mot = []
        , message_d_erreur = Nothing
        , jsonState = Chargement
        , mot_mis = Nothing
        , devinette_mise = Nothing
        , mot_a_deviner = ""
        , tentatives = 0
      }
    , getMot
    )


getDefinition : String -> Cmd Msg
getDefinition (mot)=
    Http.get
        { url = "https://api.dictionaryapi.dev/api/v2/entries/en/"++mot
        , expect = Http.expectJson Definitionfetched decodeur1
        }

getMot : Cmd Msg
getMot =
    Http.get
        { url = "http://127.0.0.1:5000/random"
        , expect = Http.expectString Motfetched
        }

-- MODEL

type alias Model =
      { mot : Mot
      , jsonState : Statut
      , message_d_erreur : Maybe String
      , mot_mis : Maybe String
      , devinette_mise : Maybe String
      , mot_a_deviner : String
      , tentatives : Int
    }

type alias Mot = List Description

type alias Description =
    { mot : String
    , significations : List Signification
    }

type alias Signification =
    { partOfSpeech : String
    , definitions : List Definition
    }

type alias Definition =
    {
      definition : String
    }

type Statut = Success | Echec | Chargement

type Msg
    =  TextEntered String
    | GuessSubmitted
    | Abandonclicked
    | RequestWordSend
    | Motfetched (Result Http.Error String)
    | Definitionfetched (Result Http.Error Mot)

-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TextEntered string_in ->
            ( { model | mot_mis = Just string_in }
            , Cmd.none
            )

        GuessSubmitted ->
            ( { model
                | mot_mis = Nothing
                , devinette_mise = model.mot_mis
                , tentatives = model.tentatives + 1
              }
            , Cmd.none
            )
            
        Abandonclicked ->
            ( { model
                | mot_mis = Nothing
                , devinette_mise = Just "J'abandonne"
                , tentatives = model.tentatives + 1
              }
            , Cmd.none
            )

        RequestWordSend ->
            ( { model | tentatives = 0}, getMot )

        Motfetched (Ok wordStr) ->
          let 
            mot = String.dropRight 2 (String.dropLeft 2 wordStr)
          in 
            ( {model | mot_a_deviner = mot}, getDefinition (mot) )

        Motfetched (Err httpError) ->
            ( { model
                | message_d_erreur = Just (message_d_erreurs httpError)
              }
            , getMot
            )
        Definitionfetched (Ok desc) ->
            ( { model | jsonState = Success , mot = desc }, Cmd.none )

        Definitionfetched (Err httpError) ->
            ( { model | message_d_erreur = Just (message_d_erreurs httpError), jsonState = Echec
              }
            , getMot
            )
                               
-- VIEW
view : Model -> Html Msg
view model =
    case model.jsonState of
            Success -> div [style "border" "3px solid black", style "padding" "10px", style "background-color" "AliceBlue", style "border-color" "black"] ([h1 [ style "text-align" "center", style "font-style" "italic", style "color" "green" ] [ text "GUESS IF YOU CAN!"] ] ++[img [src "ques6.png",style "width" "40%" ,style "height" "20%"] []]++affichage model.mot ++ [viewInterface model])
      
            Chargement -> div [] 
                [ button [ onClick RequestWordSend]
                [ text "Play"]]

            Echec -> div [] 
                [ button [ onClick RequestWordSend]
                    [ text "Play Again"], viewWordOrError model]

viewInterface : Model -> Html Msg
viewInterface model =
    let
       inputDiv = div [] 
          [
            div[]
            [input [ type_ "text", onInput TextEntered, model.mot_mis |> Maybe.withDefault "" |> value][]
              , button[ style "background-color" "green" ,style "color" "white",style "height" "25px", style "width" "300px", onClick GuessSubmitted ][ text "Devinez si vous pensez pouvoir le faire" ]
              , button[ style "background-color" "red" ,style "color" "white", onClick Abandonclicked ][ text "Ou Abandonnez car c'est facile!" ]
            ]
            , div [] [ text <| "Tentatives: " ++ String.fromInt model.tentatives]  
          ]
    in
    case model.devinette_mise of
        Just "J'abandonne" ->
            div [] [ inputDiv, div [] [ text <| "Le mot à deviner est : " ++ model.mot_a_deviner]]
            
        Just guess ->
            if guess == model.mot_a_deviner then
                div [] [ button[ onClick RequestWordSend ][ text "Jouer encore" ], div [] [ text <| "Vous avez réussi, félicitations " ++ model.mot_a_deviner ] ]

            else
                div [] [ inputDiv, div [] [ text "Faux, veuillez réessayer" ] ]

        Nothing ->
            div [] [ inputDiv]
            
        

affichage : Mot -> List (Html Msg)
affichage list = case list of
    [] -> []
    (x::xs) -> [div [] ([ul [] (affichage_significations x.significations)])] ++ (affichage xs) 

    
viewWordOrError : Model -> Html Msg
viewWordOrError model =
    case model.message_d_erreur of
        Just message ->
            viewError message

        Nothing ->
             viewError "erreur"


viewError : String -> Html Msg
viewError message_d_erreur =
    let
        errorHeading =
            "Couldn't fetch mot at this time."
    in
    div []
        [ h3 [] [ text errorHeading ]
        , text ("Error: " ++ message_d_erreur)
        ]

affichage_significations : List Signification -> List (Html Msg)
affichage_significations list = case list of
    [] -> []
    (x::xs) -> [li [] [text x.partOfSpeech]] ++ [ol [] (affichage_de_definitions x.definitions)] ++ (affichage_significations xs)

affichage_de_definitions : List Definition -> List (Html Msg)
affichage_de_definitions list = case list of
    [] -> []
    (x::xs) -> [li [] [text x.definition]] ++ (affichage_de_definitions xs)


decodeur1 : Decoder (List Description)
decodeur1 = Decode.list decodeur2

decodeur2 : Decoder Description
decodeur2 = 
            Decode.map2 Description
              (field "word" string)
              (field "meanings" <| Decode.list decodeur3)

decodeur3 : Decoder Signification
decodeur3 =
            Decode.map2 Signification
              (field "partOfSpeech" string)
              (field "definitions" <| Decode.list decodeur4)

decodeur4 : Decoder Definition
decodeur4 =
            Decode.map Definition
              (field "definition" string)

message_d_erreurs : Http.Error -> String
message_d_erreurs httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Réessayez ultérieurement."

        Http.NetworkError ->
            "Problème!->Serveur"

        Http.BadStatus statusCode ->
            "Echec de la requête: " ++ String.fromInt statusCode

        Http.BadBody message ->
            message
            

