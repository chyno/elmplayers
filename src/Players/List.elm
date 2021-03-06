module Players.List exposing (..)
import Html.Events exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Models exposing (Player)
import Msgs exposing (Msg)
import RemoteData exposing (WebData)
import Routing exposing (playerPath, newplayerPath)
import Json.Decode as Json

view : String ->  WebData (List Player) -> Html Msg
view fltr response =
    div []
        [ nav
        , maybeList fltr response
        ]


nav : Html Msg
nav =
    div [ class "clearfix mb2 white bg-black" ]
        [ div [ class "left p2" ] [ text "Players" ] ]


maybeList : String ->  WebData (List Player) -> Html Msg
maybeList fltr response =
    case response of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            text "Loading..."

        RemoteData.Success players ->
            players |>
            filterPlayers fltr |>
            list  

        RemoteData.Failure error ->
            text (toString error)


list : List Player -> Html Msg
list players =
    div [ class "p2" ]
        [ 
            filterPlayerPage,
            table []
            [ thead []
                [ tr []
                    [ th [] [ text "Id" ]
                    , th [] [ text "Name" ]
                    , th [] [ text "Level" ]
                    , th [] [ text "Actions" ]
                    ]
                ]
            , tbody [] (List.map playerRow players)
            ]
        ]

playerRow : Player -> Html Msg
playerRow player =
    tr []
        [ td [] [ text player.id ]
        , td [] [ text player.name ]
        , td [] [ text (toString player.level) ]
        , td []
            [ editBtn player ]
        ]

addBtn :  Html.Html Msg
addBtn  =
  a [class "btn regular" , href  newplayerPath ]
    [ i [ class "fa fa-pencil mr1" ] [], text "Add" ]

editBtn : Player -> Html.Html Msg
editBtn player =
    let
        path =
            playerPath player.id
    in
        a
            [ class "btn regular"
            , href path
            ]
            [ i [ class "fa fa-pencil mr1" ] [], text "Edit" ]

filterPlayerPage :  Html Msg
filterPlayerPage   =
  div 
  []
  [
      label[][text "Filter:"],
      input
      [
         placeholder "Search by Name"
         , onInput Msgs.Setfilter       
      ]
      [] ,
      addBtn 
  ]

filterPlayers : String -> List Player -> List Player
filterPlayers filterstr players =
  if String.isEmpty filterstr then
    players
  else
   List.filter  (\ cp  -> String.contains filterstr  cp.name) players
         
onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not ENTER"
    in
      on "keydown" (Json.andThen isEnter keyCode)
