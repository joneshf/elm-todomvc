module View exposing (view)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import Json.Decode as Json

import Model exposing (BaseEntry, Entry, Model)
import NonBlankString exposing (NonBlankString)
import Tagged exposing (untag)
import These exposing (these)
import Update exposing (Msg(..))
import Visibility exposing
  ( Visibility, Active, Completed
  , active, all, completed
  )

view : Model -> Html Msg
view model =
  div
    [ class "todomvc-wrapper"
    , style [ ("visibility", "hidden") ]
    ]
    [ section
        [ class "todoapp" ]
        [ lazy viewInput model.field
        , lazy viewEntries model
        , lazy viewControls model
        ]
    , infoFooter
    ]


viewInput : String -> Html Msg
viewInput task =
  header
    [ class "header" ]
    [ h1 [] [ text "todos" ]
    , input
        [ class "new-todo"
        , placeholder "What needs to be done?"
        , autofocus True
        , value task
        , name "newTodo"
        , onInput UpdateField
        , on "keydown" (keyCode `Json.andThen` \code ->
            case code of
              13 -> Json.succeed Add
              _ -> Json.fail "Not <enter>"
          )
        ]
        []
    ]

-- VIEW ALL ENTRIES

viewEntries : Model -> Html Msg
viewEntries {active, allCompleted, noneVisible, completed, visibility} =
  section
    [ class "main"
    , style [ ("visibility", if noneVisible then "hidden" else "visible") ]
    ]
    [ input
        [ class "toggle-all"
        , type' "checkbox"
        , name "toggle"
        , checked allCompleted
        , onClick (CheckAll (not allCompleted))
        ]
        []
    , label
        [ for "toggle-all" ]
        [ text "Mark all as complete" ]
    , Keyed.ul [ class "todo-list" ]
        (visibility
          |> These.bimap (witness active) (witness completed)
          |> These.bimap (Dict.map (lazy2 viewActive)) (Dict.map (lazy2 viewCompleted))
          |> these identity identity Dict.union
          |> viewKeyedEntries
        )
    ]

witness : Dict a (Entry b) -> b -> Dict a (Entry b)
witness =
  always

-- VIEW INDIVIDUAL ENTRIES


viewKeyedEntries : Dict comparable a -> List (String, a)
viewKeyedEntries =
  Dict.foldr (\id todo todos-> (toString id, todo) :: todos) []

viewActive : Int -> Entry Active -> Html Msg
viewActive id entry =
  viewEntry False id (untag entry)

viewCompleted : Int -> Entry Completed -> Html Msg
viewCompleted id entry =
  viewEntry True id (untag entry)

viewEntry : Bool -> Int -> BaseEntry -> Html Msg
viewEntry bool todoId todo =
  li
    [ classList [ ("completed", bool), ("editing", todo.editing) ] ]
    [ div
        [ class "view" ]
        [ input
            [ class "toggle"
            , type' "checkbox"
            , checked bool
            , onClick (Check todoId (not bool))
            ]
            []
        , label
            [ onDoubleClick (EditingEntry todoId) ]
            [ text (NonBlankString.string todo.description) ]
        , button
            [ class "destroy"
            , onClick (Delete todoId)
            ]
            []
        ]
    , input
        [ class "edit"
        , value (NonBlankString.string todo.description)
        , name "title"
        , id ("todo-" ++ toString todoId)
        , on "blur" (Json.map (UpdateEntry todoId) targetValue)
        , on "keydown" (keyCode `Json.andThen` \code ->
            case code of
              13 -> Json.map (UpdateEntry todoId) targetValue
              27 -> Json.succeed (UpdateEntry todoId (NonBlankString.string todo.description))
              _ -> Json.fail "Not <enter> or <esc>"
          )
        ]
        []
    ]

-- VIEW CONTROLS AND FOOTER


viewControls : Model -> Html Msg
viewControls {entriesCompleted, entriesLeft, noneVisible, visibility} =
  footer
    [ class "footer"
    , hidden noneVisible
    ]
    [ lazy viewControlsCount entriesLeft
    , lazy viewControlsFilters visibility
    , lazy viewControlsClear entriesCompleted
    ]


viewControlsCount : Int -> Html msg
viewControlsCount entriesLeft =
  span
    [ class "todo-count" ]
    [ strong [] [ text (toString entriesLeft) ]
    , text (if entriesLeft == 1 then " item left" else " items left")
    ]


viewControlsFilters : Visibility -> Html Msg
viewControlsFilters visibility =
  ul
    [ class "filters" ]
    [ visibilitySwap "#/" all visibility
    , text " "
    , visibilitySwap "#/active" active visibility
    , text " "
    , visibilitySwap "#/completed" completed visibility
    ]


visibilitySwap : String -> Visibility -> Visibility -> Html Msg
visibilitySwap uri visibility actualVisibility =
  li
    [ onClick (ChangeVisibility visibility) ]
    [ a [ href uri, classList [("selected", visibility == actualVisibility)] ]
        [ text (Visibility.string visibility) ]
    ]


viewControlsClear : Int -> Html Msg
viewControlsClear entriesCompleted =
  button
    [ class "clear-completed"
    , hidden (entriesCompleted == 0)
    , onClick DeleteComplete
    ]
    [ text ("Clear completed (" ++ toString entriesCompleted ++ ")")
    ]


infoFooter : Html msg
infoFooter =
  footer [ class "info" ]
    [ p [] [ text "Double-click to edit a todo" ]
    , p []
        [ text "Written by "
        , a [ href "https://github.com/evancz" ] [ text "Evan Czaplicki" ]
        ]
    , p []
        [ text "Part of "
        , a [ href "http://todomvc.com" ] [ text "TodoMVC" ]
        ]
    ]
