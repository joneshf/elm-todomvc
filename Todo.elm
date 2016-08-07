port module Todo exposing (..)
{-| TodoMVC implemented in Elm, using plain HTML and CSS for rendering.

This application is broken up into three key parts:

  1. Model  - a full definition of the application's state
  2. Update - a way to step the application state forward
  3. View   - a way to visualize our application state with HTML

This clean division of concerns is a core part of Elm. You can read more about
this in <http://guide.elm-lang.org/architecture/index.html>
-}

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Dict as Dict exposing (Dict)
import Json.Decode as Json

import Model exposing
  ( BaseEntry, Entry, Model
  , dependentFields, newEntry, setDescription, setEditing
  )
import NonBlankString
import Storage exposing (storage, egarots)
import Tagged exposing (retag, untag)
import These exposing (these)
import Visibility exposing (Visibility, Active, Completed, all, active, completed)

main : Program (Maybe Storage.Model)
main =
  App.programWithFlags
    { init = init
    , view = view
    , update = updateWithStorage
    , subscriptions = \_ -> Sub.none
    }


port setStorage : Storage.Model -> Cmd msg

port focus : String -> Cmd msg

init : Maybe Storage.Model -> ( Model, Cmd Msg )
init savedModel =
  Maybe.withDefault Model.empty (Maybe.map egarots savedModel) ! []

-- UPDATE


{-| Users of our app can trigger messages by clicking and typing. These
messages are fed into the `update` function as they occur, letting us react
to them.
-}
type Msg
    = UpdateField String
    | EditingEntry Int
    | UpdateEntry Int String
    | Add
    | Delete Int
    | DeleteComplete
    | Check Int Bool
    | CheckAll Bool
    | ChangeVisibility Visibility


-- How we update our Model on a given Msg?
update : Msg -> Model -> ( Model, Cmd a )
update msg =
  updateCmd msg
    << dependentFields
    << updateModel msg

{-| We want to `setStorage` on every update. This function adds the setStorage
command for every step of the update function.
-}
updateWithStorage : Msg -> Model -> (Model, Cmd a)
updateWithStorage msg model =
  let
    (newModel, cmds) =
      update msg model
  in
    newModel ! [ setStorage (storage newModel), cmds ]

updateModel : Msg -> Model -> Model
updateModel msg ({active, completed, uid, field} as model) =
  case msg of
    Add ->
      { model | uid = uid + 1, field = "", active = Dict.update uid (\_ -> newEntry field) active }

    UpdateField str ->
      { model | field = str }

    EditingEntry id ->
      { model
        | active = Dict.update id (Maybe.map (setEditing True)) active
        , completed = Dict.update id (Maybe.map (setEditing True)) completed
      }

    UpdateEntry id task ->
      { model
        | active = Dict.update id (flip Maybe.andThen (setDescription task << setEditing False)) active
        , completed = Dict.update id (flip Maybe.andThen (setDescription task << setEditing False)) completed
      }

    Delete id ->
      { model | active = Dict.remove id active, completed = Dict.remove id completed }

    DeleteComplete ->
      { model | completed = Dict.empty }

    Check id True ->
      { model
        | active = Dict.remove id active
        , completed = Dict.update id (\_ -> Maybe.map retag (Dict.get id active)) completed
      }

    Check id False ->
      { model
        | active = Dict.update id (\_ -> Maybe.map retag (Dict.get id completed)) active
        , completed = Dict.remove id completed
      }

    CheckAll True ->
      { model
        | active = Dict.empty
        , completed = Dict.union (Dict.map (\_ -> retag) active) completed
      }

    CheckAll False ->
      { model
        | active = Dict.union active (Dict.map (\_ -> retag) completed)
        , completed = Dict.empty
      }

    ChangeVisibility visibility ->
      { model | visibility = visibility }


updateCmd : Msg -> a -> (a, Cmd b)
updateCmd msg x =
  case msg of
    EditingEntry id ->
      x ! [ focus ("#todo-" ++ toString id) ]
    _ ->
      x ! []

-- VIEW


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


viewKeyedEntries : Dict Int a -> List (String, a)
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


viewControlsCount : Int -> Html Msg
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
