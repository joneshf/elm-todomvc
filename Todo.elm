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
import String



main : Program (Maybe StorageModel)
main =
  App.programWithFlags
    { init = \storageModel -> init (Maybe.andThen storageModel egarots)
    , view = view
    , update = updateWithStorage
    , subscriptions = \_ -> Sub.none
    }


port setStorage : StorageModel -> Cmd msg

port focus : String -> Cmd msg


{-| We want to `setStorage` on every update. This function adds the setStorage
command for every step of the update function.
-}
updateWithStorage : Msg -> Model -> (Model, Cmd Msg)
updateWithStorage msg model =
  let
    (newModel, cmds) =
      update msg model
  in
    ( newModel
    , Cmd.batch [ setStorage (storage newModel), cmds ]
    )



-- MODEL


-- The full application state of our todo app.
type alias BaseModel entries visibility =
    { active : entries
    , completed : entries
    , field : String
    , uid : Int
    , visibility : visibility
    }

type alias StorageModel =
    BaseModel (List (Int, StorageEntry)) String

type alias Model =
    BaseModel (Dict Int Entry) Visibility


type alias StorageEntry =
    { description : String
    , editing : Bool
    }

type alias Entry =
  BaseEntry StorageEntry

type BaseEntry a
  = Active a
  | Completed a

type Visibility
  = AllEntries
  | ActiveEntries
  | CompletedEntries

emptyModel : Model
emptyModel =
  { active = Dict.empty
  , completed = Dict.empty
  , visibility = AllEntries
  , field = ""
  , uid = 0
  }

baseEntryMap : (a -> b) -> BaseEntry a -> BaseEntry b
baseEntryMap f base =
  case base of
    Active a ->
      Active (f a)
    Completed a ->
      Completed (f a)

baseEntryExtract : BaseEntry a -> a
baseEntryExtract base =
  case base of
    Active a ->
      a
    Completed a ->
      a

newEntry : String -> Entry
newEntry desc =
  Active
    { description = desc
    , editing = False
    }

setEditing : a -> {r | editing : b} -> {r | editing : a}
setEditing editing record =
  {record | editing = editing}

setDescription : a -> {r | description : b} -> {r | description : a}
setDescription description record =
  {record | description = description}

init : Maybe Model -> ( Model, Cmd Msg )
init savedModel =
  Maybe.withDefault emptyModel savedModel ! []

storage
  :  BaseModel (Dict  comparable (BaseEntry a)) Visibility
  -> BaseModel (List (comparable,           a)) String
storage ({active, completed, visibility} as model) =
  { model
  | visibility = visibilityString visibility
  , active = Dict.map (\_ -> baseEntryExtract) active |> Dict.toList
  , completed = Dict.map (\_ -> baseEntryExtract) completed |> Dict.toList
  }

egarots
  :         BaseModel (List (comparable,            a)) String
  -> Maybe (BaseModel (Dict  comparable  (BaseEntry a)) Visibility)
egarots ({active, completed, visibility} as model) =
  let
    go v =
      { model
      | visibility = v
      , active = Dict.fromList active |> Dict.map (\_ -> Active)
      , completed = Dict.fromList completed |> Dict.map (\_ -> Completed)
      }
  in
    Maybe.map go (parseVisibility visibility)

parseVisibility : String -> Maybe Visibility
parseVisibility str =
  case str of
    "All" ->
      Just AllEntries
    "Active" ->
      Just ActiveEntries
    "Completed" ->
      Just CompletedEntries
    _ ->
      Nothing

visibilityString : Visibility -> String
visibilityString visibility =
  case visibility of
    AllEntries ->
      "All"
    ActiveEntries ->
      "Active"
    CompletedEntries ->
      "Completed"

-- UPDATE


{-| Users of our app can trigger messages by clicking and typing. These
messages are fed into the `update` function as they occur, letting us react
to them.
-}
type Msg
    = NoOp
    | UpdateField String
    | EditingEntry Int Bool
    | UpdateEntry Int String
    | Add
    | Delete Int
    | DeleteComplete
    | Check Int Bool
    | CheckAll Bool
    | ChangeVisibility Visibility


-- How we update our Model on a given Msg?
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      model ! []

    Add ->
      { model
        | uid = model.uid + 1
        , field = ""
        , active =
            Dict.insert model.uid (newEntry model.field) model.active
      }
        ! []

    UpdateField str ->
      { model | field = str }
        ! []

    EditingEntry id isEditing ->
      { model
        | active = Dict.update id (Maybe.map (baseEntryMap (setEditing isEditing))) model.active
        , completed = Dict.update id (Maybe.map (baseEntryMap (setEditing isEditing))) model.completed
      }
        ! [ focus ("#todo-" ++ toString id) ]

    UpdateEntry id task ->
      { model
        | active = Dict.update id (Maybe.map (baseEntryMap (setDescription task))) model.active
        , completed = Dict.update id (Maybe.map (baseEntryMap (setDescription task))) model.completed
      }
        ! []

    Delete id ->
      { model
        | active = Dict.remove id model.active
        , completed = Dict.remove id model.completed
      }
        ! []

    DeleteComplete ->
      { model | completed = Dict.empty }
        ! []

    Check id True ->
      let
        entry =
          Maybe.map (Dict.singleton id << Completed << baseEntryExtract) (Dict.get id model.active)
      in
        { model
          | active = Dict.remove id model.active
          , completed = Dict.union (Maybe.withDefault Dict.empty entry) model.completed
        }
          ! []

    Check id False ->
      let
        entry =
          Maybe.map (Dict.singleton id << Active << baseEntryExtract) (Dict.get id model.completed)
      in
        { model
          | active = Dict.union (Maybe.withDefault Dict.empty entry) model.active
          , completed = Dict.remove id model.completed
        }
          ! []

    CheckAll True ->
      { model
        | active = Dict.empty
        , completed = Dict.union (Dict.map (\_ -> Completed << baseEntryExtract) model.active) model.completed
      }
        ! []

    CheckAll False ->
      { model
        | active = Dict.union model.active (Dict.map (\_ -> Active << baseEntryExtract) model.completed)
        , completed = Dict.empty
      }
        ! []

    ChangeVisibility visibility ->
      { model | visibility = visibility }
        ! []



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
        , lazy3 viewEntries model.visibility model.active model.completed
        , lazy3 viewControls model.visibility model.active model.completed
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
        , onEnter (if String.isEmpty task then NoOp else Add)
        ]
        []
    ]


onEnter : Msg -> Attribute Msg
onEnter msg =
  let
    tagger code =
      if code == 13 then msg else NoOp
  in
    on "keydown" (Json.map tagger keyCode)



-- VIEW ALL ENTRIES


viewEntries : Visibility -> Dict Int Entry -> Dict Int Entry -> Html Msg
viewEntries visibility active completed =
  let
    visible =
      case visibility of
        CompletedEntries -> Dict.map (lazy2 viewEntry) completed
        ActiveEntries -> Dict.map (lazy2 viewEntry) active
        AllEntries -> Dict.union (Dict.map (lazy2 viewEntry) active) (Dict.map (lazy2 viewEntry) completed)

    allCompleted =
      Dict.isEmpty active

    cssVisibility =
      if Dict.isEmpty active && Dict.isEmpty completed then
        "hidden"
      else
        "visible"
  in
    section
      [ class "main"
      , style [ ("visibility", cssVisibility) ]
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
      , Keyed.ul [ class "todo-list" ] <|
          viewKeyedEntries visible
      ]

dictAll : (b -> Bool) -> Dict comparable b -> Bool
dictAll p =
  Dict.foldl (\_ b acc -> acc && p b) True


-- VIEW INDIVIDUAL ENTRIES


viewKeyedEntries : Dict Int a -> List (String, a)
viewKeyedEntries =
  Dict.foldr (\id todo -> (::) (toString id, todo)) []

viewEntry : Int -> Entry -> Html Msg
viewEntry todoId entry =
  case entry of
    Active todo ->
      viewEntry' False todoId todo
    Completed todo ->
      viewEntry' True todoId todo

viewEntry' : Bool -> Int -> StorageEntry -> Html Msg
viewEntry' bool todoId todo =
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
            [ onDoubleClick (EditingEntry todoId bool) ]
            [ text todo.description ]
        , button
            [ class "destroy"
            , onClick (Delete todoId)
            ]
            []
        ]
    , input
        [ class "edit"
        , value todo.description
        , name "title"
        , id ("todo-" ++ toString todoId)
        , onInput (UpdateEntry todoId)
        , onBlur (EditingEntry todoId False)
        , onEnter (EditingEntry todoId False)
        ]
        []
    ]



-- VIEW CONTROLS AND FOOTER


viewControls : Visibility -> Dict Int Entry -> Dict Int Entry -> Html Msg
viewControls visibility active completed =
  footer
    [ class "footer"
    , hidden (Dict.isEmpty active && Dict.isEmpty completed)
    ]
    [ lazy viewControlsCount (Dict.size active)
    , lazy viewControlsFilters visibility
    , lazy viewControlsClear (Dict.size completed)
    ]


viewControlsCount : Int -> Html Msg
viewControlsCount entriesLeft =
  let
    item_ =
      if entriesLeft == 1 then " item" else " items"
  in
    span
      [ class "todo-count" ]
      [ strong [] [ text (toString entriesLeft) ]
      , text (item_ ++ " left")
      ]


viewControlsFilters : Visibility -> Html Msg
viewControlsFilters visibility =
  ul
    [ class "filters" ]
    [ visibilitySwap "#/" AllEntries visibility
    , text " "
    , visibilitySwap "#/active" ActiveEntries visibility
    , text " "
    , visibilitySwap "#/completed" CompletedEntries visibility
    ]


visibilitySwap : String -> Visibility -> Visibility -> Html Msg
visibilitySwap uri visibility actualVisibility =
  li
    [ onClick (ChangeVisibility visibility) ]
    [ a [ href uri, classList [("selected", visibility == actualVisibility)] ]
        [ text (visibilityString visibility) ]
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
