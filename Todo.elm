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
    { init = init << Maybe.map egarots
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
type alias Model =
  { active : Dict Int (Entry Active)
  , allCompleted : Bool
  , completed : Dict Int (Entry Completed)
  , entriesCompleted : Int
  , entriesLeft : Int
  , field : String
  , noneVisible : Bool
  , uid : Int
  , visibility : Visibility
  }

type alias StorageModel =
  List StorageEntry

type alias BaseEntry =
  { description : NonBlankString
  , editing : Bool
  }

type alias StorageEntry =
  { completed : Bool
  , id : Int
  , title : String
  }

type alias Entry state =
  Tagged state BaseEntry

type Active
  = Active

type Completed
  = Completed

type alias Visibility =
  These Active Completed

emptyModel : Model
emptyModel =
  { active = Dict.empty
  , allCompleted = False
  , completed = Dict.empty
  , entriesCompleted = 0
  , entriesLeft = 0
  , field = ""
  , noneVisible = True
  , uid = 0
  , visibility = These Active Completed
  }

allCompleted : Model -> Model
allCompleted ({active, completed} as model) =
  {model | allCompleted = Dict.isEmpty active && not (Dict.isEmpty completed)}

entriesCompleted : Model -> Model
entriesCompleted ({completed} as model) =
  {model | entriesCompleted = Dict.size completed}

entriesLeft : Model -> Model
entriesLeft ({active} as model) =
  {model | entriesLeft = Dict.size active}

noneVisible : Model -> Model
noneVisible ({active, completed} as model) =
  {model | noneVisible = Dict.isEmpty active && Dict.isEmpty completed}

uid : Model -> Model
uid ({active, completed} as model) =
  { model
  | uid =
      Maybe.map2 Basics.max (List.maximum (Dict.keys active)) (List.maximum (Dict.keys completed))
        |> Maybe.withDefault 0
        |> (+) 1
  }

newEntry : String -> Maybe (Entry Active)
newEntry desc =
  entry
    { completed = False
    , id = 0
    , title = desc
    }

entry : StorageEntry -> Maybe (Entry a)
entry storageEntry =
  nonBlankString storageEntry.title
    |> Maybe.map (\str -> Tagged {description = str, editing = False})

setEntryEditing : Bool -> Entry a -> Entry a
setEntryEditing =
  taggedMap << setEditing

setEntryDescription : String -> Entry a -> Maybe (Entry a)
setEntryDescription str entry =
  Maybe.map (flip taggedMap entry << setDescription) (nonBlankString str)

setEditing : a -> {r | editing : b} -> {r | editing : a}
setEditing editing record =
  {record | editing = editing}

mapDescription : (a -> b) -> {r | description : a} -> {r | description : b}
mapDescription f ({description} as record) =
  {record | description = f description}

setDescription : a -> {r | description : b} -> {r | description : a}
setDescription =
  mapDescription << always

init : Maybe Model -> ( Model, Cmd Msg )
init savedModel =
  Maybe.withDefault emptyModel savedModel ! []

storage : Model -> StorageModel
storage ({active, completed, visibility} as model) =
  storageEntry False active ++ storageEntry True completed

egarots : StorageModel -> Model
egarots storageModel =
  let
    (completed, active) =
      List.partition .completed storageModel
  in
    { emptyModel
    | active = yrtneEgarots active
    , completed = yrtneEgarots completed
    }
      |> updateDependents
      |> uid

yrtneEgarots : List StorageEntry -> Dict Int (Entry a)
yrtneEgarots =
  Dict.fromList << List.filterMap (\y -> Maybe.map ((,) y.id) (entry y))

storageEntry : Bool -> Dict comparable (Entry a) -> List StorageEntry
storageEntry completed =
  Dict.values << Dict.map (\id tagged ->
    { completed = completed, id = id, title = string (untag tagged).description}
  )

witness : Dict a (Entry b) -> b -> Dict a (Entry b)
witness =
  always

parseVisibility : String -> Visibility
parseVisibility str =
  case str of
    "Active" ->
      This Active
    "Completed" ->
      That Completed
    _ ->
      These Active Completed

visibilityString : Visibility -> String
visibilityString =
  these toString toString (\_ _ -> "All")

type NonBlankString
  = NonBlankString String

nonBlankString : String -> Maybe NonBlankString
nonBlankString rawString =
  case String.trim rawString of
    "" ->
      Nothing
    str ->
      Just (NonBlankString str)

string : NonBlankString -> String
string (NonBlankString str) =
  str

type Tagged tag value
  = Tagged value

retag : Tagged oldTag value -> Tagged newTag value
retag (Tagged x) =
  Tagged x

taggedMap : (oldValue -> newValue) -> Tagged tag oldValue -> Tagged tag newValue
taggedMap f entry =
  case entry of
    Tagged todo ->
      Tagged (f todo)

untag : Tagged tag value -> value
untag entry =
  case entry of
    Tagged a ->
      a

type These a b
  = This a
  | That b
  | These a b

these : (a -> c) -> (b -> c) -> (a -> b -> c) -> These a b -> c
these f g h these =
  case these of
    This a ->
      f a
    That b ->
      g b
    These a b ->
      h a b

theseBimap : (a -> c) -> (b -> d) -> These a b -> These c d
theseBimap f g =
  these (This << f) (That << g) (\a b -> These (f a) (g b))

-- UPDATE


{-| Users of our app can trigger messages by clicking and typing. These
messages are fed into the `update` function as they occur, letting us react
to them.
-}
type Msg
    = NoOp
    | UpdateField String
    | EditingEntry Int
    | UpdateEntry Int String
    | Add
    | Delete Int
    | DeleteComplete
    | Check Int Bool
    | CheckAll Bool
    | ChangeVisibility Visibility


-- How we update our Model on a given Msg?
update : Msg -> Model -> ( Model, Cmd Msg )
update msg =
  updateCmd msg
    << updateDependents
    << updateModel msg

updateDependents : Model -> Model
updateDependents =
  allCompleted
  << entriesCompleted
  << entriesLeft
  << noneVisible

updateModel : Msg -> Model -> Model
updateModel msg ({active, completed} as model) =
  case msg of
    NoOp ->
      model

    Add ->
      { model
        | uid = model.uid + 1
        , field = ""
        , active =
            newEntry model.field
              |> Maybe.map (\entry -> Dict.insert model.uid entry active)
              |> Maybe.withDefault active
      }

    UpdateField str ->
      { model | field = str }

    EditingEntry id ->
      { model
        | active = Dict.update id (Maybe.map (setEntryEditing True)) active
        , completed = Dict.update id (Maybe.map (setEntryEditing True)) completed
      }

    UpdateEntry id task ->
      { model
        | active = Dict.update id (flip Maybe.andThen (setEntryDescription task << setEntryEditing False)) active
        , completed = Dict.update id (flip Maybe.andThen (setEntryDescription task << setEntryEditing False)) completed
      }

    Delete id ->
      { model
        | active = Dict.remove id active
        , completed = Dict.remove id completed
      }

    DeleteComplete ->
      { model | completed = Dict.empty }

    Check id True ->
      let
        entry =
          Maybe.map (Dict.singleton id << retag) (Dict.get id active)
      in
        { model
          | active = Dict.remove id active
          , completed = Dict.union (Maybe.withDefault Dict.empty entry) completed
        }

    Check id False ->
      let
        entry =
          Maybe.map (Dict.singleton id << retag) (Dict.get id completed)
      in
        { model
          | active = Dict.union (Maybe.withDefault Dict.empty entry) active
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


updateCmd : Msg -> a -> (a, Cmd Msg)
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
        , on "keydown" (Json.map (whenEnter Add) keyCode)
        ]
        []
    ]

whenEnter : Msg -> number -> Msg
whenEnter msg code =
  if code == 13 then msg else NoOp

-- VIEW ALL ENTRIES

viewEntries : Model -> Html Msg
viewEntries {active, allCompleted, noneVisible, completed, visibility} =
  let
    visible =
      visibility
        |> theseBimap (witness active) (witness completed)
        |> theseBimap (Dict.map (lazy2 viewActive)) (Dict.map (lazy2 viewCompleted))
        |> these identity identity Dict.union
  in
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
      , Keyed.ul [ class "todo-list" ] <|
          viewKeyedEntries visible
      ]


-- VIEW INDIVIDUAL ENTRIES


viewKeyedEntries : Dict Int a -> List (String, a)
viewKeyedEntries =
  Dict.foldr (\id todo -> (::) (toString id, todo)) []

viewActive : Int -> Entry Active -> Html Msg
viewActive id =
  viewEntry False id << untag

viewCompleted : Int -> Entry Completed -> Html Msg
viewCompleted id =
  viewEntry True id << untag

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
            [ text (string todo.description) ]
        , button
            [ class "destroy"
            , onClick (Delete todoId)
            ]
            []
        ]
    , input
        [ class "edit"
        , value (string todo.description)
        , name "title"
        , id ("todo-" ++ toString todoId)
        , on "blur" (Json.map (UpdateEntry todoId) targetValue)
        , on "keydown"(Json.object2 (\value keyCode ->
            case keyCode of
              13 ->
                UpdateEntry todoId value
              27 ->
                UpdateEntry todoId (string todo.description)
              _ ->
                NoOp
          ) targetValue keyCode)
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
    [ visibilitySwap "#/" (These Active Completed) visibility
    , text " "
    , visibilitySwap "#/active" (This Active) visibility
    , text " "
    , visibilitySwap "#/completed" (That Completed) visibility
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
