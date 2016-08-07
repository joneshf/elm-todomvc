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
type alias BaseModel active completed visibility =
    { active : active
    , completed : completed
    , field : String
    , uid : Int
    , visibility : visibility
    }

type alias StorageModel =
    BaseModel (List (Int, StorageEntry)) (List (Int, StorageEntry)) String

type alias Model =
    BaseModel (Dict Int (Entry Active)) (Dict Int (Entry Completed)) Visibility

type alias BaseEntry description =
    { description : description
    , editing : Bool
    }

type alias StorageEntry =
  BaseEntry String

type alias Entry state =
  Tagged state (BaseEntry NonBlankString)

type Active
  = Active

type Completed
  = Completed

type alias Visibility =
  These Active Completed

emptyModel : Model
emptyModel =
  { active = Dict.empty
  , completed = Dict.empty
  , visibility = These Active Completed
  , field = ""
  , uid = 0
  }

newEntry : String -> Maybe (Entry Active)
newEntry desc =
  entry
    { description = desc
    , editing = False
    }

entry : StorageEntry -> Maybe (Entry a)
entry storageEntry =
  nonEmptyString storageEntry.description
    |> Maybe.map (\str -> Tagged (setDescription str storageEntry))

setEntryEditing : Bool -> Entry a -> Entry a
setEntryEditing =
  taggedMap << setEditing

setEntryDescription : String -> Entry a -> Maybe (Entry a)
setEntryDescription str entry =
  Maybe.map (flip taggedMap entry << setDescription) (nonEmptyString str)

setEditing : a -> {r | editing : b} -> {r | editing : a}
setEditing editing record =
  {record | editing = editing}

mapDescription : (a -> b) -> {r | description : a} -> {r | description : b}
mapDescription f ({description} as record) =
  {record | description = f description}

setDescription : a -> {r | description : b} -> {r | description : a}
setDescription description =
  mapDescription (\_ -> description)

init : Maybe Model -> ( Model, Cmd Msg )
init savedModel =
  Maybe.withDefault emptyModel savedModel ! []

storage
  :  BaseModel (Dict  comparable      (Entry a)) (Dict  comparable      (Entry b)) Visibility
  -> BaseModel (List (comparable, StorageEntry)) (List (comparable, StorageEntry)) String
storage ({active, completed, visibility} as model) =
  { model
  | visibility = visibilityString visibility
  , active =
      active
        |> Dict.map (\_ -> mapDescription string << taggedExtract)
        |> Dict.toList
  , completed =
      completed
        |> Dict.map (\_ -> mapDescription string << taggedExtract)
        |> Dict.toList
  }

egarots
  :         BaseModel (List (comparable, StorageEntry)) (List (comparable, StorageEntry)) String
  -> Maybe (BaseModel (Dict  comparable      (Entry a)) (Dict  comparable      (Entry b)) Visibility)
egarots ({active, completed, visibility} as model) =
  Just
    { model
    | visibility =
        Maybe.withDefault (These Active Completed) (parseVisibility visibility)
    , active =
        List.filterMap (\(x, y) -> Maybe.map ((,) x) (entry y)) active
          |> Dict.fromList
    , completed =
        List.filterMap (\(x, y) -> Maybe.map ((,) x) (entry y)) completed
          |> Dict.fromList
    }

parseVisibility : String -> Maybe Visibility
parseVisibility str =
  case str of
    "All" ->
      Just (These Active Completed)
    "Active" ->
      Just (This Active)
    "Completed" ->
      Just (That Completed)
    _ ->
      Nothing

visibilityString : Visibility -> String
visibilityString =
  these toString toString (\_ _ -> "All")

type NonBlankString
  = NonBlankString String

nonEmptyString : String -> Maybe NonBlankString
nonEmptyString rawString =
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

taggedExtract : Tagged tag value -> value
taggedExtract entry =
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

theseMap : (b -> c) -> These a b -> These a c
theseMap f =
  these This (That << f) (\a -> These a << f)

theseSet : c -> These a b -> These a c
theseSet c =
  theseMap (\_ -> c)

theseBimap : (a -> c) -> (b -> d) -> These a b -> These c d
theseBimap f g =
  these (This << f) (That << g) (\a b -> These (f a) (g b))

theseBiset : c -> d -> These a b -> These c d
theseBiset c d =
  theseBimap (\_ -> c) (\_ -> d)

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
update msg model =
  case msg of
    NoOp ->
      model ! []

    Add ->
      { model
        | uid = model.uid + 1
        , field = ""
        , active =
            newEntry model.field
              |> Maybe.map (\entry -> Dict.insert model.uid entry model.active)
              |> Maybe.withDefault model.active
      }
        ! []

    UpdateField str ->
      { model | field = str }
        ! []

    EditingEntry id ->
      { model
        | active = Dict.update id (Maybe.map (setEntryEditing True)) model.active
        , completed = Dict.update id (Maybe.map (setEntryEditing True)) model.completed
      }
        ! [ focus ("#todo-" ++ toString id) ]

    UpdateEntry id task ->
      { model
        | active = Dict.update id (Maybe.map (setEntryEditing False) << flip Maybe.andThen (setEntryDescription task)) model.active
        , completed = Dict.update id (Maybe.map (setEntryEditing False) << flip Maybe.andThen (setEntryDescription task)) model.completed
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
          Maybe.map (Dict.singleton id << retag) (Dict.get id model.active)
      in
        { model
          | active = Dict.remove id model.active
          , completed = Dict.union (Maybe.withDefault Dict.empty entry) model.completed
        }
          ! []

    Check id False ->
      let
        entry =
          Maybe.map (Dict.singleton id << retag) (Dict.get id model.completed)
      in
        { model
          | active = Dict.union (Maybe.withDefault Dict.empty entry) model.active
          , completed = Dict.remove id model.completed
        }
          ! []

    CheckAll True ->
      { model
        | active = Dict.empty
        , completed = Dict.union (Dict.map (\_ -> retag) model.active) model.completed
      }
        ! []

    CheckAll False ->
      { model
        | active = Dict.union model.active (Dict.map (\_ -> retag) model.completed)
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
        , lazy viewEntries model
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
        , on "keydown" (Json.map (whenEnter Add) keyCode)
        ]
        []
    ]

whenEnter : Msg -> number -> Msg
whenEnter msg code =
  if code == 13 then msg else NoOp


-- VIEW ALL ENTRIES

witness : Dict Int (Entry a) -> a -> Dict Int (Entry a)
witness active _ =
  active

viewEntries : Model -> Html Msg
viewEntries ({active, completed, visibility} as model) =
  let
    visible =
      visibility
        |> theseBimap (witness active) (witness completed)
        |> theseBimap (Dict.map (lazy2 viewActive)) (Dict.map (lazy2 viewCompleted))
        |> these identity identity Dict.union

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


-- VIEW INDIVIDUAL ENTRIES


viewKeyedEntries : Dict Int a -> List (String, a)
viewKeyedEntries =
  Dict.foldr (\id todo -> (::) (toString id, todo)) []

viewActive : Int -> Entry Active -> Html Msg
viewActive id =
  viewEntry False id << taggedExtract

viewCompleted : Int -> Entry Completed -> Html Msg
viewCompleted id =
  viewEntry True id << taggedExtract

viewEntry : Bool -> Int -> BaseEntry NonBlankString -> Html Msg
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
        , on "keydown" (Json.object2 (whenEnter << UpdateEntry todoId) targetValue keyCode)
        ]
        []
    ]



-- VIEW CONTROLS AND FOOTER


viewControls : Visibility -> Dict a (Entry Active) -> Dict b (Entry Completed) -> Html Msg
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
