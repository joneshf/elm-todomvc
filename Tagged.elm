module Tagged exposing (Tagged, map, retag, tag, untag)

type Tagged tag value
  = Tagged value

map : (oldValue -> newValue) -> Tagged tag oldValue -> Tagged tag newValue
map f (Tagged todo) =
  Tagged (f todo)

retag : Tagged oldTag value -> Tagged newTag value
retag (Tagged x) =
  Tagged x

tag : value -> Tagged tag value
tag =
  Tagged

untag : Tagged tag value -> value
untag (Tagged a) =
  a
