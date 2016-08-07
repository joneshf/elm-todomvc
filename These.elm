module These exposing (..)

type These a b
  = This a
  | That b
  | These a b

bimap : (a -> c) -> (b -> d) -> These a b -> These c d
bimap f g =
  these (This << f) (That << g) (\a b -> These (f a) (g b))

these : (a -> c) -> (b -> c) -> (a -> b -> c) -> These a b -> c
these f g h these =
  case these of
    This a ->
      f a
    That b ->
      g b
    These a b ->
      h a b
