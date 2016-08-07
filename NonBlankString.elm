module NonBlankString exposing (NonBlankString, nonBlankString, string)

import String exposing (trim)

type NonBlankString
  = NonBlankString String

nonBlankString : String -> Maybe NonBlankString
nonBlankString rawString =
  case trim rawString of
    "" ->
      Nothing
    str ->
      Just (NonBlankString str)

string : NonBlankString -> String
string (NonBlankString str) =
  str
