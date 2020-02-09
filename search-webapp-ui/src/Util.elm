module Util exposing (..)

{-| Prepends a maybe to a list.
-}

import Url exposing (Url)


consMaybe : Maybe a -> List a -> List a
consMaybe x xs =
    case x of
        Nothing ->
            xs

        Just y ->
            y :: xs


consIf : Bool -> a -> List a -> List a
consIf b x xs =
    if b then
        x :: xs

    else
        xs
