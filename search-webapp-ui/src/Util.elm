module Util exposing
    ( ite, toMaybe
    , consIf, appIf
    )

{-| Utility module.


# Common language helpers

@docs ite, toMaybe


# List conditional append helpers

@docs consIf, appIf

-}


{-| Ternary operator. Unfortunately no infix notation since that's not allowed in user code any more.

    ite True 1 2 == 1

    ite False 1 2 == 2

-}
ite : Bool -> a -> a -> a
ite cond a b =
    if cond then
        a

    else
        b


{-| Converts a condition and an element to a Maybe.

    toMaybe True 1 == Just 1

    toMaybe False 1 == Nothing

-}
toMaybe : Bool -> a -> Maybe a
toMaybe cond a =
    if cond then
        Just a

    else
        Nothing


{-| Appends elem if the condition is met, or otherwise leaves it unchanged.

    appIf True 3 [ 1, 2 ] == [ 1, 2, 3 ]

    appIf False 3 [ 1, 2 ] == [ 1, 2 ]

-}
appIf : Bool -> a -> List a -> List a
appIf cond x xs =
    ite cond (xs ++ [ x ]) xs


{-| Constructs a list with the element in front if the condition is met, or otherwise leave the list unchanged.

    consIf True 1 [ 2, 3 ] == [ 1, 2, 3 ]

    consIf False 1 [ 2, 3 ] == [ 2, 3 ]

-}
consIf : Bool -> a -> List a -> List a
consIf cond x xs =
    ite cond (x :: xs) xs
