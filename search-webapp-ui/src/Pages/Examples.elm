module Pages.Examples exposing (..)

{-| the static 'examples' page.
-}

import Html exposing (Html, a, h1, h2, p, text)
import Html.Attributes exposing (href)
import Material.Typography as Typography
import Url


{-| Renders the 'example' page
-}
view : List (Html msg)
view =
    [ h1 [ Typography.headline3 ] [ text "Examples" ]
    , p [ Typography.body1 ]
        [ text "These examples document how a search session could play out, given an initial question. Each step is linked to the actual search state. Results may change in future Isabelle/AFP versions (used here: Isabelle2002-RC4/afp-2020-234a91c26d02)."
        ]
    , h2 [ Typography.headline4 ] [ text "What is proven about prime numbers?" ]
    , p [ Typography.body1 ]
        [ text "For this, we first need to look up the definition of a prime. We start by "
        , search "typing 'prime' in the main search bar" "{\"term\":\"prime\"}"
        , text ", and get more than 2000 results."
        , br
        , text "A facet appears that distinguishes kinds of semantic entities, telling us that most of the results are facts. We "
        , search "select 'Constant' from the facet" "{\"term\":\"prime\",\"facets\":{\"Kind\":[\"Constant\"]}}"
        , text " and are down to less than 150 results, still more than we can scan. But we see that the first few results only use primes, not define them."
        , br
        , text "We thus "
        , search "add a filter to search for 'prime' in the entity name as well"
            "{\"term\":\"prime\",\"fields\":[{\"field\":\"Name\",\"match\":\"OneOf\",\"terms\":[\"prime\"]}],\"facets\":{\"Kind\":[\"Constant\"]}}"
        , text ". This leaves just 60 results, and the first one is indeed a definition of prime numbers! However, it's from the theory 'ZF-ex.Primes', but we were looking for a formalization in HOL. Maybe adding a type helps?"
        , br
        , text "We "
        , search "add a 'Constant Type' filter" "{\"term\":\"prime\",\"fields\":[{\"field\":\"Name\",\"match\":\"OneOf\",\"terms\":[\"prime\"]},{\"field\":\"ConstantType\",\"match\":\"OneOf\"}],\"facets\":{\"Kind\":[\"Constant\"]}}"
        , text " and see that we can already open a drop-down list, though a click on it reveals that there are still too many."
        , br
        , text "So we type in "
        , search "nat => bool" "{\"term\":\"prime\",\"fields\":[{\"field\":\"Name\",\"match\":\"OneOf\",\"terms\":[\"prime\"]},{\"field\":\"ConstantType\",\"match\":\"OneOf\",\"terms\":[\"nat => bool\"]}],\"facets\":{\"Kind\":[\"Constant\"]}}"
        , text " and get down to 18 results. Also, a facet on 'Command' shows us that some functions and locales are in the result set as well."
        , br
        , search "Selecting only relevant commands" "{\"term\":\"prime\",\"fields\":[{\"field\":\"Name\",\"match\":\"OneOf\",\"terms\":[\"prime\"]},{\"field\":\"ConstantType\",\"match\":\"OneOf\",\"terms\":[\"nat => bool\"]}],\"facets\":{\"Command\":[\"definition\",\"qualified\"],\"Kind\":[\"Constant\"]}}"
        , text " gives us eight results. Scanning these, we see that five are indeed the definitions we are looking for (implemented slightly different or even exactly the same, in different theories). "
        , br
        , search "Clicking on 'used by'" "{\"usedIn\":{\"block\":\"definition prime :: \\\"nat ⇒ bool\\\" where [rewrite]:\\n  \\\"prime p = (1 &lt; p ∧ (∀m. m dvd p ⟶ m = 1 ∨ m = p))\\\"\\n\\n\",\"ids\":[\"Constant.Primes_Ex.prime\",\"Fact.Primes_Ex.prime_def\"]}}"
        , text " on one of them lets us finally browse lemmas about primes."
        ]
    , h2 [ Typography.headline4 ] [ text "Finding commutativity lemmas" ]
    , p [ Typography.body1 ]
        [ text "We start by "
        , search "typing in 'comm*' in the main search bar" "{\"term\":\"comm*\"}"
        , text ". An overwhelming 14000 blocks are found."
        , br
        , text "To reduce this, "
        , search "we select 'Fact' in the facets" "{\"term\":\"comm*\",\"facets\":{\"Kind\":[\"Fact\"]}}"
        , text " and "
        , search " add a filter for lemmas" "{\"term\":\"comm*\",\"fields\":[{\"field\":\"Command\",\"match\":\"OneOf\",\"terms\":[\"lemma\"]}],\"facets\":{\"Kind\":[\"Fact\"]}}"
        , text ", leaving nearly 8000 results."
        , br
        , text "Next, to filter out all blocks that only use commutativity, "
        , search "we require the entity name to be 'comm' or 'commutativity'" "{\"term\":\"comm*\",\"fields\":[{\"field\":\"Command\",\"match\":\"OneOf\",\"terms\":[\"lemma\"]},{\"field\":\"Name\",\"match\":\"OneOf\",\"terms\":[\"comm\",\"commute\"]}],\"facets\":{\"Kind\":[\"Fact\"]}}"
        , text " (as we can't use wildcards here). This gives us 1169 results, for which the first few pages all are lemmas that prove commutativity of different operators - to restrict further, we would need more context."
        ]
    ]


search : String -> String -> Html msg
search str url =
    a [ href <| "#search?q=" ++ Url.percentEncode url, Typography.typography ] [ text str ]


br : Html msg
br =
    Html.br [] []
