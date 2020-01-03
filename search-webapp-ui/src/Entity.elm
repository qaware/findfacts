module Entity exposing (Entity, decoder)

import Json.Decode as D
import Json.Encode as E

type alias Entity =
   { res : String
   }



-- JSON


decoder : D.Decoder Entity
decoder =
   D.map Entity
       (D.field "res" D.string)