port module Ports exposing (..)

import Scroll exposing (Move)
import StickyHeader

-- type alias TPort = (Move -> msg) -> Sub msg

port scroll : (Move -> StickyHeader.Msg) -> Sub StickyHeader.Msg -- StickyHeader.Port 