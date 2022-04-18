port module Ports exposing (LocalStorageRecord, setLocalStorageItem)


type alias LocalStorageRecord =
    { key : String, value : String }


port setLocalStorageItem : LocalStorageRecord -> Cmd msg
