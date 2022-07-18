{-# language CPP #-}

module Network.Wai.Middleware.Helpers where

import Control.Lens
import Language.Haskell.TH   (mkName)
#if MIN_VERSION_aeson(2,0,0)
import           Data.Aeson        (Key)
import qualified Data.Aeson.Key    as Key
import qualified Data.Aeson.KeyMap as KeyMap
#else
import qualified Data.HashMap.Strict as HM
#endif
import           Data.Bifunctor             (first)
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import qualified Data.Text                  as T

swaggerFieldRules :: LensRules
swaggerFieldRules = defaultFieldRules & lensField %~ swaggerFieldNamer
    where
    swaggerFieldNamer namer dname fnames fname =
        map fixDefName (namer dname fnames fname)

    fixDefName (MethodName cname mname) = MethodName cname (fixName mname)
    fixDefName (TopName name) = TopName (fixName name)

    fixName = mkName . fixName' . show

    fixName' "in"       = "in_"       -- keyword
    fixName' "type"     = "type_"     -- keyword
    fixName' "default"  = "default_"  -- keyword
    fixName' "minimum"  = "minimum_"  -- Prelude conflict
    fixName' "maximum"  = "maximum_"  -- Prelude conflict
    fixName' "enum"     = "enum_"     -- Control.Lens conflict
    fixName' "head"     = "head_"     -- Prelude conflict
    fixName' "not"      = "not_"      -- Prelude conflict
    fixName' n = n

#if MIN_VERSION_aeson(2,0,0)
deleteKey :: Key -> KeyMap.KeyMap v -> KeyMap.KeyMap v
deleteKey = KeyMap.delete

objectToList :: KeyMap.KeyMap v -> [(Key, v)]
objectToList = KeyMap.toList

objectKeys :: KeyMap.KeyMap v -> [T.Text]
objectKeys = map Key.toText . KeyMap.keys

stringToKey :: String -> Key
stringToKey = Key.fromString

keyToString :: Key -> String
keyToString = Key.toString

keyToText :: Key -> T.Text
keyToText = Key.toText

toInsOrdHashMap :: KeyMap.KeyMap v -> InsOrdHashMap.InsOrdHashMap T.Text v
toInsOrdHashMap = InsOrdHashMap.fromList . fmap (first Key.toText) . KeyMap.toList

fromInsOrdHashMap :: InsOrdHashMap.InsOrdHashMap T.Text v -> KeyMap.KeyMap v
fromInsOrdHashMap = KeyMap.fromList . fmap (first Key.fromText) . InsOrdHashMap.toList

lookupKey :: T.Text -> KeyMap.KeyMap v -> Maybe v
lookupKey = KeyMap.lookup . Key.fromText

hasKey :: T.Text -> KeyMap.KeyMap a -> Bool
hasKey = KeyMap.member . Key.fromText
#else
deleteKey :: T.Text -> HM.HashMap T.Text v -> HM.HashMap T.Text v
deleteKey = HM.delete

objectToList :: HM.HashMap T.Text v -> [(T.Text, v)]
objectToList = HM.toList

objectKeys :: HM.HashMap T.Text v -> [T.Text]
objectKeys = HM.keys

stringToKey :: String -> T.Text
stringToKey = T.pack

keyToString :: T.Text -> String
keyToString = T.unpack

keyToText :: T.Text -> T.Text
keyToText = id

toInsOrdHashMap :: HM.HashMap T.Text v -> InsOrdHashMap.InsOrdHashMap T.Text v
toInsOrdHashMap = InsOrdHashMap.fromHashMap

fromInsOrdHashMap :: InsOrdHashMap.InsOrdHashMap T.Text v -> HM.HashMap T.Text v
fromInsOrdHashMap = InsOrdHashMap.toHashMap

lookupKey :: T.Text -> HM.HashMap T.Text v -> Maybe v
lookupKey = HM.lookup

hasKey :: T.Text -> HM.HashMap T.Text a -> Bool
hasKey = HM.member
#endif
