module Data.Tacview.Rewrite where

import Data.Tacview
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Trie (Trie)
import Data.Trie qualified as Trie
import Data.HashMap.Strict qualified as HM

rewrite :: ParsedLine -> ParsedLine
rewrite (PropLine i p) = PropLine i $ rewriteProperties p
rewrite ol = ol

rewriteProperties :: Properties -> Properties
rewriteProperties p = case (p HM.!? "Name", p HM.!? "Type") of
    (Just (Property n), Just (Property t)) -> byNameType n t p
    _ -> p

byNameType :: Text -> Text -> (Properties -> Properties)
byNameType n t
    | "Ground" `T.isInfixOf` t && "Vehicle" `T.isInfixOf` t = groundBranch n
    | otherwise = id -- Others?

groundBranch :: Text -> Properties -> Properties
groundBranch n = case Trie.match nameTrie (T.encodeUtf8 n) of
    Just (_, f, _) -> f
    Nothing -> id


nameTrie :: Trie (Properties -> Properties)
nameTrie = Trie.fromList [
    ("AN/MPQ-", aaa),
    ("AN/MSQ-", aaa),
    ("KSAM", rename "K-SAM Pegasus" . aaa),
    ("K-SAM", rename "K-SAM Pegasus" . aaa),
    ("MIM23", rename "MIM-23 Hawk" . aaa),
    ("MIM-23", rename "MIM-23 Hawk" . aaa),
    ("MIM104", rename "MIM-104 Patriot" . aaa),
    ("MIM-104", rename "MIM-104 Patriot" . aaa),
    ("SA-2", rename "SA-2 Guideline" . aaa),
    ("SA-3", rename "SA-3 Goa" . aaa),
    ("SA-4", rename "SA-4 Ganef" . aaa),
    ("SA-5", rename "SA-5 Gammon" . aaa),
    ("SA-6", rename "SA-6 Gainful" . aaa),
    ("SA-7", rename "SA-7 Grail" . manpad),
    ("SA-8", rename "SA-8 Gecko" . aaa),
    ("SA-9", rename "SA-9 Gaskin" . aaa),
    ("SA-10", rename "SA-10 Grumble" . aaa),
    ("SA-11", rename "SA-11 Gadfly" . aaa),
    ("SA-15", rename "SA-15 Gauntlet" . aaa),
    ("SA-16", rename "SA-16 Gimlet" . manpad),
    ("SA-17", rename "SA-15 Grizzly" . aaa),
    ("SA-18", rename "SA-18 Grouse" . manpad),
    ("SA-19", rename "SA-19 Grison" . aaa),
    ("SA-20", rename "SA-20 Gargoyle" . aaa),
    ("Stinger", manpad),
    ("ZSU-23-4", rename "ZSU-23-4 Shilka" . aaa),
    ("ZPU", aaa)
    ]

rename :: Text -> Properties -> Properties
rename n = HM.insert "Name" (Property n)

retype :: Text -> Properties -> Properties
retype t = HM.insert "Type" (Property t)

aaa :: Properties -> Properties
aaa = retype "Ground+AntiAircraft"

manpad :: Properties -> Properties
manpad = retype "Ground+Human+AntiAircraft"
