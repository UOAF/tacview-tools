{-# LANGUAGE StrictData #-}

module Data.Tacview where

import Control.Monad
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Functor.Identity
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List (sortBy)
import Data.Maybe
import Data.Ord
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as T
import Data.Text.Unsafe qualified as TU
import Data.Word
import Data.Vector.Strict (Vector)
import Data.Vector.Strict qualified as V
import GHC.Data.Word64Map.Strict (Word64Map)
import GHC.Data.Word64Set (Word64Set)
import GHC.Data.Word64Set qualified as S
import GHC.Stack
import Numeric

zipExt :: String
zipExt = ".zip.acmi"

txtExt :: String
txtExt = ".txt.acmi"

-- | Objects in TacView are identified by 64-bit hex values.
type TacId = Word64

type TacIdMap a = Word64Map a

type TacIdSet = Word64Set

-- | Pull the (#) off the front of the line and parse the rest as a double.
parseTime :: HasCallStack => Text -> Double
parseTime t = case T.rational (T.tail t) of
    Left e -> error (T.unpack t <> ": " <> e)
    Right (v, _) -> v

-- | Parse a hex value from the start of the given string.
parseId :: HasCallStack => Text -> TacId
parseId t = case T.hexadecimal t of
    Left e -> error (T.unpack t <> ": " <> e)
    Right (v, _) -> v

-- | Parse a hex value from the start of the given string,
--   or return `Nothing` if it's not a hex value.
maybeId :: Text -> Maybe TacId
maybeId t = case T.hexadecimal t of
    Left _ -> Nothing
    -- "File" parses as (0xf, "ile"). Let's not do that
    Right (v, rest) -> if rest == "" || T.isPrefixOf "," rest then Just v else Nothing

-- | For filtering purposes, each line is either:
data ParsedLine =
    TimeLine Double |
    -- | A property line: @id,prop1,prop2@
    --   This introduces objects if we haven't seen them before.
    PropLine TacId Properties |
    -- | A removal line: @-id@
    RemLine TacId |
    -- | An event line: @0,Event=...|id1|id2|...@
    --   This can have multiple IDs!
    EventLine Text TacIdSet Text |
    -- | Global config or something else we don't care to parse
    OtherLine Text
    deriving stock (Show)

showLine :: ParsedLine -> Text
showLine (TimeLine t) = "#" <> (shaveZeroes . T.pack $ showFFloat Nothing t "")
showLine (PropLine i p) = T.pack (showHex i "") <> "," <> showProperties p
showLine (RemLine i) = "-" <> T.pack (showHex i "")
showLine (EventLine t is p) = mconcat [
    "0,Event=",
    t,
    "|",
    T.intercalate "|" (T.pack . flip showHex "" <$> S.toList is),
    "|",
    p
    ]
showLine (OtherLine l) = l

parseLine :: HasCallStack => Text -> ParsedLine
parseLine = parseLine' . fixupNames

parseLine' :: HasCallStack => Text -> ParsedLine
parseLine' l
    | T.isPrefixOf "#" l = TimeLine $ parseTime l
    | T.isPrefixOf "0,Event=" l = runIdentity $ do
        let rest = T.drop (T.length "0,Event=") l
            toks = T.splitOn "|" rest
            eventType = head toks
        when (eventType == "Timeout") $ error "Timeout events are unsupported"
        let tms = (\t -> (t, maybeId t)) <$> tail toks
            (ids, nonIds) = span (isJust . snd) tms
        when (any (isJust . snd) nonIds) $ error $ "Strange event: " <> T.unpack l
        let post = T.intercalate "|" $ fmap fst nonIds
        pure $ EventLine eventType (S.fromList $ mapMaybe snd ids) post
    | T.isPrefixOf "-" l = RemLine $ parseId (T.tail l)
    | T.isPrefixOf "0," l = OtherLine l -- Don't try to parse global config.
    | otherwise = case maybeId l of
        Just i -> PropLine i (lineProperties l)
        Nothing -> OtherLine l

-- | BMS currently doesn't escape names with commas. Fish them out and do that ourselves.
--
-- Transforms "Name=Goofy, One" to "Name=Goofy\, One".
-- Assumes the name doesn't have equals signs in it
-- and that the next property is well-formed (good grief).
fixupNames :: Text -> Text
fixupNames l = let
    (beforeName, nameAndRest) = T.breakOn "Name=" l
    (name, nextAndRest) = breakOnNextProperty nameAndRest
    in if T.null nameAndRest || not (T.elem ',' name)
        then l -- Fast path - don't reconcatenate something we didn't change.
        else beforeName <> T.replace "," "\\," name <> nextAndRest

-- | Split before the first match of @,[a-zA-Z0-9]+=@ (the start of the next property),
-- or return @(t, "")@ if there is no match.
--
-- This was a regex-tdfa match, but that was a fifth of the parse thread's time,
-- and almost every BMS line has a name.
breakOnNextProperty :: Text -> (Text, Text)
breakOnNextProperty t = go t where
    go s = let
        (_, commaAndRest) = T.break (== ',') s
        afterComma = T.drop 1 commaAndRest
        (key, afterKey) = T.span isAsciiAlphaNum afterComma
        in if
            | T.null commaAndRest -> (t, T.empty)
            | not (T.null key) && "=" `T.isPrefixOf` afterKey -> let
                -- A comma is one byte, so we can split on byte offsets.
                at = TU.lengthWord8 t - TU.lengthWord8 commaAndRest
                in (TU.takeWord8 at t, TU.dropWord8 at t)
            | otherwise -> go afterComma
    isAsciiAlphaNum c = isAsciiUpper c || isAsciiLower c || isDigit c

-- | Positions are a special case, where each coordinate can be delta-encoded.
data Property = Property Text | Position (Vector Text) | Referencing TacId
    deriving stock (Show, Eq)

showProperty :: Property -> Text
showProperty (Property t) = t
showProperty (Position v) = T.intercalate "|" $ V.toList v
showProperty (Referencing tid) = T.pack $ showHex tid ""

-- | Trim trailing zeroes from decimals (@1.50@ -> @1.5@, @2.00@ -> @2@, @-0.0@ -> @0@).
--
-- The result is always a slice of the input, so this doesn't copy anything.
shaveZeroes :: Text -> Text
shaveZeroes t = case T.uncons t of
    Nothing -> t
    Just ('-', rest) -> let
        n = shaveZeroes rest
        -- n is a prefix of rest unless it's "0", so "-" <> n is a prefix of t.
        in if n == "0" then n else TU.takeWord8 (TU.lengthWord8 n + 1) t
    Just _ -> case T.uncons (T.dropWhile isDigit t) of
        -- Only digits, then one decimal point, then only digits
        Just ('.', fractional) | T.all isDigit fractional -> let
            trimmed = T.dropWhileEnd (== '0') t
            in if T.last trimmed == '.' then T.init trimmed else trimmed
        _ -> t

-- | Each property is <name>=<value>
--
-- This runs for every property of every line, so it's strict to avoid building thunks.
parseProperty :: HasCallStack => Text -> (Text, Property)
parseProperty pl = case T.break (== '=') pl of
    (k, vWithEq) -> case T.uncons vWithEq of
        Nothing -> error $ "Property without '=': " <> T.unpack pl
        Just (_, v) -> let
            -- We do a little special casing, eh?
            !p = if
                | k == "T" ->  Position . V.fromList $ shaveZeroes <$> T.split (== '|') v
                | k == "FocusedTarget" -> Referencing $ parseId v
                | "LockedTarget" `T.isPrefixOf` k -> Referencing $ parseId v
                | otherwise -> Property $ shaveZeroes v
            in (k, p)

type Properties = HashMap Text Property

-- | The opposite of parse
showProperties :: Properties -> Text
showProperties ps = -- Finagling - get T= first
    T.intercalate "," $ maybeT ++ rest where
        maybeT = case ps HM.!? "T" of
            Just t -> [eqPair ("T", t)]
            Nothing -> []
        rest = fmap eqPair $ sortBy (comparing fst) $ HM.toList (HM.delete "T" ps)
        eqPair (k, v) = k <> "=" <> showProperty v

-- | Properties are comma-separated, with the first one being the `TacId`
-- (which we've already parsed)
lineProperties :: HasCallStack => Text -> Properties
lineProperties t = HM.fromList $ fmap parseProperty (tail $ splitOnUnescapedCommas t)

splitOnUnescapedCommas :: Text -> [Text]
splitOnUnescapedCommas t
    | T.elem '\\' t = joinEscaped $ T.split (== ',') t
    | otherwise = T.split (== ',') t -- Nothing escaped
  where
    -- If the last character of the last token was '\', the comma was escaped.
    -- Merge the two.
    joinEscaped (x : y : xs) = case T.unsnoc x of
        Just (_, '\\') -> (x <> "," <> y) : joinEscaped xs
        _ -> x : joinEscaped (y : xs)
    joinEscaped rest = rest

-- | Update a pervious set of object properties with the new set
-- (Updates are delta-encoded where unmentioned properties retain their previous value.)
updateProperties :: Properties -> Properties -> Properties
updateProperties = HM.unionWith updateProperty

-- | Given the previous version of a property and an update, return its new value.
-- For everything but positions, this is just the new value.
-- For positions, individual coordinates in the position can be delta-encoded.
updateProperty :: Property -> Property -> Property
updateProperty (Property _old) new@(Property _) = new
updateProperty (Referencing _old) new@(Referencing _) = new
updateProperty (Position old) (Position new) = if V.length old == V.length new
    then Position $ V.zipWith maybeCoord old new
    else Position new
updateProperty old new = error $ mconcat
    ["Mixing properties and positions: ", show old, " -> ", show new]

-- | For coordinates: Replace blanks with not blanks.
-- We're trying to accumulate data here.
maybeCoord :: Text -> Text -> Text
maybeCoord old new
    | old == "" = new
    | new == "" = old
    | otherwise = new

-- | Calculate the delta between an old set of object properties and the new set.
-- Used to determine what we should write out.
deltaProperties :: Properties -> Properties -> Properties
deltaProperties old new = HM.differenceWith (flip deltaProperty) new old

deltaProperty :: Property -> Property -> Maybe Property
deltaProperty (Property old) (Property new) = if old == new then Nothing else Just (Property new)
deltaProperty (Referencing old) (Referencing new) = if old == new then Nothing else Just (Referencing new)
deltaProperty (Position old) (Position new) = if V.length old == V.length new
    then let
        go coords -- If none of the coords have changed, skip T=||| crap.
            | V.all (== "") coords = Nothing
            | otherwise = Just (Position coords)
        in go $ V.zipWith deltaCoord old new
    else Just (Position new) -- If the coordinate type change, give us the new one.
deltaProperty old new = error $ mconcat
    ["Mixing properties and positions: ", show old, " -> ", show new]

deltaCoord :: Text -> Text -> Text
deltaCoord old new = if old == new then "" else new

-- | Delta-encode an object, generating a line with only properties that changed.
-- Returns Just the line, or Nothing if there's no changes.
deltaEncode :: Properties -> Properties -> Maybe Properties
deltaEncode old new = let
    deltaProps = deltaProperties old new
    in if HM.null deltaProps
        -- Don't write if the delta-encoded version is empty (nothing changed).
        then Nothing
        else Just deltaProps
