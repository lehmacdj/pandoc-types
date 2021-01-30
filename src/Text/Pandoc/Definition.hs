{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveGeneric,
    FlexibleContexts, GeneralizedNewtypeDeriving, PatternGuards, CPP,
    TemplateHaskell #-}

{-
Copyright (c) 2006-2019, John MacFarlane

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of John MacFarlane nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

{- |
   Module      : Text.Pandoc.Definition
   Copyright   : Copyright (C) 2006-2019 John MacFarlane
   License     : BSD3

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Definition of 'Pandoc' data structure for format-neutral representation
of documents.
-}
module Text.Pandoc.Definition ( Pandoc(..)
                              , Meta(..)
                              , MetaValue(..)
                              , nullMeta
                              , isNullMeta
                              , lookupMeta
                              , docTitle
                              , docAuthors
                              , docDate
                              , Block(..)
                              , Inline(..)
                              , ListAttributes
                              , ListNumberStyle(..)
                              , ListNumberDelim(..)
                              , Format(..)
                              , Attr
                              , nullAttr
                              , Caption(..)
                              , ShortCaption
                              , RowHeadColumns(..)
                              , Alignment(..)
                              , ColWidth(..)
                              , ColSpec
                              , Row(..)
                              , TableHead(..)
                              , TableBody(..)
                              , TableFoot(..)
                              , Cell(..)
                              , RowSpan(..)
                              , ColSpan(..)
                              , QuoteType(..)
                              , Target
                              , MathType(..)
                              , Citation(..)
                              , CitationMode(..)
                              , pandocTypesVersion
                              ) where

import Data.Generics (Data, Typeable)
import Data.Ord (comparing)
import Data.Aeson hiding (Null)
import Data.Aeson.TH (deriveJSON)
import qualified Data.Aeson.Encoding.Internal
import qualified Data.Aeson.Types as Aeson
import qualified Data.Aeson.Types
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.String
import Control.DeepSeq
import Paths_pandoc_types (version)
import qualified Data.Vector
import qualified Data.Vector.Mutable
import Data.Version (Version, versionBranch)
import Data.Semigroup (Semigroup(..))

data Pandoc = Pandoc Meta [Block]
              deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

instance Semigroup Pandoc where
  (Pandoc m1 bs1) <> (Pandoc m2 bs2) =
    Pandoc (m1 <> m2) (bs1 <> bs2)
instance Monoid Pandoc where
  mempty = Pandoc mempty mempty
  mappend = (<>)

-- | Metadata for the document:  title, authors, date.
newtype Meta = Meta { unMeta :: M.Map Text MetaValue }
               deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

instance Semigroup Meta where
  (Meta m1) <> (Meta m2) = Meta (M.union m2 m1)
  -- note: M.union is left-biased, so if there are fields in both m2
  -- and m1, m2 wins.
instance Monoid Meta where
  mempty = Meta M.empty
  mappend = (<>)

data MetaValue = MetaMap (M.Map Text MetaValue)
               | MetaList [MetaValue]
               | MetaBool Bool
               | MetaString Text
               | MetaInlines [Inline]
               | MetaBlocks [Block]
               deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

nullMeta :: Meta
nullMeta = Meta M.empty

isNullMeta :: Meta -> Bool
isNullMeta (Meta m) = M.null m

-- Helper functions to extract metadata

-- | Retrieve the metadata value for a given @key@.
lookupMeta :: Text -> Meta -> Maybe MetaValue
lookupMeta key (Meta m) = M.lookup key m

-- | Extract document title from metadata; works just like the old @docTitle@.
docTitle :: Meta -> [Inline]
docTitle meta =
  case lookupMeta "title" meta of
         Just (MetaString s)           -> [Str s]
         Just (MetaInlines ils)        -> ils
         Just (MetaBlocks [Plain ils]) -> ils
         Just (MetaBlocks [Para ils])  -> ils
         _                             -> []

-- | Extract document authors from metadata; works just like the old
-- @docAuthors@.
docAuthors :: Meta -> [[Inline]]
docAuthors meta =
  case lookupMeta "author" meta of
        Just (MetaString s)    -> [[Str s]]
        Just (MetaInlines ils) -> [ils]
        Just (MetaList   ms)   -> [ils | MetaInlines ils <- ms] ++
                                  [ils | MetaBlocks [Plain ils] <- ms] ++
                                  [ils | MetaBlocks [Para ils]  <- ms] ++
                                  [[Str x] | MetaString x <- ms]
        _                      -> []

-- | Extract date from metadata; works just like the old @docDate@.
docDate :: Meta -> [Inline]
docDate meta =
  case lookupMeta "date" meta of
         Just (MetaString s)           -> [Str s]
         Just (MetaInlines ils)        -> ils
         Just (MetaBlocks [Plain ils]) -> ils
         Just (MetaBlocks [Para ils])  -> ils
         _                             -> []

-- | List attributes.  The first element of the triple is the
-- start number of the list.
type ListAttributes = (Int, ListNumberStyle, ListNumberDelim)

-- | Style of list numbers.
data ListNumberStyle = DefaultStyle
                     | Example
                     | Decimal
                     | LowerRoman
                     | UpperRoman
                     | LowerAlpha
                     | UpperAlpha deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | Delimiter of list numbers.
data ListNumberDelim = DefaultDelim
                     | Period
                     | OneParen
                     | TwoParens deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | Attributes: identifier, classes, key-value pairs
type Attr = (Text, [Text], [(Text, Text)])

nullAttr :: Attr
nullAttr = ("",[],[])

-- | Formats for raw blocks
newtype Format = Format Text
               deriving (Read, Show, Typeable, Data, Generic, ToJSON, FromJSON)

instance IsString Format where
  fromString f = Format $ T.toCaseFold $ T.pack f

instance Eq Format where
  Format x == Format y = T.toCaseFold x == T.toCaseFold y

instance Ord Format where
  compare (Format x) (Format y) = compare (T.toCaseFold x) (T.toCaseFold y)

-- | The number of columns taken up by the row head of each row of a
-- 'TableBody'. The row body takes up the remaining columns.
newtype RowHeadColumns = RowHeadColumns Int
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, Num, Enum, ToJSON, FromJSON)

-- | Alignment of a table column.
data Alignment = AlignLeft
               | AlignRight
               | AlignCenter
               | AlignDefault deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | The width of a table column, as a fraction of the total table
-- width.
data ColWidth = ColWidth Double
              | ColWidthDefault deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | The specification for a single table column.
type ColSpec = (Alignment, ColWidth)

-- | A table row.
data Row = Row Attr [Cell]
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | The head of a table.
data TableHead = TableHead Attr [Row]
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | A body of a table, with an intermediate head, intermediate body,
-- and the specified number of row header columns in the intermediate
-- body.
data TableBody = TableBody Attr RowHeadColumns [Row] [Row]
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | The foot of a table.
data TableFoot = TableFoot Attr [Row]
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | A short caption, for use in, for instance, lists of figures.
type ShortCaption = [Inline]

-- | The caption of a table, with an optional short caption.
data Caption = Caption (Maybe ShortCaption) [Block]
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | A table cell.
data Cell = Cell Attr Alignment RowSpan ColSpan [Block]
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | The number of rows occupied by a cell; the height of a cell.
newtype RowSpan = RowSpan Int
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, Num, Enum, ToJSON, FromJSON)

-- | The number of columns occupied by a cell; the width of a cell.
newtype ColSpan = ColSpan Int
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, Num, Enum, ToJSON, FromJSON)

-- | Block element.
data Block
    -- | Plain text, not a paragraph
    = Plain [Inline]
    -- | Paragraph
    | Para [Inline]
    -- | Multiple non-breaking lines
    | LineBlock [[Inline]]
    -- | Code block (literal) with attributes
    | CodeBlock Attr Text
    -- | Raw block
    | RawBlock Format Text
    -- | Block quote (list of blocks)
    | BlockQuote [Block]
    -- | Ordered list (attributes and a list of items, each a list of
    -- blocks)
    | OrderedList ListAttributes [[Block]]
    -- | Bullet list (list of items, each a list of blocks)
    | BulletList [[Block]]
    -- | Definition list. Each list item is a pair consisting of a
    -- term (a list of inlines) and one or more definitions (each a
    -- list of blocks)
    | DefinitionList [([Inline],[[Block]])]
    -- | Header - level (integer) and text (inlines)
    | Header Int Attr [Inline]
    -- | Horizontal rule
    | HorizontalRule
    -- | Table, with attributes, caption, optional short caption,
    -- column alignments and widths (required), table head, table
    -- bodies, and table foot
    | Table Attr Caption [ColSpec] TableHead [TableBody] TableFoot
    -- | Generic block container with attributes
    | Div Attr [Block]
    -- | Nothing
    | Null
    deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | Type of quotation marks to use in Quoted inline.
data QuoteType = SingleQuote | DoubleQuote deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)

-- | Link target (URL, title).
type Target = (Text, Text)

-- | Type of math element (display or inline).
data MathType = DisplayMath | InlineMath deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)

-- | Inline elements.
data Inline
    = Str Text            -- ^ Text (string)
    | Emph [Inline]         -- ^ Emphasized text (list of inlines)
    | Underline [Inline]    -- ^  Underlined text (list of inlines)
    | Strong [Inline]       -- ^ Strongly emphasized text (list of inlines)
    | Strikeout [Inline]    -- ^ Strikeout text (list of inlines)
    | Superscript [Inline]  -- ^ Superscripted text (list of inlines)
    | Subscript [Inline]    -- ^ Subscripted text (list of inlines)
    | SmallCaps [Inline]    -- ^ Small caps text (list of inlines)
    | Quoted QuoteType [Inline] -- ^ Quoted text (list of inlines)
    | Cite [Citation]  [Inline] -- ^ Citation (list of inlines)
    | Code Attr Text      -- ^ Inline code (literal)
    | Space                 -- ^ Inter-word space
    | SoftBreak             -- ^ Soft line break
    | LineBreak             -- ^ Hard line break
    | Math MathType Text  -- ^ TeX math (literal)
    | RawInline Format Text -- ^ Raw inline
    | Link Attr [Inline] Target  -- ^ Hyperlink: alt text (list of inlines), target
    | Image Attr [Inline] Target -- ^ Image:  alt text (list of inlines), target
    | Note [Block]          -- ^ Footnote or endnote
    | Span Attr [Inline]    -- ^ Generic inline container with attributes
    deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)

data Citation = Citation { citationId      :: Text
                         , citationPrefix  :: [Inline]
                         , citationSuffix  :: [Inline]
                         , citationMode    :: CitationMode
                         , citationNoteNum :: Int
                         , citationHash    :: Int
                         }
                deriving (Show, Eq, Read, Typeable, Data, Generic)

instance Ord Citation where
    compare = comparing citationHash

data CitationMode = AuthorInText | SuppressAuthor | NormalCitation
                    deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)


-- Private functions from Data.Aeson.TH that we need to splice the code by hand

-- | The name of the outermost 'Value' constructor.
valueConName :: Value -> String
valueConName (Object _) = "Object"
valueConName (Array  _) = "Array"
valueConName (String _) = "String"
valueConName (Number _) = "Number"
valueConName (Bool   _) = "Boolean"
valueConName Null       = "Null"

parseTypeMismatch' :: String -> String -> String -> String -> Parser fail
parseTypeMismatch' conName tName expected actual =
      fail $ printf "When parsing the constructor %s of type %s expected %s but got %s."
                        conName tName expected actual

conNotFoundFailTaggedObject :: String -> [String] -> String -> Parser fail
conNotFoundFailTaggedObject t cs o =
      fail $ printf "When parsing %s expected an Object with a tag field where the value is one of [%s], but got %s."
                        t (intercalate ", " cs) o

-- ToJSON/FromJSON instances. Some are defined by hand so that we have
-- more control over the format.

instance ToJSON MetaValue where
  toJSON
    = \ value_atzE
        -> case value_atzE of
             MetaMap arg1_atAZ
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "MetaMap"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (toJSON arg1_atAZ))
             MetaList arg1_atB4
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "MetaList"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (toJSON arg1_atB4))
             MetaBool arg1_atB5
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "MetaBool"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (toJSON arg1_atB5))
             MetaString arg1_atB6
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "MetaString"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (toJSON arg1_atB6))
             MetaInlines arg1_atB7
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "MetaInlines"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (toJSON arg1_atB7))
             MetaBlocks arg1_atB8
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "MetaBlocks"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (toJSON arg1_atB8))
  toEncoding
    = \ value_atB9
        -> case value_atB9 of
             MetaMap arg1_atBe
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "MetaMap"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (toEncoding arg1_atBe))
             MetaList arg1_atBj
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "MetaList"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (toEncoding arg1_atBj))
             MetaBool arg1_atBk
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "MetaBool"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (toEncoding arg1_atBk))
             MetaString arg1_atBl
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "MetaString"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (toEncoding arg1_atBl))
             MetaInlines arg1_atBm
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "MetaInlines"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (toEncoding arg1_atBm))
             MetaBlocks arg1_atBn
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "MetaBlocks"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (toEncoding arg1_atBn))
instance FromJSON MetaValue where
  parseJSON
    = \ value_atBo
        -> case value_atBo of
             Object obj_atBp
               -> do conKey_atBq <- (obj_atBp .: T.pack "t")
                     case conKey_atBq of {
                       _ | (conKey_atBq == T.pack "MetaMap")
                         -> do val_atBv <- (obj_atBp .: T.pack "c")
                               case val_atBv of { arg_atBw -> (MetaMap <$> parseJSON arg_atBw) }
                         | (conKey_atBq == T.pack "MetaList")
                         -> do val_atBB <- (obj_atBp .: T.pack "c")
                               case val_atBB of {
                                 arg_atBC -> (MetaList <$> parseJSON arg_atBC) }
                         | (conKey_atBq == T.pack "MetaBool")
                         -> do val_atBD <- (obj_atBp .: T.pack "c")
                               case val_atBD of {
                                 arg_atBE -> (MetaBool <$> parseJSON arg_atBE) }
                         | (conKey_atBq == T.pack "MetaString")
                         -> do val_atBF <- (obj_atBp .: T.pack "c")
                               case val_atBF of {
                                 arg_atBG -> (MetaString <$> parseJSON arg_atBG) }
                         | (conKey_atBq == T.pack "MetaInlines")
                         -> do val_atBH <- (obj_atBp .: T.pack "c")
                               case val_atBH of {
                                 arg_atBI -> (MetaInlines <$> parseJSON arg_atBI) }
                         | (conKey_atBq == T.pack "MetaBlocks")
                         -> do val_atBJ <- (obj_atBp .: T.pack "c")
                               case val_atBJ of {
                                 arg_atBK -> (MetaBlocks <$> parseJSON arg_atBK) }
                         | otherwise
                         -> ((conNotFoundFailTaggedObject
                                "Text.Pandoc.Definition.MetaValue")
                               ["MetaMap", "MetaList", "MetaBool", "MetaString", "MetaInlines",
                                "MetaBlocks"])
                              (T.unpack conKey_atBq) }
             other_atBL
               -> (Data.Aeson.TH.noObjectFail "Text.Pandoc.Definition.MetaValue")
                    (valueConName other_atBL)
instance ToJSON CitationMode where
  toJSON
    = \ value_atBM
        -> case value_atBM of
             AuthorInText
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "AuthorInText")))
             SuppressAuthor
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "SuppressAuthor")))
             NormalCitation
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "NormalCitation")))
  toEncoding
    = \ value_atBN
        -> case value_atBN of
             AuthorInText
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "AuthorInText")))
             SuppressAuthor
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "SuppressAuthor")))
             NormalCitation
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "NormalCitation")))
instance FromJSON CitationMode where
  parseJSON
    = \ value_atBO
        -> case value_atBO of
             Object obj_atBP
               -> do conKey_atBQ <- (obj_atBP .: T.pack "t")
                     case conKey_atBQ of {
                       _ | (conKey_atBQ == T.pack "AuthorInText") -> pure AuthorInText
                         | (conKey_atBQ == T.pack "SuppressAuthor") -> pure SuppressAuthor
                         | (conKey_atBQ == T.pack "NormalCitation") -> pure NormalCitation
                         | otherwise
                         -> ((conNotFoundFailTaggedObject
                                "Text.Pandoc.Definition.CitationMode")
                               ["AuthorInText", "SuppressAuthor", "NormalCitation"])
                              (T.unpack conKey_atBQ) }
             other_atBR
               -> (Data.Aeson.TH.noObjectFail
                     "Text.Pandoc.Definition.CitationMode")
                    (valueConName other_atBR)
instance ToJSON Citation where
  toJSON
    = \ value_atBS
        -> case value_atBS of {
             Citation arg1_atBT
                      arg2_atBU
                      arg3_atBV
                      arg4_atBW
                      arg5_atBX
                      arg6_atBY
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "citationId")
                       (toJSON arg1_atBT)
                       <>
                         ((Data.Aeson.Encoding.Internal.pair "citationPrefix")
                            (toJSON arg2_atBU)
                            <>
                              ((Data.Aeson.Encoding.Internal.pair "citationSuffix")
                                 (toJSON arg3_atBV)
                                 <>
                                   ((Data.Aeson.Encoding.Internal.pair "citationMode")
                                      (toJSON arg4_atBW)
                                      <>
                                        ((Data.Aeson.Encoding.Internal.pair
                                            "citationNoteNum")
                                           (toJSON arg5_atBX)
                                           <>
                                             (Data.Aeson.Encoding.Internal.pair
                                                "citationHash")
                                               (toJSON arg6_atBY)))))) }
  toEncoding
    = \ value_atBZ
        -> case value_atBZ of {
             Citation arg1_atC0
                      arg2_atC1
                      arg3_atC2
                      arg4_atC3
                      arg5_atC4
                      arg6_atC5
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "citationId")
                       (toEncoding arg1_atC0)
                       <>
                         ((Data.Aeson.Encoding.Internal.pair "citationPrefix")
                            (toEncoding arg2_atC1)
                            <>
                              ((Data.Aeson.Encoding.Internal.pair "citationSuffix")
                                 (toEncoding arg3_atC2)
                                 <>
                                   ((Data.Aeson.Encoding.Internal.pair "citationMode")
                                      (toEncoding arg4_atC3)
                                      <>
                                        ((Data.Aeson.Encoding.Internal.pair
                                            "citationNoteNum")
                                           (toEncoding arg5_atC4)
                                           <>
                                             (Data.Aeson.Encoding.Internal.pair
                                                "citationHash")
                                               (toEncoding arg6_atC5)))))) }
instance FromJSON Citation where
  parseJSON
    = \ value_atC6
        -> case value_atC6 of
             Object recObj_atC7
               -> ((((((Citation
                          <$>
                            ((((Data.Aeson.TH.lookupField parseJSON)
                                 "Text.Pandoc.Definition.Citation")
                                "Citation")
                               recObj_atC7)
                              (T.pack "citationId"))
                         <*>
                           ((((Data.Aeson.TH.lookupField parseJSON)
                                "Text.Pandoc.Definition.Citation")
                               "Citation")
                              recObj_atC7)
                             (T.pack "citationPrefix"))
                        <*>
                          ((((Data.Aeson.TH.lookupField parseJSON)
                               "Text.Pandoc.Definition.Citation")
                              "Citation")
                             recObj_atC7)
                            (T.pack "citationSuffix"))
                       <*>
                         ((((Data.Aeson.TH.lookupField parseJSON)
                              "Text.Pandoc.Definition.Citation")
                             "Citation")
                            recObj_atC7)
                           (T.pack "citationMode"))
                      <*>
                        ((((Data.Aeson.TH.lookupField parseJSON)
                             "Text.Pandoc.Definition.Citation")
                            "Citation")
                           recObj_atC7)
                          (T.pack "citationNoteNum"))
                     <*>
                       ((((Data.Aeson.TH.lookupField parseJSON)
                            "Text.Pandoc.Definition.Citation")
                           "Citation")
                          recObj_atC7)
                         (T.pack "citationHash"))
             other_atC8
               -> (((parseTypeMismatch' "Citation")
                      "Text.Pandoc.Definition.Citation")
                     "Object")
                    (valueConName other_atC8)
instance ToJSON QuoteType where
  toJSON
    = \ value_atC9
        -> case value_atC9 of
             SingleQuote
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "SingleQuote")))
             DoubleQuote
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "DoubleQuote")))
  toEncoding
    = \ value_atCa
        -> case value_atCa of
             SingleQuote
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "SingleQuote")))
             DoubleQuote
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "DoubleQuote")))
instance FromJSON QuoteType where
  parseJSON
    = \ value_atCb
        -> case value_atCb of
             Object obj_atCc
               -> do conKey_atCd <- (obj_atCc .: T.pack "t")
                     case conKey_atCd of {
                       _ | (conKey_atCd == T.pack "SingleQuote") -> pure SingleQuote
                         | (conKey_atCd == T.pack "DoubleQuote") -> pure DoubleQuote
                         | otherwise
                         -> ((conNotFoundFailTaggedObject
                                "Text.Pandoc.Definition.QuoteType")
                               ["SingleQuote", "DoubleQuote"])
                              (T.unpack conKey_atCd) }
             other_atCe
               -> (Data.Aeson.TH.noObjectFail "Text.Pandoc.Definition.QuoteType")
                    (valueConName other_atCe)
instance ToJSON MathType where
  toJSON
    = \ value_atCf
        -> case value_atCf of
             DisplayMath
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "DisplayMath")))
             InlineMath
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "InlineMath")))
  toEncoding
    = \ value_atCg
        -> case value_atCg of
             DisplayMath
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "DisplayMath")))
             InlineMath
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "InlineMath")))
instance FromJSON MathType where
  parseJSON
    = \ value_atCh
        -> case value_atCh of
             Object obj_atCi
               -> do conKey_atCj <- (obj_atCi .: T.pack "t")
                     case conKey_atCj of {
                       _ | (conKey_atCj == T.pack "DisplayMath") -> pure DisplayMath
                         | (conKey_atCj == T.pack "InlineMath") -> pure InlineMath
                         | otherwise
                         -> ((conNotFoundFailTaggedObject
                                "Text.Pandoc.Definition.MathType")
                               ["DisplayMath", "InlineMath"])
                              (T.unpack conKey_atCj) }
             other_atCk
               -> (Data.Aeson.TH.noObjectFail "Text.Pandoc.Definition.MathType")
                    (valueConName other_atCk)
instance ToJSON ListNumberStyle where
  toJSON
    = \ value_atCl
        -> case value_atCl of
             DefaultStyle
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "DefaultStyle")))
             Example
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "Example")))
             Decimal
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "Decimal")))
             LowerRoman
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "LowerRoman")))
             UpperRoman
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "UpperRoman")))
             LowerAlpha
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "LowerAlpha")))
             UpperAlpha
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "UpperAlpha")))
  toEncoding
    = \ value_atCm
        -> case value_atCm of
             DefaultStyle
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "DefaultStyle")))
             Example
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "Example")))
             Decimal
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "Decimal")))
             LowerRoman
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "LowerRoman")))
             UpperRoman
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "UpperRoman")))
             LowerAlpha
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "LowerAlpha")))
             UpperAlpha
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "UpperAlpha")))
instance FromJSON ListNumberStyle where
  parseJSON
    = \ value_atCn
        -> case value_atCn of
             Object obj_atCo
               -> do conKey_atCp <- (obj_atCo .: T.pack "t")
                     case conKey_atCp of {
                       _ | (conKey_atCp == T.pack "DefaultStyle") -> pure DefaultStyle
                         | (conKey_atCp == T.pack "Example") -> pure Example
                         | (conKey_atCp == T.pack "Decimal") -> pure Decimal
                         | (conKey_atCp == T.pack "LowerRoman") -> pure LowerRoman
                         | (conKey_atCp == T.pack "UpperRoman") -> pure UpperRoman
                         | (conKey_atCp == T.pack "LowerAlpha") -> pure LowerAlpha
                         | (conKey_atCp == T.pack "UpperAlpha") -> pure UpperAlpha
                         | otherwise
                         -> ((conNotFoundFailTaggedObject
                                "Text.Pandoc.Definition.ListNumberStyle")
                               ["DefaultStyle", "Example", "Decimal", "LowerRoman",
                                "UpperRoman", "LowerAlpha", "UpperAlpha"])
                              (T.unpack conKey_atCp) }
             other_atCq
               -> (Data.Aeson.TH.noObjectFail
                     "Text.Pandoc.Definition.ListNumberStyle")
                    (valueConName other_atCq)
instance ToJSON ListNumberDelim where
  toJSON
    = \ value_atCr
        -> case value_atCr of
             DefaultDelim
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "DefaultDelim")))
             Period
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "Period")))
             OneParen
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "OneParen")))
             TwoParens
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "TwoParens")))
  toEncoding
    = \ value_atCs
        -> case value_atCs of
             DefaultDelim
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "DefaultDelim")))
             Period
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "Period")))
             OneParen
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "OneParen")))
             TwoParens
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "TwoParens")))
instance FromJSON ListNumberDelim where
  parseJSON
    = \ value_atCt
        -> case value_atCt of
             Object obj_atCu
               -> do conKey_atCv <- (obj_atCu .: T.pack "t")
                     case conKey_atCv of {
                       _ | (conKey_atCv == T.pack "DefaultDelim") -> pure DefaultDelim
                         | (conKey_atCv == T.pack "Period") -> pure Period
                         | (conKey_atCv == T.pack "OneParen") -> pure OneParen
                         | (conKey_atCv == T.pack "TwoParens") -> pure TwoParens
                         | otherwise
                         -> ((conNotFoundFailTaggedObject
                                "Text.Pandoc.Definition.ListNumberDelim")
                               ["DefaultDelim", "Period", "OneParen", "TwoParens"])
                              (T.unpack conKey_atCv) }
             other_atCw
               -> (Data.Aeson.TH.noObjectFail
                     "Text.Pandoc.Definition.ListNumberDelim")
                    (valueConName other_atCw)
instance ToJSON Alignment where
  toJSON
    = \ value_atCx
        -> case value_atCx of
             AlignLeft
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "AlignLeft")))
             AlignRight
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "AlignRight")))
             AlignCenter
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "AlignCenter")))
             AlignDefault
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "AlignDefault")))
  toEncoding
    = \ value_atCy
        -> case value_atCy of
             AlignLeft
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "AlignLeft")))
             AlignRight
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "AlignRight")))
             AlignCenter
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "AlignCenter")))
             AlignDefault
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "AlignDefault")))
instance FromJSON Alignment where
  parseJSON
    = \ value_atCz
        -> case value_atCz of
             Object obj_atCA
               -> do conKey_atCB <- (obj_atCA .: T.pack "t")
                     case conKey_atCB of {
                       _ | (conKey_atCB == T.pack "AlignLeft") -> pure AlignLeft
                         | (conKey_atCB == T.pack "AlignRight") -> pure AlignRight
                         | (conKey_atCB == T.pack "AlignCenter") -> pure AlignCenter
                         | (conKey_atCB == T.pack "AlignDefault") -> pure AlignDefault
                         | otherwise
                         -> ((conNotFoundFailTaggedObject
                                "Text.Pandoc.Definition.Alignment")
                               ["AlignLeft", "AlignRight", "AlignCenter", "AlignDefault"])
                              (T.unpack conKey_atCB) }
             other_atCC
               -> (Data.Aeson.TH.noObjectFail "Text.Pandoc.Definition.Alignment")
                    (valueConName other_atCC)
instance ToJSON ColWidth where
  toJSON
    = \ value_atCD
        -> case value_atCD of
             ColWidth arg1_atCE
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "ColWidth"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (toJSON arg1_atCE))
             ColWidthDefault
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "ColWidthDefault")))
  toEncoding
    = \ value_atCF
        -> case value_atCF of
             ColWidth arg1_atCG
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "ColWidth"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (toEncoding arg1_atCG))
             ColWidthDefault
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "ColWidthDefault")))
instance FromJSON ColWidth where
  parseJSON
    = \ value_atCH
        -> case value_atCH of
             Object obj_atCI
               -> do conKey_atCJ <- (obj_atCI .: T.pack "t")
                     case conKey_atCJ of {
                       _ | (conKey_atCJ == T.pack "ColWidth")
                         -> do val_atCK <- (obj_atCI .: T.pack "c")
                               case val_atCK of {
                                 arg_atCL -> (ColWidth <$> parseJSON arg_atCL) }
                         | (conKey_atCJ == T.pack "ColWidthDefault") -> pure ColWidthDefault
                         | otherwise
                         -> ((conNotFoundFailTaggedObject
                                "Text.Pandoc.Definition.ColWidth")
                               ["ColWidth", "ColWidthDefault"])
                              (T.unpack conKey_atCJ) }
             other_atCM
               -> (Data.Aeson.TH.noObjectFail "Text.Pandoc.Definition.ColWidth")
                    (valueConName other_atCM)
instance ToJSON Row where
  toJSON
    = \ value_atCN
        -> case value_atCN of {
             Row arg1_atCO arg2_atCP
               -> Array
                    (Data.Vector.create
                       (do mv_atCQ <- Data.Vector.Mutable.unsafeNew 2
                           ((Data.Vector.Mutable.unsafeWrite mv_atCQ) 0)
                             (toJSON arg1_atCO)
                           ((Data.Vector.Mutable.unsafeWrite mv_atCQ) 1)
                             (toJSON arg2_atCP)
                           return mv_atCQ)) }
  toEncoding
    = \ value_atCR
        -> case value_atCR of {
             Row arg1_atCS arg2_atCT
               -> Data.Aeson.Encoding.Internal.wrapArray
                    (toEncoding arg1_atCS
                       Data.Aeson.Encoding.Internal.><
                         (Data.Aeson.Encoding.Internal.comma
                            Data.Aeson.Encoding.Internal.>< toEncoding arg2_atCT)) }
instance FromJSON Row where
  parseJSON
    = \ value_atCU
        -> case value_atCU of
             Array arr_atCV
               -> if (Data.Vector.length arr_atCV == 2) then
                      ((Row
                          <$>
                            parseJSON (arr_atCV `Data.Vector.unsafeIndex` 0))
                         <*>
                           parseJSON (arr_atCV `Data.Vector.unsafeIndex` 1))
                  else
                      (((parseTypeMismatch' "Row")
                          "Text.Pandoc.Definition.Row")
                         "Array of length 2")
                        ("Array of length "
                           ++ (show . Data.Vector.length) arr_atCV)
             other_atCW
               -> (((parseTypeMismatch' "Row")
                      "Text.Pandoc.Definition.Row")
                     "Array")
                    (valueConName other_atCW)
instance ToJSON Caption where
  toJSON
    = \ value_atCX
        -> case value_atCX of {
             Caption arg1_atD0 arg2_atD1
               -> Array
                    (Data.Vector.create
                       (do mv_atD2 <- Data.Vector.Mutable.unsafeNew 2
                           ((Data.Vector.Mutable.unsafeWrite mv_atD2) 0)
                             (toJSON arg1_atD0)
                           ((Data.Vector.Mutable.unsafeWrite mv_atD2) 1)
                             (toJSON arg2_atD1)
                           return mv_atD2)) }
  toEncoding
    = \ value_atD5
        -> case value_atD5 of {
             Caption arg1_atD8 arg2_atD9
               -> Data.Aeson.Encoding.Internal.wrapArray
                    (toEncoding arg1_atD8
                       Data.Aeson.Encoding.Internal.><
                         (Data.Aeson.Encoding.Internal.comma
                            Data.Aeson.Encoding.Internal.>< toEncoding arg2_atD9)) }
instance FromJSON Caption where
  parseJSON
    = \ value_atDc
        -> case value_atDc of
             Array arr_atDf
               -> if (Data.Vector.length arr_atDf == 2) then
                      ((Caption
                          <$>
                            parseJSON (arr_atDf `Data.Vector.unsafeIndex` 0))
                         <*>
                           parseJSON (arr_atDf `Data.Vector.unsafeIndex` 1))
                  else
                      (((parseTypeMismatch' "Caption")
                          "Text.Pandoc.Definition.Caption")
                         "Array of length 2")
                        ("Array of length "
                           ++ (show . Data.Vector.length) arr_atDf)
             other_atDi
               -> (((parseTypeMismatch' "Caption")
                      "Text.Pandoc.Definition.Caption")
                     "Array")
                    (valueConName other_atDi)
instance ToJSON TableHead where
  toJSON
    = \ value_atDj
        -> case value_atDj of {
             TableHead arg1_atDk arg2_atDl
               -> Array
                    (Data.Vector.create
                       (do mv_atDm <- Data.Vector.Mutable.unsafeNew 2
                           ((Data.Vector.Mutable.unsafeWrite mv_atDm) 0)
                             (toJSON arg1_atDk)
                           ((Data.Vector.Mutable.unsafeWrite mv_atDm) 1)
                             (toJSON arg2_atDl)
                           return mv_atDm)) }
  toEncoding
    = \ value_atDn
        -> case value_atDn of {
             TableHead arg1_atDo arg2_atDp
               -> Data.Aeson.Encoding.Internal.wrapArray
                    (toEncoding arg1_atDo
                       Data.Aeson.Encoding.Internal.><
                         (Data.Aeson.Encoding.Internal.comma
                            Data.Aeson.Encoding.Internal.>< toEncoding arg2_atDp)) }
instance FromJSON TableHead where
  parseJSON
    = \ value_atDq
        -> case value_atDq of
             Array arr_atDr
               -> if (Data.Vector.length arr_atDr == 2) then
                      ((TableHead
                          <$>
                            parseJSON (arr_atDr `Data.Vector.unsafeIndex` 0))
                         <*>
                           parseJSON (arr_atDr `Data.Vector.unsafeIndex` 1))
                  else
                      (((parseTypeMismatch' "TableHead")
                          "Text.Pandoc.Definition.TableHead")
                         "Array of length 2")
                        ("Array of length "
                           ++ (show . Data.Vector.length) arr_atDr)
             other_atDs
               -> (((parseTypeMismatch' "TableHead")
                      "Text.Pandoc.Definition.TableHead")
                     "Array")
                    (valueConName other_atDs)
instance ToJSON TableBody where
  toJSON
    = \ value_atDt
        -> case value_atDt of {
             TableBody arg1_atDu arg2_atDv arg3_atDw arg4_atDx
               -> Array
                    (Data.Vector.create
                       (do mv_atDy <- Data.Vector.Mutable.unsafeNew 4
                           ((Data.Vector.Mutable.unsafeWrite mv_atDy) 0)
                             (toJSON arg1_atDu)
                           ((Data.Vector.Mutable.unsafeWrite mv_atDy) 1)
                             (toJSON arg2_atDv)
                           ((Data.Vector.Mutable.unsafeWrite mv_atDy) 2)
                             (toJSON arg3_atDw)
                           ((Data.Vector.Mutable.unsafeWrite mv_atDy) 3)
                             (toJSON arg4_atDx)
                           return mv_atDy)) }
  toEncoding
    = \ value_atDz
        -> case value_atDz of {
             TableBody arg1_atDA arg2_atDB arg3_atDC arg4_atDD
               -> Data.Aeson.Encoding.Internal.wrapArray
                    (toEncoding arg1_atDA
                       Data.Aeson.Encoding.Internal.><
                         (Data.Aeson.Encoding.Internal.comma
                            Data.Aeson.Encoding.Internal.><
                              (toEncoding arg2_atDB
                                 Data.Aeson.Encoding.Internal.><
                                   (Data.Aeson.Encoding.Internal.comma
                                      Data.Aeson.Encoding.Internal.><
                                        (toEncoding arg3_atDC
                                           Data.Aeson.Encoding.Internal.><
                                             (Data.Aeson.Encoding.Internal.comma
                                                Data.Aeson.Encoding.Internal.><
                                                  toEncoding arg4_atDD)))))) }
instance FromJSON TableBody where
  parseJSON
    = \ value_atDE
        -> case value_atDE of
             Array arr_atDF
               -> if (Data.Vector.length arr_atDF == 4) then
                      ((((TableBody
                            <$>
                              parseJSON (arr_atDF `Data.Vector.unsafeIndex` 0))
                           <*>
                             parseJSON (arr_atDF `Data.Vector.unsafeIndex` 1))
                          <*>
                            parseJSON (arr_atDF `Data.Vector.unsafeIndex` 2))
                         <*>
                           parseJSON (arr_atDF `Data.Vector.unsafeIndex` 3))
                  else
                      (((parseTypeMismatch' "TableBody")
                          "Text.Pandoc.Definition.TableBody")
                         "Array of length 4")
                        ("Array of length "
                           ++ (show . Data.Vector.length) arr_atDF)
             other_atDG
               -> (((parseTypeMismatch' "TableBody")
                      "Text.Pandoc.Definition.TableBody")
                     "Array")
                    (valueConName other_atDG)
instance ToJSON TableFoot where
  toJSON
    = \ value_atDH
        -> case value_atDH of {
             TableFoot arg1_atDI arg2_atDJ
               -> Array
                    (Data.Vector.create
                       (do mv_atDK <- Data.Vector.Mutable.unsafeNew 2
                           ((Data.Vector.Mutable.unsafeWrite mv_atDK) 0)
                             (toJSON arg1_atDI)
                           ((Data.Vector.Mutable.unsafeWrite mv_atDK) 1)
                             (toJSON arg2_atDJ)
                           return mv_atDK)) }
  toEncoding
    = \ value_atDL
        -> case value_atDL of {
             TableFoot arg1_atDM arg2_atDN
               -> Data.Aeson.Encoding.Internal.wrapArray
                    (toEncoding arg1_atDM
                       Data.Aeson.Encoding.Internal.><
                         (Data.Aeson.Encoding.Internal.comma
                            Data.Aeson.Encoding.Internal.>< toEncoding arg2_atDN)) }
instance FromJSON TableFoot where
  parseJSON
    = \ value_atDO
        -> case value_atDO of
             Array arr_atDP
               -> if (Data.Vector.length arr_atDP == 2) then
                      ((TableFoot
                          <$>
                            parseJSON (arr_atDP `Data.Vector.unsafeIndex` 0))
                         <*>
                           parseJSON (arr_atDP `Data.Vector.unsafeIndex` 1))
                  else
                      (((parseTypeMismatch' "TableFoot")
                          "Text.Pandoc.Definition.TableFoot")
                         "Array of length 2")
                        ("Array of length "
                           ++ (show . Data.Vector.length) arr_atDP)
             other_atDQ
               -> (((parseTypeMismatch' "TableFoot")
                      "Text.Pandoc.Definition.TableFoot")
                     "Array")
                    (valueConName other_atDQ)
instance ToJSON Cell where
  toJSON
    = \ value_atDR
        -> case value_atDR of {
             Cell arg1_atDS arg2_atDT arg3_atDU arg4_atDV arg5_atDW
               -> Array
                    (Data.Vector.create
                       (do mv_atDX <- Data.Vector.Mutable.unsafeNew 5
                           ((Data.Vector.Mutable.unsafeWrite mv_atDX) 0)
                             (toJSON arg1_atDS)
                           ((Data.Vector.Mutable.unsafeWrite mv_atDX) 1)
                             (toJSON arg2_atDT)
                           ((Data.Vector.Mutable.unsafeWrite mv_atDX) 2)
                             (toJSON arg3_atDU)
                           ((Data.Vector.Mutable.unsafeWrite mv_atDX) 3)
                             (toJSON arg4_atDV)
                           ((Data.Vector.Mutable.unsafeWrite mv_atDX) 4)
                             (toJSON arg5_atDW)
                           return mv_atDX)) }
  toEncoding
    = \ value_atDY
        -> case value_atDY of {
             Cell arg1_atDZ arg2_atE0 arg3_atE1 arg4_atE2 arg5_atE3
               -> Data.Aeson.Encoding.Internal.wrapArray
                    (toEncoding arg1_atDZ
                       Data.Aeson.Encoding.Internal.><
                         (Data.Aeson.Encoding.Internal.comma
                            Data.Aeson.Encoding.Internal.><
                              (toEncoding arg2_atE0
                                 Data.Aeson.Encoding.Internal.><
                                   (Data.Aeson.Encoding.Internal.comma
                                      Data.Aeson.Encoding.Internal.><
                                        (toEncoding arg3_atE1
                                           Data.Aeson.Encoding.Internal.><
                                             (Data.Aeson.Encoding.Internal.comma
                                                Data.Aeson.Encoding.Internal.><
                                                  (toEncoding arg4_atE2
                                                     Data.Aeson.Encoding.Internal.><
                                                       (Data.Aeson.Encoding.Internal.comma
                                                          Data.Aeson.Encoding.Internal.><
                                                            toEncoding arg5_atE3)))))))) }
instance FromJSON Cell where
  parseJSON
    = \ value_atE4
        -> case value_atE4 of
             Array arr_atE5
               -> if (Data.Vector.length arr_atE5 == 5) then
                      (((((Cell
                             <$>
                               parseJSON (arr_atE5 `Data.Vector.unsafeIndex` 0))
                            <*>
                              parseJSON (arr_atE5 `Data.Vector.unsafeIndex` 1))
                           <*>
                             parseJSON (arr_atE5 `Data.Vector.unsafeIndex` 2))
                          <*>
                            parseJSON (arr_atE5 `Data.Vector.unsafeIndex` 3))
                         <*>
                           parseJSON (arr_atE5 `Data.Vector.unsafeIndex` 4))
                  else
                      (((parseTypeMismatch' "Cell")
                          "Text.Pandoc.Definition.Cell")
                         "Array of length 5")
                        ("Array of length "
                           ++ (show . Data.Vector.length) arr_atE5)
             other_atE6
               -> (((parseTypeMismatch' "Cell")
                      "Text.Pandoc.Definition.Cell")
                     "Array")
                    (valueConName other_atE6)
instance ToJSON Inline where
  toJSON
    = \ value_atE7
        -> case value_atE7 of
             Str arg1_atE8
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "Str"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (toJSON arg1_atE8))
             Emph arg1_atE9
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "Emph"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (toJSON arg1_atE9))
             Underline arg1_atEa
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "Underline"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (toJSON arg1_atEa))
             Strong arg1_atEb
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "Strong"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (toJSON arg1_atEb))
             Strikeout arg1_atEc
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "Strikeout"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (toJSON arg1_atEc))
             Superscript arg1_atEd
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "Superscript"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (toJSON arg1_atEd))
             Subscript arg1_atEe
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "Subscript"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (toJSON arg1_atEe))
             SmallCaps arg1_atEf
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "SmallCaps"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (toJSON arg1_atEf))
             Quoted arg1_atEg arg2_atEh
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "Quoted"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (Array
                              (Data.Vector.create
                                 (do mv_atEi <- Data.Vector.Mutable.unsafeNew 2
                                     ((Data.Vector.Mutable.unsafeWrite mv_atEi)
                                        0)
                                       (toJSON arg1_atEg)
                                     ((Data.Vector.Mutable.unsafeWrite mv_atEi)
                                        1)
                                       (toJSON arg2_atEh)
                                     return mv_atEi))))
             Cite arg1_atEj arg2_atEk
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "Cite"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (Array
                              (Data.Vector.create
                                 (do mv_atEl <- Data.Vector.Mutable.unsafeNew 2
                                     ((Data.Vector.Mutable.unsafeWrite mv_atEl)
                                        0)
                                       (toJSON arg1_atEj)
                                     ((Data.Vector.Mutable.unsafeWrite mv_atEl)
                                        1)
                                       (toJSON arg2_atEk)
                                     return mv_atEl))))
             Code arg1_atEm arg2_atEn
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "Code"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (Array
                              (Data.Vector.create
                                 (do mv_atEo <- Data.Vector.Mutable.unsafeNew 2
                                     ((Data.Vector.Mutable.unsafeWrite mv_atEo)
                                        0)
                                       (toJSON arg1_atEm)
                                     ((Data.Vector.Mutable.unsafeWrite mv_atEo)
                                        1)
                                       (toJSON arg2_atEn)
                                     return mv_atEo))))
             Space
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "Space")))
             SoftBreak
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "SoftBreak")))
             LineBreak
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "LineBreak")))
             Math arg1_atEp arg2_atEq
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "Math"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (Array
                              (Data.Vector.create
                                 (do mv_atEr <- Data.Vector.Mutable.unsafeNew 2
                                     ((Data.Vector.Mutable.unsafeWrite mv_atEr)
                                        0)
                                       (toJSON arg1_atEp)
                                     ((Data.Vector.Mutable.unsafeWrite mv_atEr)
                                        1)
                                       (toJSON arg2_atEq)
                                     return mv_atEr))))
             RawInline arg1_atEs arg2_atEt
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "RawInline"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (Array
                              (Data.Vector.create
                                 (do mv_atEu <- Data.Vector.Mutable.unsafeNew 2
                                     ((Data.Vector.Mutable.unsafeWrite mv_atEu)
                                        0)
                                       (toJSON arg1_atEs)
                                     ((Data.Vector.Mutable.unsafeWrite mv_atEu)
                                        1)
                                       (toJSON arg2_atEt)
                                     return mv_atEu))))
             Link arg1_atEv arg2_atEw arg3_atEx
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "Link"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (Array
                              (Data.Vector.create
                                 (do mv_atEy <- Data.Vector.Mutable.unsafeNew 3
                                     ((Data.Vector.Mutable.unsafeWrite mv_atEy)
                                        0)
                                       (toJSON arg1_atEv)
                                     ((Data.Vector.Mutable.unsafeWrite mv_atEy)
                                        1)
                                       (toJSON arg2_atEw)
                                     ((Data.Vector.Mutable.unsafeWrite mv_atEy)
                                        2)
                                       (toJSON arg3_atEx)
                                     return mv_atEy))))
             Image arg1_atEz arg2_atEA arg3_atEB
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "Image"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (Array
                              (Data.Vector.create
                                 (do mv_atEC <- Data.Vector.Mutable.unsafeNew 3
                                     ((Data.Vector.Mutable.unsafeWrite mv_atEC)
                                        0)
                                       (toJSON arg1_atEz)
                                     ((Data.Vector.Mutable.unsafeWrite mv_atEC)
                                        1)
                                       (toJSON arg2_atEA)
                                     ((Data.Vector.Mutable.unsafeWrite mv_atEC)
                                        2)
                                       (toJSON arg3_atEB)
                                     return mv_atEC))))
             Note arg1_atED
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "Note"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (toJSON arg1_atED))
             Span arg1_atEE arg2_atEF
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "Span"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (Array
                              (Data.Vector.create
                                 (do mv_atEG <- Data.Vector.Mutable.unsafeNew 2
                                     ((Data.Vector.Mutable.unsafeWrite mv_atEG)
                                        0)
                                       (toJSON arg1_atEE)
                                     ((Data.Vector.Mutable.unsafeWrite mv_atEG)
                                        1)
                                       (toJSON arg2_atEF)
                                     return mv_atEG))))
  toEncoding
    = \ value_atEH
        -> case value_atEH of
             Str arg1_atEI
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "Str"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (toEncoding arg1_atEI))
             Emph arg1_atEJ
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "Emph"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (toEncoding arg1_atEJ))
             Underline arg1_atEK
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "Underline"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (toEncoding arg1_atEK))
             Strong arg1_atEL
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "Strong"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (toEncoding arg1_atEL))
             Strikeout arg1_atEM
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "Strikeout"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (toEncoding arg1_atEM))
             Superscript arg1_atEN
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "Superscript"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (toEncoding arg1_atEN))
             Subscript arg1_atEO
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "Subscript"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (toEncoding arg1_atEO))
             SmallCaps arg1_atEP
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "SmallCaps"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (toEncoding arg1_atEP))
             Quoted arg1_atEQ arg2_atER
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "Quoted"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (Data.Aeson.Encoding.Internal.wrapArray
                              (toEncoding arg1_atEQ
                                 Data.Aeson.Encoding.Internal.><
                                   (Data.Aeson.Encoding.Internal.comma
                                      Data.Aeson.Encoding.Internal.>< toEncoding arg2_atER))))
             Cite arg1_atES arg2_atET
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "Cite"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (Data.Aeson.Encoding.Internal.wrapArray
                              (toEncoding arg1_atES
                                 Data.Aeson.Encoding.Internal.><
                                   (Data.Aeson.Encoding.Internal.comma
                                      Data.Aeson.Encoding.Internal.>< toEncoding arg2_atET))))
             Code arg1_atEU arg2_atEV
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "Code"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (Data.Aeson.Encoding.Internal.wrapArray
                              (toEncoding arg1_atEU
                                 Data.Aeson.Encoding.Internal.><
                                   (Data.Aeson.Encoding.Internal.comma
                                      Data.Aeson.Encoding.Internal.>< toEncoding arg2_atEV))))
             Space
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "Space")))
             SoftBreak
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "SoftBreak")))
             LineBreak
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "LineBreak")))
             Math arg1_atEW arg2_atEX
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "Math"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (Data.Aeson.Encoding.Internal.wrapArray
                              (toEncoding arg1_atEW
                                 Data.Aeson.Encoding.Internal.><
                                   (Data.Aeson.Encoding.Internal.comma
                                      Data.Aeson.Encoding.Internal.>< toEncoding arg2_atEX))))
             RawInline arg1_atEY arg2_atEZ
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "RawInline"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (Data.Aeson.Encoding.Internal.wrapArray
                              (toEncoding arg1_atEY
                                 Data.Aeson.Encoding.Internal.><
                                   (Data.Aeson.Encoding.Internal.comma
                                      Data.Aeson.Encoding.Internal.>< toEncoding arg2_atEZ))))
             Link arg1_atF0 arg2_atF1 arg3_atF2
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "Link"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (Data.Aeson.Encoding.Internal.wrapArray
                              (toEncoding arg1_atF0
                                 Data.Aeson.Encoding.Internal.><
                                   (Data.Aeson.Encoding.Internal.comma
                                      Data.Aeson.Encoding.Internal.><
                                        (toEncoding arg2_atF1
                                           Data.Aeson.Encoding.Internal.><
                                             (Data.Aeson.Encoding.Internal.comma
                                                Data.Aeson.Encoding.Internal.><
                                                  toEncoding arg3_atF2))))))
             Image arg1_atF3 arg2_atF4 arg3_atF5
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "Image"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (Data.Aeson.Encoding.Internal.wrapArray
                              (toEncoding arg1_atF3
                                 Data.Aeson.Encoding.Internal.><
                                   (Data.Aeson.Encoding.Internal.comma
                                      Data.Aeson.Encoding.Internal.><
                                        (toEncoding arg2_atF4
                                           Data.Aeson.Encoding.Internal.><
                                             (Data.Aeson.Encoding.Internal.comma
                                                Data.Aeson.Encoding.Internal.><
                                                  toEncoding arg3_atF5))))))
             Note arg1_atF6
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "Note"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (toEncoding arg1_atF6))
             Span arg1_atF7 arg2_atF8
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "Span"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (Data.Aeson.Encoding.Internal.wrapArray
                              (toEncoding arg1_atF7
                                 Data.Aeson.Encoding.Internal.><
                                   (Data.Aeson.Encoding.Internal.comma
                                      Data.Aeson.Encoding.Internal.>< toEncoding arg2_atF8))))
instance FromJSON Inline where
  parseJSON
    = \ value_atF9
        -> case value_atF9 of
             Object obj_atFa
               -> do conKey_atFb <- (obj_atFa .: T.pack "t")
                     case conKey_atFb of {
                       _ | (conKey_atFb == T.pack "Str")
                         -> do val_atFc <- (obj_atFa .: T.pack "c")
                               case val_atFc of { arg_atFd -> (Str <$> parseJSON arg_atFd) }
                         | (conKey_atFb == T.pack "Emph")
                         -> do val_atFe <- (obj_atFa .: T.pack "c")
                               case val_atFe of { arg_atFf -> (Emph <$> parseJSON arg_atFf) }
                         | (conKey_atFb == T.pack "Underline")
                         -> do val_atFg <- (obj_atFa .: T.pack "c")
                               case val_atFg of {
                                 arg_atFh -> (Underline <$> parseJSON arg_atFh) }
                         | (conKey_atFb == T.pack "Strong")
                         -> do val_atFi <- (obj_atFa .: T.pack "c")
                               case val_atFi of { arg_atFj -> (Strong <$> parseJSON arg_atFj) }
                         | (conKey_atFb == T.pack "Strikeout")
                         -> do val_atFk <- (obj_atFa .: T.pack "c")
                               case val_atFk of {
                                 arg_atFl -> (Strikeout <$> parseJSON arg_atFl) }
                         | (conKey_atFb == T.pack "Superscript")
                         -> do val_atFm <- (obj_atFa .: T.pack "c")
                               case val_atFm of {
                                 arg_atFn -> (Superscript <$> parseJSON arg_atFn) }
                         | (conKey_atFb == T.pack "Subscript")
                         -> do val_atFo <- (obj_atFa .: T.pack "c")
                               case val_atFo of {
                                 arg_atFp -> (Subscript <$> parseJSON arg_atFp) }
                         | (conKey_atFb == T.pack "SmallCaps")
                         -> do val_atFq <- (obj_atFa .: T.pack "c")
                               case val_atFq of {
                                 arg_atFr -> (SmallCaps <$> parseJSON arg_atFr) }
                         | (conKey_atFb == T.pack "Quoted")
                         -> do val_atFs <- (obj_atFa .: T.pack "c")
                               case val_atFs of
                                 Array arr_atFt
                                   -> if (Data.Vector.length arr_atFt == 2) then
                                          ((Quoted
                                              <$>
                                                parseJSON
                                                  (arr_atFt
                                                     `Data.Vector.unsafeIndex`
                                                       0))
                                             <*>
                                               parseJSON
                                                 (arr_atFt
                                                    `Data.Vector.unsafeIndex`
                                                      1))
                                      else
                                          (((parseTypeMismatch' "Quoted")
                                              "Text.Pandoc.Definition.Inline")
                                             "Array of length 2")
                                            ("Array of length "
                                               ++
                                                 (show . Data.Vector.length)
                                                   arr_atFt)
                                 other_atFu
                                   -> (((parseTypeMismatch' "Quoted")
                                          "Text.Pandoc.Definition.Inline")
                                         "Array")
                                        (valueConName other_atFu)
                         | (conKey_atFb == T.pack "Cite")
                         -> do val_atFv <- (obj_atFa .: T.pack "c")
                               case val_atFv of
                                 Array arr_atFw
                                   -> if (Data.Vector.length arr_atFw == 2) then
                                          ((Cite
                                              <$>
                                                parseJSON
                                                  (arr_atFw
                                                     `Data.Vector.unsafeIndex`
                                                       0))
                                             <*>
                                               parseJSON
                                                 (arr_atFw
                                                    `Data.Vector.unsafeIndex`
                                                      1))
                                      else
                                          (((parseTypeMismatch' "Cite")
                                              "Text.Pandoc.Definition.Inline")
                                             "Array of length 2")
                                            ("Array of length "
                                               ++
                                                 (show . Data.Vector.length)
                                                   arr_atFw)
                                 other_atFx
                                   -> (((parseTypeMismatch' "Cite")
                                          "Text.Pandoc.Definition.Inline")
                                         "Array")
                                        (valueConName other_atFx)
                         | (conKey_atFb == T.pack "Code")
                         -> do val_atFy <- (obj_atFa .: T.pack "c")
                               case val_atFy of
                                 Array arr_atFz
                                   -> if (Data.Vector.length arr_atFz == 2) then
                                          ((Code
                                              <$>
                                                parseJSON
                                                  (arr_atFz
                                                     `Data.Vector.unsafeIndex`
                                                       0))
                                             <*>
                                               parseJSON
                                                 (arr_atFz
                                                    `Data.Vector.unsafeIndex`
                                                      1))
                                      else
                                          (((parseTypeMismatch' "Code")
                                              "Text.Pandoc.Definition.Inline")
                                             "Array of length 2")
                                            ("Array of length "
                                               ++
                                                 (show . Data.Vector.length)
                                                   arr_atFz)
                                 other_atFA
                                   -> (((parseTypeMismatch' "Code")
                                          "Text.Pandoc.Definition.Inline")
                                         "Array")
                                        (valueConName other_atFA)
                         | (conKey_atFb == T.pack "Space") -> pure Space
                         | (conKey_atFb == T.pack "SoftBreak") -> pure SoftBreak
                         | (conKey_atFb == T.pack "LineBreak") -> pure LineBreak
                         | (conKey_atFb == T.pack "Math")
                         -> do val_atFB <- (obj_atFa .: T.pack "c")
                               case val_atFB of
                                 Array arr_atFC
                                   -> if (Data.Vector.length arr_atFC == 2) then
                                          ((Math
                                              <$>
                                                parseJSON
                                                  (arr_atFC
                                                     `Data.Vector.unsafeIndex`
                                                       0))
                                             <*>
                                               parseJSON
                                                 (arr_atFC
                                                    `Data.Vector.unsafeIndex`
                                                      1))
                                      else
                                          (((parseTypeMismatch' "Math")
                                              "Text.Pandoc.Definition.Inline")
                                             "Array of length 2")
                                            ("Array of length "
                                               ++
                                                 (show . Data.Vector.length)
                                                   arr_atFC)
                                 other_atFD
                                   -> (((parseTypeMismatch' "Math")
                                          "Text.Pandoc.Definition.Inline")
                                         "Array")
                                        (valueConName other_atFD)
                         | (conKey_atFb == T.pack "RawInline")
                         -> do val_atFE <- (obj_atFa .: T.pack "c")
                               case val_atFE of
                                 Array arr_atFF
                                   -> if (Data.Vector.length arr_atFF == 2) then
                                          ((RawInline
                                              <$>
                                                parseJSON
                                                  (arr_atFF
                                                     `Data.Vector.unsafeIndex`
                                                       0))
                                             <*>
                                               parseJSON
                                                 (arr_atFF
                                                    `Data.Vector.unsafeIndex`
                                                      1))
                                      else
                                          (((parseTypeMismatch' "RawInline")
                                              "Text.Pandoc.Definition.Inline")
                                             "Array of length 2")
                                            ("Array of length "
                                               ++
                                                 (show . Data.Vector.length)
                                                   arr_atFF)
                                 other_atFG
                                   -> (((parseTypeMismatch' "RawInline")
                                          "Text.Pandoc.Definition.Inline")
                                         "Array")
                                        (valueConName other_atFG)
                         | (conKey_atFb == T.pack "Link")
                         -> do val_atFH <- (obj_atFa .: T.pack "c")
                               case val_atFH of
                                 Array arr_atFI
                                   -> if (Data.Vector.length arr_atFI == 3) then
                                          (((Link
                                               <$>
                                                 parseJSON
                                                   (arr_atFI
                                                      `Data.Vector.unsafeIndex`
                                                        0))
                                              <*>
                                                parseJSON
                                                  (arr_atFI
                                                     `Data.Vector.unsafeIndex`
                                                       1))
                                             <*>
                                               parseJSON
                                                 (arr_atFI
                                                    `Data.Vector.unsafeIndex`
                                                      2))
                                      else
                                          (((parseTypeMismatch' "Link")
                                              "Text.Pandoc.Definition.Inline")
                                             "Array of length 3")
                                            ("Array of length "
                                               ++
                                                 (show . Data.Vector.length)
                                                   arr_atFI)
                                 other_atFJ
                                   -> (((parseTypeMismatch' "Link")
                                          "Text.Pandoc.Definition.Inline")
                                         "Array")
                                        (valueConName other_atFJ)
                         | (conKey_atFb == T.pack "Image")
                         -> do val_atFK <- (obj_atFa .: T.pack "c")
                               case val_atFK of
                                 Array arr_atFL
                                   -> if (Data.Vector.length arr_atFL == 3) then
                                          (((Image
                                               <$>
                                                 parseJSON
                                                   (arr_atFL
                                                      `Data.Vector.unsafeIndex`
                                                        0))
                                              <*>
                                                parseJSON
                                                  (arr_atFL
                                                     `Data.Vector.unsafeIndex`
                                                       1))
                                             <*>
                                               parseJSON
                                                 (arr_atFL
                                                    `Data.Vector.unsafeIndex`
                                                      2))
                                      else
                                          (((parseTypeMismatch' "Image")
                                              "Text.Pandoc.Definition.Inline")
                                             "Array of length 3")
                                            ("Array of length "
                                               ++
                                                 (show . Data.Vector.length)
                                                   arr_atFL)
                                 other_atFM
                                   -> (((parseTypeMismatch' "Image")
                                          "Text.Pandoc.Definition.Inline")
                                         "Array")
                                        (valueConName other_atFM)
                         | (conKey_atFb == T.pack "Note")
                         -> do val_atFN <- (obj_atFa .: T.pack "c")
                               case val_atFN of { arg_atFO -> (Note <$> parseJSON arg_atFO) }
                         | (conKey_atFb == T.pack "Span")
                         -> do val_atFP <- (obj_atFa .: T.pack "c")
                               case val_atFP of
                                 Array arr_atFQ
                                   -> if (Data.Vector.length arr_atFQ == 2) then
                                          ((Span
                                              <$>
                                                parseJSON
                                                  (arr_atFQ
                                                     `Data.Vector.unsafeIndex`
                                                       0))
                                             <*>
                                               parseJSON
                                                 (arr_atFQ
                                                    `Data.Vector.unsafeIndex`
                                                      1))
                                      else
                                          (((parseTypeMismatch' "Span")
                                              "Text.Pandoc.Definition.Inline")
                                             "Array of length 2")
                                            ("Array of length "
                                               ++
                                                 (show . Data.Vector.length)
                                                   arr_atFQ)
                                 other_atFR
                                   -> (((parseTypeMismatch' "Span")
                                          "Text.Pandoc.Definition.Inline")
                                         "Array")
                                        (valueConName other_atFR)
                         | otherwise
                         -> ((conNotFoundFailTaggedObject
                                "Text.Pandoc.Definition.Inline")
                               ["Str", "Emph", "Underline", "Strong", "Strikeout",
                                "Superscript", "Subscript", "SmallCaps", "Quoted", "Cite",
                                "Code", "Space", "SoftBreak", "LineBreak", "Math", "RawInline",
                                "Link", "Image", "Note", "Span"])
                              (T.unpack conKey_atFb) }
             other_atFS
               -> (Data.Aeson.TH.noObjectFail "Text.Pandoc.Definition.Inline")
                    (valueConName other_atFS)
instance ToJSON Block where
  toJSON
    = \ value_atFT
        -> case value_atFT of
             Plain arg1_atFU
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "Plain"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (toJSON arg1_atFU))
             Para arg1_atFV
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "Para"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (toJSON arg1_atFV))
             LineBlock arg1_atFW
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "LineBlock"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (toJSON arg1_atFW))
             CodeBlock arg1_atFX arg2_atFY
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "CodeBlock"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (Array
                              (Data.Vector.create
                                 (do mv_atFZ <- Data.Vector.Mutable.unsafeNew 2
                                     ((Data.Vector.Mutable.unsafeWrite mv_atFZ)
                                        0)
                                       (toJSON arg1_atFX)
                                     ((Data.Vector.Mutable.unsafeWrite mv_atFZ)
                                        1)
                                       (toJSON arg2_atFY)
                                     return mv_atFZ))))
             RawBlock arg1_atG0 arg2_atG1
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "RawBlock"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (Array
                              (Data.Vector.create
                                 (do mv_atG2 <- Data.Vector.Mutable.unsafeNew 2
                                     ((Data.Vector.Mutable.unsafeWrite mv_atG2)
                                        0)
                                       (toJSON arg1_atG0)
                                     ((Data.Vector.Mutable.unsafeWrite mv_atG2)
                                        1)
                                       (toJSON arg2_atG1)
                                     return mv_atG2))))
             BlockQuote arg1_atG3
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "BlockQuote"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (toJSON arg1_atG3))
             OrderedList arg1_atG4 arg2_atG5
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "OrderedList"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (Array
                              (Data.Vector.create
                                 (do mv_atG6 <- Data.Vector.Mutable.unsafeNew 2
                                     ((Data.Vector.Mutable.unsafeWrite mv_atG6)
                                        0)
                                       (toJSON arg1_atG4)
                                     ((Data.Vector.Mutable.unsafeWrite mv_atG6)
                                        1)
                                       (toJSON arg2_atG5)
                                     return mv_atG6))))
             BulletList arg1_atG7
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "BulletList"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (toJSON arg1_atG7))
             DefinitionList arg1_atG8
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "DefinitionList"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (toJSON arg1_atG8))
             Header arg1_atG9 arg2_atGa arg3_atGb
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "Header"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (Array
                              (Data.Vector.create
                                 (do mv_atGc <- Data.Vector.Mutable.unsafeNew 3
                                     ((Data.Vector.Mutable.unsafeWrite mv_atGc)
                                        0)
                                       (toJSON arg1_atG9)
                                     ((Data.Vector.Mutable.unsafeWrite mv_atGc)
                                        1)
                                       (toJSON arg2_atGa)
                                     ((Data.Vector.Mutable.unsafeWrite mv_atGc)
                                        2)
                                       (toJSON arg3_atGb)
                                     return mv_atGc))))
             HorizontalRule
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "HorizontalRule")))
             Table arg1_atGd arg2_atGe arg3_atGf arg4_atGg arg5_atGh arg6_atGi
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "Table"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (Array
                              (Data.Vector.create
                                 (do mv_atGj <- Data.Vector.Mutable.unsafeNew 6
                                     ((Data.Vector.Mutable.unsafeWrite mv_atGj)
                                        0)
                                       (toJSON arg1_atGd)
                                     ((Data.Vector.Mutable.unsafeWrite mv_atGj)
                                        1)
                                       (toJSON arg2_atGe)
                                     ((Data.Vector.Mutable.unsafeWrite mv_atGj)
                                        2)
                                       (toJSON arg3_atGf)
                                     ((Data.Vector.Mutable.unsafeWrite mv_atGj)
                                        3)
                                       (toJSON arg4_atGg)
                                     ((Data.Vector.Mutable.unsafeWrite mv_atGj)
                                        4)
                                       (toJSON arg5_atGh)
                                     ((Data.Vector.Mutable.unsafeWrite mv_atGj)
                                        5)
                                       (toJSON arg6_atGi)
                                     return mv_atGj))))
             Div arg1_atGk arg2_atGl
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "Div"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (Array
                              (Data.Vector.create
                                 (do mv_atGm <- Data.Vector.Mutable.unsafeNew 2
                                     ((Data.Vector.Mutable.unsafeWrite mv_atGm)
                                        0)
                                       (toJSON arg1_atGk)
                                     ((Data.Vector.Mutable.unsafeWrite mv_atGm)
                                        1)
                                       (toJSON arg2_atGl)
                                     return mv_atGm))))
             Null
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (String (T.pack "Null")))
  toEncoding
    = \ value_atGn
        -> case value_atGn of
             Plain arg1_atGo
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "Plain"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (toEncoding arg1_atGo))
             Para arg1_atGp
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "Para"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (toEncoding arg1_atGp))
             LineBlock arg1_atGq
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "LineBlock"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (toEncoding arg1_atGq))
             CodeBlock arg1_atGr arg2_atGs
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "CodeBlock"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (Data.Aeson.Encoding.Internal.wrapArray
                              (toEncoding arg1_atGr
                                 Data.Aeson.Encoding.Internal.><
                                   (Data.Aeson.Encoding.Internal.comma
                                      Data.Aeson.Encoding.Internal.>< toEncoding arg2_atGs))))
             RawBlock arg1_atGt arg2_atGu
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "RawBlock"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (Data.Aeson.Encoding.Internal.wrapArray
                              (toEncoding arg1_atGt
                                 Data.Aeson.Encoding.Internal.><
                                   (Data.Aeson.Encoding.Internal.comma
                                      Data.Aeson.Encoding.Internal.>< toEncoding arg2_atGu))))
             BlockQuote arg1_atGv
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "BlockQuote"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (toEncoding arg1_atGv))
             OrderedList arg1_atGw arg2_atGx
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "OrderedList"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (Data.Aeson.Encoding.Internal.wrapArray
                              (toEncoding arg1_atGw
                                 Data.Aeson.Encoding.Internal.><
                                   (Data.Aeson.Encoding.Internal.comma
                                      Data.Aeson.Encoding.Internal.>< toEncoding arg2_atGx))))
             BulletList arg1_atGy
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "BulletList"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (toEncoding arg1_atGy))
             DefinitionList arg1_atGz
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "DefinitionList"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (toEncoding arg1_atGz))
             Header arg1_atGA arg2_atGB arg3_atGC
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "Header"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (Data.Aeson.Encoding.Internal.wrapArray
                              (toEncoding arg1_atGA
                                 Data.Aeson.Encoding.Internal.><
                                   (Data.Aeson.Encoding.Internal.comma
                                      Data.Aeson.Encoding.Internal.><
                                        (toEncoding arg2_atGB
                                           Data.Aeson.Encoding.Internal.><
                                             (Data.Aeson.Encoding.Internal.comma
                                                Data.Aeson.Encoding.Internal.><
                                                  toEncoding arg3_atGC))))))
             HorizontalRule
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "HorizontalRule")))
             Table arg1_atGD arg2_atGE arg3_atGF arg4_atGG arg5_atGH arg6_atGI
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "Table"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (Data.Aeson.Encoding.Internal.wrapArray
                              (toEncoding arg1_atGD
                                 Data.Aeson.Encoding.Internal.><
                                   (Data.Aeson.Encoding.Internal.comma
                                      Data.Aeson.Encoding.Internal.><
                                        (toEncoding arg2_atGE
                                           Data.Aeson.Encoding.Internal.><
                                             (Data.Aeson.Encoding.Internal.comma
                                                Data.Aeson.Encoding.Internal.><
                                                  (toEncoding arg3_atGF
                                                     Data.Aeson.Encoding.Internal.><
                                                       (Data.Aeson.Encoding.Internal.comma
                                                          Data.Aeson.Encoding.Internal.><
                                                            (toEncoding arg4_atGG
                                                               Data.Aeson.Encoding.Internal.><
                                                                 (Data.Aeson.Encoding.Internal.comma
                                                                    Data.Aeson.Encoding.Internal.><
                                                                      (toEncoding arg5_atGH
                                                                         Data.Aeson.Encoding.Internal.><
                                                                           (Data.Aeson.Encoding.Internal.comma
                                                                              Data.Aeson.Encoding.Internal.><
                                                                                toEncoding
                                                                                  arg6_atGI))))))))))))
             Div arg1_atGJ arg2_atGK
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "Div"))
                       <>
                         (Data.Aeson.Encoding.Internal.pair "c")
                           (Data.Aeson.Encoding.Internal.wrapArray
                              (toEncoding arg1_atGJ
                                 Data.Aeson.Encoding.Internal.><
                                   (Data.Aeson.Encoding.Internal.comma
                                      Data.Aeson.Encoding.Internal.>< toEncoding arg2_atGK))))
             Null
               -> Data.Aeson.Encoding.Internal.pairs
                    ((Data.Aeson.Encoding.Internal.pair "t")
                       (Data.Aeson.Encoding.Internal.text (T.pack "Null")))
instance FromJSON Block where
  parseJSON
    = \ value_atGL
        -> case value_atGL of
             Object obj_atGM
               -> do conKey_atGN <- (obj_atGM .: T.pack "t")
                     case conKey_atGN of {
                       _ | (conKey_atGN == T.pack "Plain")
                         -> do val_atGO <- (obj_atGM .: T.pack "c")
                               case val_atGO of { arg_atGP -> (Plain <$> parseJSON arg_atGP) }
                         | (conKey_atGN == T.pack "Para")
                         -> do val_atGQ <- (obj_atGM .: T.pack "c")
                               case val_atGQ of { arg_atGR -> (Para <$> parseJSON arg_atGR) }
                         | (conKey_atGN == T.pack "LineBlock")
                         -> do val_atGS <- (obj_atGM .: T.pack "c")
                               case val_atGS of {
                                 arg_atGT -> (LineBlock <$> parseJSON arg_atGT) }
                         | (conKey_atGN == T.pack "CodeBlock")
                         -> do val_atGU <- (obj_atGM .: T.pack "c")
                               case val_atGU of
                                 Array arr_atGV
                                   -> if (Data.Vector.length arr_atGV == 2) then
                                          ((CodeBlock
                                              <$>
                                                parseJSON
                                                  (arr_atGV
                                                     `Data.Vector.unsafeIndex`
                                                       0))
                                             <*>
                                               parseJSON
                                                 (arr_atGV
                                                    `Data.Vector.unsafeIndex`
                                                      1))
                                      else
                                          (((parseTypeMismatch' "CodeBlock")
                                              "Text.Pandoc.Definition.Block")
                                             "Array of length 2")
                                            ("Array of length "
                                               ++
                                                 (show . Data.Vector.length)
                                                   arr_atGV)
                                 other_atGW
                                   -> (((parseTypeMismatch' "CodeBlock")
                                          "Text.Pandoc.Definition.Block")
                                         "Array")
                                        (valueConName other_atGW)
                         | (conKey_atGN == T.pack "RawBlock")
                         -> do val_atGX <- (obj_atGM .: T.pack "c")
                               case val_atGX of
                                 Array arr_atGY
                                   -> if (Data.Vector.length arr_atGY == 2) then
                                          ((RawBlock
                                              <$>
                                                parseJSON
                                                  (arr_atGY
                                                     `Data.Vector.unsafeIndex`
                                                       0))
                                             <*>
                                               parseJSON
                                                 (arr_atGY
                                                    `Data.Vector.unsafeIndex`
                                                      1))
                                      else
                                          (((parseTypeMismatch' "RawBlock")
                                              "Text.Pandoc.Definition.Block")
                                             "Array of length 2")
                                            ("Array of length "
                                               ++
                                                 (show . Data.Vector.length)
                                                   arr_atGY)
                                 other_atGZ
                                   -> (((parseTypeMismatch' "RawBlock")
                                          "Text.Pandoc.Definition.Block")
                                         "Array")
                                        (valueConName other_atGZ)
                         | (conKey_atGN == T.pack "BlockQuote")
                         -> do val_atH0 <- (obj_atGM .: T.pack "c")
                               case val_atH0 of {
                                 arg_atH1 -> (BlockQuote <$> parseJSON arg_atH1) }
                         | (conKey_atGN == T.pack "OrderedList")
                         -> do val_atH2 <- (obj_atGM .: T.pack "c")
                               case val_atH2 of
                                 Array arr_atH3
                                   -> if (Data.Vector.length arr_atH3 == 2) then
                                          ((OrderedList
                                              <$>
                                                parseJSON
                                                  (arr_atH3
                                                     `Data.Vector.unsafeIndex`
                                                       0))
                                             <*>
                                               parseJSON
                                                 (arr_atH3
                                                    `Data.Vector.unsafeIndex`
                                                      1))
                                      else
                                          (((parseTypeMismatch' "OrderedList")
                                              "Text.Pandoc.Definition.Block")
                                             "Array of length 2")
                                            ("Array of length "
                                               ++
                                                 (show . Data.Vector.length)
                                                   arr_atH3)
                                 other_atH4
                                   -> (((parseTypeMismatch' "OrderedList")
                                          "Text.Pandoc.Definition.Block")
                                         "Array")
                                        (valueConName other_atH4)
                         | (conKey_atGN == T.pack "BulletList")
                         -> do val_atH5 <- (obj_atGM .: T.pack "c")
                               case val_atH5 of {
                                 arg_atH6 -> (BulletList <$> parseJSON arg_atH6) }
                         | (conKey_atGN == T.pack "DefinitionList")
                         -> do val_atH7 <- (obj_atGM .: T.pack "c")
                               case val_atH7 of {
                                 arg_atH8 -> (DefinitionList <$> parseJSON arg_atH8) }
                         | (conKey_atGN == T.pack "Header")
                         -> do val_atH9 <- (obj_atGM .: T.pack "c")
                               case val_atH9 of
                                 Array arr_atHa
                                   -> if (Data.Vector.length arr_atHa == 3) then
                                          (((Header
                                               <$>
                                                 parseJSON
                                                   (arr_atHa
                                                      `Data.Vector.unsafeIndex`
                                                        0))
                                              <*>
                                                parseJSON
                                                  (arr_atHa
                                                     `Data.Vector.unsafeIndex`
                                                       1))
                                             <*>
                                               parseJSON
                                                 (arr_atHa
                                                    `Data.Vector.unsafeIndex`
                                                      2))
                                      else
                                          (((parseTypeMismatch' "Header")
                                              "Text.Pandoc.Definition.Block")
                                             "Array of length 3")
                                            ("Array of length "
                                               ++
                                                 (show . Data.Vector.length)
                                                   arr_atHa)
                                 other_atHb
                                   -> (((parseTypeMismatch' "Header")
                                          "Text.Pandoc.Definition.Block")
                                         "Array")
                                        (valueConName other_atHb)
                         | (conKey_atGN == T.pack "HorizontalRule") -> pure HorizontalRule
                         | (conKey_atGN == T.pack "Table")
                         -> do val_atHc <- (obj_atGM .: T.pack "c")
                               case val_atHc of
                                 Array arr_atHd
                                   -> if (Data.Vector.length arr_atHd == 6) then
                                          ((((((Table
                                                  <$>
                                                    parseJSON
                                                      (arr_atHd
                                                         `Data.Vector.unsafeIndex`
                                                           0))
                                                 <*>
                                                   parseJSON
                                                     (arr_atHd
                                                        `Data.Vector.unsafeIndex`
                                                          1))
                                                <*>
                                                  parseJSON
                                                    (arr_atHd
                                                       `Data.Vector.unsafeIndex`
                                                         2))
                                               <*>
                                                 parseJSON
                                                   (arr_atHd
                                                      `Data.Vector.unsafeIndex`
                                                        3))
                                              <*>
                                                parseJSON
                                                  (arr_atHd
                                                     `Data.Vector.unsafeIndex`
                                                       4))
                                             <*>
                                               parseJSON
                                                 (arr_atHd
                                                    `Data.Vector.unsafeIndex`
                                                      5))
                                      else
                                          (((parseTypeMismatch' "Table")
                                              "Text.Pandoc.Definition.Block")
                                             "Array of length 6")
                                            ("Array of length "
                                               ++
                                                 (show . Data.Vector.length)
                                                   arr_atHd)
                                 other_atHe
                                   -> (((parseTypeMismatch' "Table")
                                          "Text.Pandoc.Definition.Block")
                                         "Array")
                                        (valueConName other_atHe)
                         | (conKey_atGN == T.pack "Div")
                         -> do val_atHf <- (obj_atGM .: T.pack "c")
                               case val_atHf of
                                 Array arr_atHg
                                   -> if (Data.Vector.length arr_atHg == 2) then
                                          ((Div
                                              <$>
                                                parseJSON
                                                  (arr_atHg
                                                     `Data.Vector.unsafeIndex`
                                                       0))
                                             <*>
                                               parseJSON
                                                 (arr_atHg
                                                    `Data.Vector.unsafeIndex`
                                                      1))
                                      else
                                          (((parseTypeMismatch' "Div")
                                              "Text.Pandoc.Definition.Block")
                                             "Array of length 2")
                                            ("Array of length "
                                               ++
                                                 (show . Data.Vector.length)
                                                   arr_atHg)
                                 other_atHh
                                   -> (((parseTypeMismatch' "Div")
                                          "Text.Pandoc.Definition.Block")
                                         "Array")
                                        (valueConName other_atHh)
                         | (conKey_atGN == T.pack "Null") -> pure Null
                         | otherwise
                         -> ((conNotFoundFailTaggedObject
                                "Text.Pandoc.Definition.Block")
                               ["Plain", "Para", "LineBlock", "CodeBlock", "RawBlock",
                                "BlockQuote", "OrderedList", "BulletList", "DefinitionList",
                                "Header", "HorizontalRule", "Table", "Div", "Null"])
                              (T.unpack conKey_atGN) }
             other_atHi
               -> (Data.Aeson.TH.noObjectFail "Text.Pandoc.Definition.Block")
                    (valueConName other_atHi)

instance FromJSON Meta where
  parseJSON = fmap Meta . parseJSON
instance ToJSON Meta where
  toJSON (Meta m) = toJSON m
  toEncoding (Meta m) = toEncoding m

instance FromJSON Pandoc where
  parseJSON (Object v) = do
    mbJVersion <- v .:? "pandoc-api-version" :: Aeson.Parser (Maybe [Int])
    case mbJVersion of
      Just jVersion  | x : y : _ <- jVersion
                     , x' : y' : _ <- versionBranch pandocTypesVersion
                     , x == x'
                     , y == y' -> Pandoc <$> v .: "meta" <*> v .: "blocks"
                     | otherwise ->
                         fail $ mconcat [ "Incompatible API versions: "
                                        , "encoded with "
                                        , show jVersion
                                        , " but attempted to decode with "
                                        , show $ versionBranch pandocTypesVersion
                                        , "."
                                        ]
      _ -> fail "JSON missing pandoc-api-version."
  parseJSON _ = mempty
instance ToJSON Pandoc where
  toJSON (Pandoc meta blks) =
    object [ "pandoc-api-version" .= versionBranch pandocTypesVersion
           , "meta"               .= meta
           , "blocks"             .= blks
           ]
  toEncoding (Pandoc meta blks) =
    pairs $ mconcat [ "pandoc-api-version" .= versionBranch pandocTypesVersion
                    , "meta"               .= meta
                    , "blocks"             .= blks
                    ]

-- Instances for deepseq
instance NFData MetaValue
instance NFData Meta
instance NFData Citation
instance NFData Alignment
instance NFData RowSpan
instance NFData ColSpan
instance NFData Cell
instance NFData Row
instance NFData TableHead
instance NFData TableBody
instance NFData TableFoot
instance NFData Caption
instance NFData Inline
instance NFData MathType
instance NFData Format
instance NFData CitationMode
instance NFData QuoteType
instance NFData ListNumberDelim
instance NFData ListNumberStyle
instance NFData ColWidth
instance NFData RowHeadColumns
instance NFData Block
instance NFData Pandoc

pandocTypesVersion :: Version
pandocTypesVersion = version
