-- | Contains all the data structures and functions for composing
-- pieces of graphical text.
module Helm.Graphics2D.Text
  (
    -- * Types
    Text(..)
  , FontWeight(..)
  , FontStyle(..)
  , Alignment(..)
  , TextAlignment(..)
    -- * Composing
  , defaultText
  , toText
  , showText
    -- * Formatting
  , light
  , bold
  , italic
  , oblique
  , color
  , typeface
  , height
  , align
  , alignCenter
  , alignBottomLeft
  ) where

import Helm.Color (Color(..), rgb)

-- | Represents the weight for a text's font.
data FontWeight
  = LightWeight
  | NormalWeight
  | BoldWeight
    deriving (Show, Eq, Ord, Enum, Read)

-- | Represents the style for a text's font.
data FontStyle
  = NormalStyle
  | ObliqueStyle
  | ItalicStyle
    deriving (Show, Eq, Ord, Enum, Read)

-- | Represents alignment along an axis.
data Alignment
  = AlignBefore -- ^ draw before the origin
  | AlignCenter -- ^ center on the origin
  | AlignAfter  -- ^ draw after the origin
    deriving (Show, Eq, Ord, Enum, Read)

-- | Determines where text should be aligned relative to its origin.
-- Note that the vertical axis runs from top to bottom of a screen,
-- so AlignBefore moves text upwards and vice versa.
data TextAlignment = TextAlignment
  { verticalAlignment   :: Alignment
  , horizontalAlignment :: Alignment
  } deriving (Show, Eq)

-- | Represents a graphical piece of text,
-- containing a UTF-8 string and any relevant style fields.
data Text = Text
  { textString :: String
  , textColor :: Color
  , textTypeface :: String
  , textHeight :: Double
  , textWeight :: FontWeight
  , textStyle :: FontStyle
  , textAlign :: TextAlignment
  } deriving (Show, Eq)

-- | Create the default text. By default it is is black sans-serif
-- with a height of 14pt, centered on the origin.
defaultText :: Text
defaultText = Text {
  textString = "",
  textColor = rgb 0 0 0,
  textTypeface = "sans-serif",
  textHeight = 14,
  textWeight = NormalWeight,
  textStyle = NormalStyle,
  textAlign = TextAlignment AlignCenter AlignCenter
}

-- | Create a text from a string. By default, this text will be 14pt,
-- black and unstyled.
toText :: String -> Text
toText str = defaultText { textString = str }

-- | Create a text from a type instancing 'Show'.
showText :: Show a => a -> Text
showText a = toText $ show a

-- | Set the weight of a text to bold.
bold :: Text -> Text
bold txt = txt { textWeight = BoldWeight }

-- | Set the weight of a piece of text to light.
light :: Text -> Text
light txt = txt { textWeight = LightWeight }

-- | Set the slant of a piece of text to italic.
italic :: Text -> Text
italic txt = txt { textStyle = ItalicStyle }

-- | Set the slant of a piece of text to oblique.
oblique :: Text -> Text
oblique txt = txt { textStyle = ObliqueStyle }

-- | Set the color of a piece of text.
color :: Color -> Text -> Text
color col txt = txt { textColor = col }

-- | Set the typeface of the text.
typeface :: String -> Text -> Text
typeface face txt = txt { textTypeface = face }

-- | Set the size of a piece of text.
height :: Double -> Text -> Text
height size txt = txt { textHeight = size }

-- | Set the alignment of a piece of text.
align :: TextAlignment -> Text -> Text
align alignment txt = txt { textAlign = alignment }

-- | Convenience function to center text on origin.
alignCenter :: Text -> Text
alignCenter = align (TextAlignment AlignCenter AlignCenter)

-- | Convenience function to render text to the bottom-left of the origin.
alignBottomLeft :: Text -> Text
alignBottomLeft = align (TextAlignment AlignAfter AlignAfter)
