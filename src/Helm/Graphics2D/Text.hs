{-| Contains all the data structures and functions for composing
    pieces of formatted text. -}
module Helm.Graphics2D.Text (
  -- * Elements
  plainText,
  asText,
  text,
  -- * Composing
  defaultText,
  toText,
  -- * Formatting
  light,
  bold,
  italic,
  oblique,
  color,
  monospace,
  typeface,
  header,
  height
) where

import Helm.Color (Color(..), rgb)
import Helm.Graphics2D (Element(TextElement), Text(..), FontWeight(..), FontStyle(..))

{-| Creates the default text. By default the text is black sans-serif
    with a height of 14pt. -}
defaultText :: Text
defaultText = Text {
  textUTF8 = "",
  textColor = rgb 0 0 0,
  textTypeface = "sans-serif",
  textHeight = 14,
  textWeight = NormalWeight,
  textStyle = NormalStyle
}

{-| Creates a text from a string. -}
toText :: String -> Text
toText utf8 = defaultText { textUTF8 = utf8 }

{-| Creates a text element from a string. -}
plainText :: String -> Element
plainText utf8 = text $ toText utf8

{-| Creates a text element from any showable type, defaulting to
    the monospace typeface. -}
asText :: Show a => a -> Element
asText val = text $ monospace $ toText $ show val

{-| Creates an element from a text. -}
text :: Text -> Element
text = TextElement

{-| Sets the weight of a piece of text to bold. -}
bold :: Text -> Text
bold txt = txt { textWeight = BoldWeight }

{-| Sets the weight of a piece of text to light. -}
light :: Text -> Text
light txt = txt { textWeight = LightWeight }

{-| Sets the slant of a piece of text to italic. -}
italic :: Text -> Text
italic txt = txt { textStyle = ItalicStyle }

{-| Sets the slant of a piece of text to oblique. -}
oblique :: Text -> Text
oblique txt = txt { textStyle = ObliqueStyle }

{-| Sets the color of a piece of text. -}
color :: Color -> Text -> Text
color col txt = txt { textColor = col }

{-| Sets the typeface of the text to monospace. -}
monospace :: Text -> Text
monospace txt = txt { textTypeface = "monospace" }

{-| Sets the typeface of the text. -}
typeface :: String -> Text -> Text
typeface face txt = txt { textTypeface = face }

{-| Sets the size of a text noticeably large. -}
header :: Text -> Text
header = height 32

{-| Sets the size of a piece of text. -}
height :: Double -> Text -> Text
height size txt = txt { textHeight = size }
