{-| Contains all the data structures and functions for composing
    pieces of formatted text. -}
module FRP.Helm.Text (
  -- * Elements
  plainText,
  asText,
  text,
  -- * Composing
  defaultText,
  toText,
  -- * Formatting
  bold,
  italic,
  color,
  monospace,
  typeface,
  header,
  height
) where

import FRP.Helm.Color (Color, black)
import FRP.Helm.Graphics (Element(TextElement), Text(..))
import qualified Graphics.Rendering.Cairo as Cairo

{-| Creates the default text. By default the text is black sans-serif
    with a height of 14px. -}
defaultText :: Text
defaultText = Text {
  textUTF8 = "",
  textColor = black,
  textTypeface = "sans-serif",
  textHeight = 14,
  textWeight = Cairo.FontWeightNormal,
  textSlant = Cairo.FontSlantNormal
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

{- TODO:
centered
justified
righted
underline
strikeThrough
overline
-}

{-| Sets the weight of a piece of text to bold. -}
bold :: Text -> Text
bold txt = txt { textWeight = Cairo.FontWeightBold }

{-| Sets the slant of a piece of text to italic. -}
italic :: Text -> Text
italic txt = txt { textSlant = Cairo.FontSlantItalic }

{-| Sets the color of a piece of text. -}
color :: Color -> Text -> Text
color col txt = txt { textColor = col }

{-| Sets the typeface of the text to monospace. -}
monospace :: Text -> Text
monospace txt = txt { textTypeface = "monospace" }

{-| Sets the typeface of the text. Only fonts
    supported by Cairo's toy font API are currently
    supported. -}
typeface :: String -> Text -> Text
typeface face txt = txt { textTypeface = face }

{-| Sets the size of a text noticeably large. -}
header :: Text -> Text
header = height 32

{-| Sets the size of a piece of text. -}
height :: Double -> Text -> Text
height size txt = txt { textHeight = size }
