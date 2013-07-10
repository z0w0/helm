module FRP.Helm.Keyboard (
  shift,
  ctrl,
  enter,
  Key(..),
  space,
  arrows,
  wasd
) where

import Control.Applicative
import Data.List
import Foreign hiding (shift)
import Foreign.C.Types
import FRP.Elerea.Simple
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Utilities as Utilities

-- The SDL bindings for Haskell don't wrap this, so we have to use the FFI ourselves.
foreign import ccall unsafe "SDL_GetKeyState" sdlGetKeyState :: Ptr CInt -> IO (Ptr Word8)

-- Based on http://coderepos.org/share/browser/lang/haskell/nario/Main.hs?rev=22646#L49
getKeyState :: IO [SDL.SDLKey]
getKeyState = alloca $ \numkeysPtr -> do
  keysPtr <- sdlGetKeyState numkeysPtr
  numkeys <- peek numkeysPtr
  (map Utilities.toEnum . map fromIntegral . findIndices (== 1)) <$> peekArray (fromIntegral numkeys) keysPtr

data Key = BackspaceKey | TabKey | ClearKey | EnterKey | PauseKey | EscapeKey |
           SpaceKey | ExclaimKey | QuotedBlKey | HashKey | DollarKey | AmpersandKey |
           QuoteKey | LeftParenKey | RightParenKey | AsteriskKey | PlusKey | CommaKey |
           MinusKey | PeriodKey | SlashKey | Num0Key | Num1Key | Num2Key |
           Num3Key | Num4Key | Num5Key | Num6Key | Num7Key | Num8Key |
           Num9Key | ColonKey | SemicolonKey | LessKey | EqualsKey | GreaterKey |
           QuestionKey | AtKey | LeftBracketKey | BackslashKey | RightBracketKey | CaretKey |
           UnderscoreKey | BackquoteKey | AKey | BKey | CKey | DKey |
           EKey | FKey | GKey | HKey | IKey | JKey |
           LKey | MKey | NKey | OKey | PKey | QKey |
           RKey | SKey | TKey | UKey | VKey | WKey |
           XKey | YKey | ZKey | DeleteKey | Keypad0Key | Keypad1Key |
           Keypad2Key | Keypad3Key | Keypad4Key | Keypad5Key | Keypad6Key | Keypad7Key |
           Keypad8Key | Keypad9Key | KeypadPeriodKey | KeypadDivideKey | KeypadMultiplyKey | KeypadMinusKey |
           KeypadPlusKey | KeypadEnterKey | KeypadEqualsKey | UpKey | DownKey | RightKey |
           LeftKey | InsertKey | HomeKey | EndKey | PageUpKey | PageDownKey |
           F1Key | F2Key | F3Key | F4Key |  F5Key | F6Key |
           F7Key | F8Key | F9Key | F10Key | F11Key | F12Key |
           F13Key | F14Key | F15Key | NumLockKey | CapsLockKey | ScrollLockKey |
           RShiftKey | LShiftKey | RCtrlKey | LCtrlKey | RAltKey | LAltKey |
           RMetaKey | LMetaKey | RSuperKey | LSuperKey | ComposeKey | HelpKey |
           PrintKey | SysReqKey | BreakKey | MenuKey | PowerKey | EuroKey |
           UndoKey

mapKey :: Key -> SDL.SDLKey
mapKey k =
  case k of
    BackspaceKey -> SDL.SDLK_BACKSPACE
    TabKey -> SDL.SDLK_TAB
    ClearKey -> SDL.SDLK_CLEAR
    EnterKey -> SDL.SDLK_RETURN
    PauseKey -> SDL.SDLK_PAUSE
    EscapeKey -> SDL.SDLK_ESCAPE
    SpaceKey -> SDL.SDLK_SPACE
    ExclaimKey -> SDL.SDLK_EXCLAIM
    QuotedBlKey -> SDL.SDLK_QUOTEDBL
    HashKey -> SDL.SDLK_HASH
    DollarKey -> SDL.SDLK_DOLLAR
    AmpersandKey -> SDL.SDLK_AMPERSAND
    QuoteKey -> SDL.SDLK_QUOTE
    LeftParenKey -> SDL.SDLK_LEFTPAREN
    RightParenKey -> SDL.SDLK_RIGHTPAREN
    AsteriskKey -> SDL.SDLK_ASTERISK
    PlusKey -> SDL.SDLK_PLUS
    CommaKey -> SDL.SDLK_COMMA
    MinusKey -> SDL.SDLK_MINUS
    PeriodKey -> SDL.SDLK_PERIOD
    SlashKey -> SDL.SDLK_SLASH
    Num0Key -> SDL.SDLK_0
    Num1Key -> SDL.SDLK_1
    Num2Key -> SDL.SDLK_2
    Num3Key -> SDL.SDLK_3
    Num4Key -> SDL.SDLK_4
    Num5Key -> SDL.SDLK_5
    Num6Key -> SDL.SDLK_6
    Num7Key -> SDL.SDLK_7
    Num8Key -> SDL.SDLK_8
    Num9Key -> SDL.SDLK_9
    ColonKey -> SDL.SDLK_COLON
    SemicolonKey -> SDL.SDLK_SEMICOLON
    LessKey -> SDL.SDLK_LESS
    EqualsKey -> SDL.SDLK_EQUALS
    GreaterKey -> SDL.SDLK_GREATER
    QuestionKey -> SDL.SDLK_QUESTION
    AtKey -> SDL.SDLK_AT
    LeftBracketKey -> SDL.SDLK_LEFTBRACKET
    BackslashKey -> SDL.SDLK_BACKSLASH
    RightBracketKey -> SDL.SDLK_RIGHTBRACKET
    CaretKey -> SDL.SDLK_CARET
    UnderscoreKey -> SDL.SDLK_UNDERSCORE
    BackquoteKey -> SDL.SDLK_BACKQUOTE
    AKey -> SDL.SDLK_a
    BKey -> SDL.SDLK_b
    CKey -> SDL.SDLK_c
    DKey -> SDL.SDLK_d
    EKey -> SDL.SDLK_e
    FKey -> SDL.SDLK_f
    GKey -> SDL.SDLK_g
    HKey -> SDL.SDLK_h
    IKey -> SDL.SDLK_i
    JKey -> SDL.SDLK_j
    LKey -> SDL.SDLK_l
    MKey -> SDL.SDLK_m
    NKey -> SDL.SDLK_n
    OKey -> SDL.SDLK_o
    PKey -> SDL.SDLK_p
    QKey -> SDL.SDLK_q
    RKey -> SDL.SDLK_r
    SKey -> SDL.SDLK_s
    TKey -> SDL.SDLK_t
    UKey -> SDL.SDLK_u
    VKey -> SDL.SDLK_v
    WKey -> SDL.SDLK_w
    XKey -> SDL.SDLK_x
    YKey -> SDL.SDLK_y
    ZKey -> SDL.SDLK_z
    DeleteKey -> SDL.SDLK_DELETE
    Keypad0Key -> SDL.SDLK_KP0
    Keypad1Key -> SDL.SDLK_KP1
    Keypad2Key -> SDL.SDLK_KP2
    Keypad3Key -> SDL.SDLK_KP3
    Keypad4Key -> SDL.SDLK_KP4
    Keypad5Key -> SDL.SDLK_KP5
    Keypad6Key -> SDL.SDLK_KP6
    Keypad7Key -> SDL.SDLK_KP7
    Keypad8Key -> SDL.SDLK_KP8
    Keypad9Key -> SDL.SDLK_KP9
    KeypadPeriodKey -> SDL.SDLK_KP_PERIOD
    KeypadDivideKey -> SDL.SDLK_KP_DIVIDE
    KeypadMultiplyKey -> SDL.SDLK_KP_MULTIPLY
    KeypadMinusKey -> SDL.SDLK_KP_MINUS
    KeypadPlusKey -> SDL.SDLK_KP_PLUS
    KeypadEnterKey -> SDL.SDLK_KP_ENTER
    KeypadEqualsKey -> SDL.SDLK_KP_EQUALS
    UpKey -> SDL.SDLK_UP
    DownKey -> SDL.SDLK_DOWN
    RightKey -> SDL.SDLK_RIGHT
    LeftKey -> SDL.SDLK_LEFT
    InsertKey -> SDL.SDLK_INSERT
    HomeKey -> SDL.SDLK_HOME
    EndKey -> SDL.SDLK_END
    PageUpKey -> SDL.SDLK_PAGEUP
    PageDownKey -> SDL.SDLK_PAGEDOWN
    F1Key -> SDL.SDLK_F1
    F2Key -> SDL.SDLK_F2
    F3Key -> SDL.SDLK_F3
    F4Key -> SDL.SDLK_F4
    F5Key -> SDL.SDLK_F5
    F6Key -> SDL.SDLK_F6
    F7Key -> SDL.SDLK_F7
    F8Key -> SDL.SDLK_F8
    F9Key -> SDL.SDLK_F9
    F10Key -> SDL.SDLK_F10
    F11Key -> SDL.SDLK_F11
    F12Key -> SDL.SDLK_F12
    F13Key -> SDL.SDLK_F13
    F14Key -> SDL.SDLK_F14
    F15Key -> SDL.SDLK_F15
    NumLockKey -> SDL.SDLK_NUMLOCK
    CapsLockKey -> SDL.SDLK_CAPSLOCK
    ScrollLockKey -> SDL.SDLK_SCROLLOCK
    RShiftKey -> SDL.SDLK_RSHIFT
    LShiftKey -> SDL.SDLK_LSHIFT
    RCtrlKey -> SDL.SDLK_RCTRL
    LCtrlKey -> SDL.SDLK_LCTRL
    RAltKey -> SDL.SDLK_RALT
    LAltKey -> SDL.SDLK_LALT
    RMetaKey -> SDL.SDLK_RMETA
    LMetaKey -> SDL.SDLK_LMETA
    RSuperKey -> SDL.SDLK_RSUPER
    LSuperKey -> SDL.SDLK_LSUPER
    ComposeKey -> SDL.SDLK_COMPOSE
    HelpKey -> SDL.SDLK_HELP
    PrintKey -> SDL.SDLK_PRINT
    SysReqKey -> SDL.SDLK_SYSREQ
    BreakKey -> SDL.SDLK_BREAK
    MenuKey -> SDL.SDLK_MENU
    PowerKey -> SDL.SDLK_POWER
    EuroKey -> SDL.SDLK_EURO
    UndoKey -> SDL.SDLK_UNDO

-- |Whether either shift key is pressed.
shift :: SignalGen (Signal Bool)
shift = effectful $ (elem SDL.KeyModShift) <$> SDL.getModState

-- |Whether either control key is pressed.
ctrl :: SignalGen (Signal Bool)
ctrl = effectful $ (elem SDL.KeyModCtrl) <$> SDL.getModState

-- |Whether a specific key is pressed.
isDown :: Key -> SignalGen (Signal Bool)
isDown k = effectful $ (elem (mapKey k)) <$> getKeyState

-- |Whether the shift key is pressed.
enter :: SignalGen (Signal Bool)
enter = isDown EnterKey

-- |Whether the space key is pressed.
space :: SignalGen (Signal Bool)
space = isDown SpaceKey

{- TODO:
keysDown :: SignalGen (Signal [Key])
-}

{-| A unit vector combined from the arrow keys. When no keys are being pressed
    this signal samples to (0, 0), otherwise it samples to a specific direction
    based on which keys are pressed. For example, pressing the left key results
    in (-1, 0), the down key (0, 1), etc. -}
arrows :: SignalGen (Signal (Int, Int))
arrows = do
  up <- isDown UpKey
  left <- isDown LeftKey
  down <- isDown DownKey
  right <- isDown RightKey

  return $ arrows' <$> up <*> left <*> down <*> right

arrows' :: Bool -> Bool -> Bool -> Bool -> (Int, Int)
arrows' u l d r = (-1 * fromEnum l + 1 * fromEnum r, -1 * fromEnum u + 1 * fromEnum d)

-- |Similar to the 'arrows' signal, but uses the W, A, S and D keys instead.
wasd :: SignalGen (Signal (Int, Int))
wasd = do
  w <- isDown WKey
  a <- isDown AKey
  s <- isDown SKey
  d <- isDown DKey

  return $ arrows' <$> w <*> a <*> s <*> d
