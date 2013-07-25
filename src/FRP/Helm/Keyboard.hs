{-| Contains signals that sample input from the keyboard. -}
module FRP.Helm.Keyboard (
  -- * Types
  Key(..),
  -- * Key State
  shift, ctrl, enter,
  space, isDown, keysDown,
  -- * Directions
  arrows, wasd
) where

import Control.Applicative
import Data.List
import Foreign hiding (shift)
import Foreign.C.Types
import FRP.Elerea.Simple
import qualified Graphics.UI.SDL as SDL

{-| The SDL bindings for Haskell don't wrap this, so we have to use the FFI ourselves. -}
foreign import ccall unsafe "SDL_GetKeyState" sdlGetKeyState :: Ptr CInt -> IO (Ptr Word8)

{-| A utility function for getting a list of SDL keys currently pressed.
    Based on <http://coderepos.org/share/browser/lang/haskell/nario/Main.hs?rev=22646#L49>. -}
getKeyState :: IO [Int]
getKeyState = alloca $ \numkeysPtr -> do
  keysPtr <- sdlGetKeyState numkeysPtr
  numkeys <- peek numkeysPtr

  (map fromIntegral . elemIndices 1) <$> peekArray (fromIntegral numkeys) keysPtr

{-| A data structure describing a physical key on a keyboard. -}
data Key = BackspaceKey | TabKey | ClearKey | EnterKey | PauseKey | EscapeKey |
           SpaceKey | ExclaimKey | QuotedBlKey | HashKey | DollarKey | AmpersandKey |
           QuoteKey | LeftParenKey | RightParenKey | AsteriskKey | PlusKey | CommaKey |
           MinusKey | PeriodKey | SlashKey | Num0Key | Num1Key | Num2Key |
           Num3Key | Num4Key | Num5Key | Num6Key | Num7Key | Num8Key |
           Num9Key | ColonKey | SemicolonKey | LessKey | EqualsKey | GreaterKey |
           QuestionKey | AtKey | LeftBracketKey | BackslashKey | RightBracketKey | CaretKey |
           UnderscoreKey | BackquoteKey | AKey | BKey | CKey | DKey |
           EKey | FKey | GKey | HKey | IKey | JKey | KKey |
           LKey | MKey | NKey | OKey | PKey | QKey |
           RKey | SKey | TKey | UKey | VKey | WKey |
           XKey | YKey | ZKey | DeleteKey | KeypadNum0Key | KeypadNum1Key |
           KeypadNum2Key | KeypadNum3Key | KeypadNum4Key | KeypadNum5Key | KeypadNum6Key | KeypadNum7Key |
           KeypadNum8Key | KeypadNum9Key | KeypadPeriodKey | KeypadDivideKey | KeypadMultiplyKey | KeypadMinusKey |
           KeypadPlusKey | KeypadEnterKey | KeypadEqualsKey | UpKey | DownKey | RightKey |
           LeftKey | InsertKey | HomeKey | EndKey | PageUpKey | PageDownKey |
           F1Key | F2Key | F3Key | F4Key |  F5Key | F6Key |
           F7Key | F8Key | F9Key | F10Key | F11Key | F12Key |
           F13Key | F14Key | F15Key | NumLockKey | CapsLockKey | ScrollLockKey |
           RShiftKey | LShiftKey | RCtrlKey | LCtrlKey | RAltKey | LAltKey |
           RMetaKey | LMetaKey | RSuperKey | LSuperKey | ModeKey | ComposeKey | HelpKey |
           PrintKey | SysReqKey | BreakKey | MenuKey | PowerKey | EuroKey |
           UndoKey deriving (Show, Eq, Ord)

{- All integer values of this enum are equivalent to the SDL key enum. -}
instance Enum Key where
  fromEnum BackspaceKey = 8
  fromEnum TabKey = 9
  fromEnum ClearKey = 12
  fromEnum EnterKey = 13
  fromEnum PauseKey = 19
  fromEnum EscapeKey = 27
  fromEnum SpaceKey = 32
  fromEnum ExclaimKey = 33
  fromEnum QuotedBlKey = 34
  fromEnum HashKey = 35
  fromEnum DollarKey = 36
  fromEnum AmpersandKey = 38
  fromEnum QuoteKey = 39
  fromEnum LeftParenKey = 40
  fromEnum RightParenKey = 41
  fromEnum AsteriskKey = 42
  fromEnum PlusKey = 43
  fromEnum CommaKey = 44
  fromEnum MinusKey = 45
  fromEnum PeriodKey = 46
  fromEnum SlashKey = 47
  fromEnum Num0Key = 48
  fromEnum Num1Key = 49
  fromEnum Num2Key = 50
  fromEnum Num3Key = 51
  fromEnum Num4Key = 52
  fromEnum Num5Key = 53
  fromEnum Num6Key = 54
  fromEnum Num7Key = 55
  fromEnum Num8Key = 56
  fromEnum Num9Key = 57
  fromEnum ColonKey = 58
  fromEnum SemicolonKey = 59
  fromEnum LessKey = 60
  fromEnum EqualsKey = 61
  fromEnum GreaterKey = 62
  fromEnum QuestionKey = 63
  fromEnum AtKey = 64
  fromEnum LeftBracketKey = 91
  fromEnum BackslashKey = 92
  fromEnum RightBracketKey = 93
  fromEnum CaretKey = 94
  fromEnum UnderscoreKey = 95
  fromEnum BackquoteKey = 96
  fromEnum AKey = 97
  fromEnum BKey = 98
  fromEnum CKey = 99
  fromEnum DKey = 100
  fromEnum EKey = 101
  fromEnum FKey = 102
  fromEnum GKey = 103
  fromEnum HKey = 104
  fromEnum IKey = 105
  fromEnum JKey = 106
  fromEnum KKey = 107
  fromEnum LKey = 108
  fromEnum MKey = 109
  fromEnum NKey = 110
  fromEnum OKey = 111
  fromEnum PKey = 112
  fromEnum QKey = 113
  fromEnum RKey = 114
  fromEnum SKey = 115
  fromEnum TKey = 116
  fromEnum UKey = 117
  fromEnum VKey = 118
  fromEnum WKey = 119
  fromEnum XKey = 120
  fromEnum YKey = 121
  fromEnum ZKey = 122
  fromEnum DeleteKey = 127
  fromEnum KeypadNum0Key = 256
  fromEnum KeypadNum1Key = 257
  fromEnum KeypadNum2Key = 258
  fromEnum KeypadNum3Key = 259
  fromEnum KeypadNum4Key = 260
  fromEnum KeypadNum5Key = 261
  fromEnum KeypadNum6Key = 262
  fromEnum KeypadNum7Key = 263
  fromEnum KeypadNum8Key = 264
  fromEnum KeypadNum9Key = 265
  fromEnum KeypadPeriodKey = 266
  fromEnum KeypadDivideKey = 267
  fromEnum KeypadMultiplyKey = 268
  fromEnum KeypadMinusKey = 269
  fromEnum KeypadPlusKey = 270
  fromEnum KeypadEnterKey = 271
  fromEnum KeypadEqualsKey = 272
  fromEnum UpKey = 273
  fromEnum DownKey = 274
  fromEnum RightKey = 275
  fromEnum LeftKey = 276
  fromEnum InsertKey = 277
  fromEnum HomeKey = 278
  fromEnum EndKey = 279
  fromEnum PageUpKey = 280
  fromEnum PageDownKey = 281
  fromEnum F1Key = 282
  fromEnum F2Key = 283
  fromEnum F3Key = 284
  fromEnum F4Key = 285
  fromEnum F5Key = 286
  fromEnum F6Key = 287
  fromEnum F7Key = 288
  fromEnum F8Key = 289
  fromEnum F9Key = 290
  fromEnum F10Key = 291
  fromEnum F11Key = 292
  fromEnum F12Key = 293
  fromEnum F13Key = 294
  fromEnum F14Key = 295
  fromEnum F15Key = 296
  fromEnum NumLockKey = 300
  fromEnum CapsLockKey = 301
  fromEnum ScrollLockKey = 302
  fromEnum RShiftKey = 303
  fromEnum LShiftKey = 304
  fromEnum RCtrlKey = 305
  fromEnum LCtrlKey = 306
  fromEnum RAltKey = 307
  fromEnum LAltKey = 308
  fromEnum RMetaKey = 309
  fromEnum LMetaKey = 310
  fromEnum LSuperKey = 311
  fromEnum RSuperKey = 312
  fromEnum ModeKey = 313
  fromEnum ComposeKey = 314
  fromEnum HelpKey = 315
  fromEnum PrintKey = 316
  fromEnum SysReqKey = 317
  fromEnum BreakKey = 318
  fromEnum MenuKey = 319
  fromEnum PowerKey = 320
  fromEnum EuroKey = 321
  fromEnum UndoKey = 322

  toEnum 8 = BackspaceKey
  toEnum 9 = TabKey
  toEnum 12 = ClearKey
  toEnum 13 = EnterKey
  toEnum 19 = PauseKey
  toEnum 27 = EscapeKey
  toEnum 32 = SpaceKey
  toEnum 33 = ExclaimKey
  toEnum 34 = QuotedBlKey
  toEnum 35 = HashKey
  toEnum 36 = DollarKey
  toEnum 38 = AmpersandKey
  toEnum 39 = QuoteKey
  toEnum 40 = LeftParenKey
  toEnum 41 = RightParenKey
  toEnum 42 = AsteriskKey
  toEnum 43 = PlusKey
  toEnum 44 = CommaKey
  toEnum 45 = MinusKey
  toEnum 46 = PeriodKey
  toEnum 47 = SlashKey
  toEnum 48 = Num0Key
  toEnum 49 = Num1Key
  toEnum 50 = Num2Key
  toEnum 51 = Num3Key
  toEnum 52 = Num4Key
  toEnum 53 = Num5Key
  toEnum 54 = Num6Key
  toEnum 55 = Num7Key
  toEnum 56 = Num8Key
  toEnum 57 = Num9Key
  toEnum 58 = ColonKey
  toEnum 59 = SemicolonKey
  toEnum 60 = LessKey
  toEnum 61 = EqualsKey
  toEnum 62 = GreaterKey
  toEnum 63 = QuestionKey
  toEnum 64 = AtKey
  toEnum 91 = LeftBracketKey
  toEnum 92 = BackslashKey
  toEnum 93 = RightBracketKey
  toEnum 94 = CaretKey
  toEnum 95 = UnderscoreKey
  toEnum 96 = BackquoteKey
  toEnum 97 = AKey
  toEnum 98 = BKey
  toEnum 99 = CKey
  toEnum 100 = DKey
  toEnum 101 = EKey
  toEnum 102 = FKey
  toEnum 103 = GKey
  toEnum 104 = HKey
  toEnum 105 = IKey
  toEnum 106 = JKey
  toEnum 107 = KKey
  toEnum 108 = LKey
  toEnum 109 = MKey
  toEnum 110 = NKey
  toEnum 111 = OKey
  toEnum 112 = PKey
  toEnum 113 = QKey
  toEnum 114 = RKey
  toEnum 115 = SKey
  toEnum 116 = TKey
  toEnum 117 = UKey
  toEnum 118 = VKey
  toEnum 119 = WKey
  toEnum 120 = XKey
  toEnum 121 = YKey
  toEnum 122 = ZKey
  toEnum 127 = DeleteKey
  toEnum 256 = KeypadNum0Key
  toEnum 257 = KeypadNum1Key
  toEnum 258 = KeypadNum2Key
  toEnum 259 = KeypadNum3Key
  toEnum 260 = KeypadNum4Key
  toEnum 261 = KeypadNum5Key
  toEnum 262 = KeypadNum6Key
  toEnum 263 = KeypadNum7Key
  toEnum 264 = KeypadNum8Key
  toEnum 265 = KeypadNum9Key
  toEnum 266 = KeypadPeriodKey
  toEnum 267 = KeypadDivideKey
  toEnum 268 = KeypadMultiplyKey
  toEnum 269 = KeypadMinusKey
  toEnum 270 = KeypadPlusKey
  toEnum 271 = KeypadEnterKey
  toEnum 272 = KeypadEqualsKey
  toEnum 273 = UpKey
  toEnum 274 = DownKey
  toEnum 275 = RightKey
  toEnum 276 = LeftKey
  toEnum 277 = InsertKey
  toEnum 278 = HomeKey
  toEnum 279 = EndKey
  toEnum 280 = PageUpKey
  toEnum 281 = PageDownKey
  toEnum 282 = F1Key
  toEnum 283 = F2Key
  toEnum 284 = F3Key
  toEnum 285 = F4Key
  toEnum 286 = F5Key
  toEnum 287 = F6Key
  toEnum 288 = F7Key
  toEnum 289 = F8Key
  toEnum 290 = F9Key
  toEnum 291 = F10Key
  toEnum 292 = F11Key
  toEnum 293 = F12Key
  toEnum 294 = F13Key
  toEnum 295 = F14Key
  toEnum 296 = F15Key
  toEnum 300 = NumLockKey
  toEnum 301 = CapsLockKey
  toEnum 302 = ScrollLockKey
  toEnum 303 = RShiftKey
  toEnum 304 = LShiftKey
  toEnum 305 = RCtrlKey
  toEnum 306 = LCtrlKey
  toEnum 307 = RAltKey
  toEnum 308 = LAltKey
  toEnum 309 = RMetaKey
  toEnum 310 = LMetaKey
  toEnum 311 = LSuperKey
  toEnum 312 = RSuperKey
  toEnum 313 = ModeKey
  toEnum 314 = ComposeKey
  toEnum 315 = HelpKey
  toEnum 316 = PrintKey
  toEnum 317 = SysReqKey
  toEnum 318 = BreakKey
  toEnum 319 = MenuKey
  toEnum 320 = PowerKey
  toEnum 321 = EuroKey
  toEnum 322 = UndoKey
  toEnum _ = error "FRP.Helm.Keyboard.Key.toEnum: bad argument"

{-| Whether either shift key is pressed. -}
shift :: SignalGen (Signal Bool)
shift = effectful $ elem SDL.KeyModShift <$> SDL.getModState

{-| Whether either control key is pressed. -}
ctrl :: SignalGen (Signal Bool)
ctrl = effectful $ elem SDL.KeyModCtrl <$> SDL.getModState

{-| Whether a key is pressed. -}
isDown :: Key -> SignalGen (Signal Bool)
isDown k = effectful $ elem (fromEnum k) <$> getKeyState

{-| Whether the enter (a.k.a. return) key is pressed. -}
enter :: SignalGen (Signal Bool)
enter = isDown EnterKey

{-| Whether the space key is pressed. -}
space :: SignalGen (Signal Bool)
space = isDown SpaceKey

{-| A list of keys that are currently being pressed. -}
keysDown :: SignalGen (Signal [Key])
keysDown = effectful $ map toEnum <$> getKeyState

{-| A directional tuple combined from the arrow keys. When none of the arrow keys
    are being pressed this signal samples to /(0, 0)/, otherwise it samples to a
    direction based on which keys are pressed. For example, pressing the left key
    results in /(-1, 0)/, the down key /(0, 1)/, up and right /(1, -1)/, etc. -}
arrows :: SignalGen (Signal (Int, Int))
arrows = do
  up <- isDown UpKey
  left <- isDown LeftKey
  down <- isDown DownKey
  right <- isDown RightKey

  return $ arrows' <$> up <*> left <*> down <*> right

{-| A utility function for setting up a vector signal from directional keys. -}
arrows' :: Bool -> Bool -> Bool -> Bool -> (Int, Int)
arrows' u l d r = (-1 * fromEnum l + 1 * fromEnum r, -1 * fromEnum u + 1 * fromEnum d)

{-| Similar to the 'arrows' signal, but uses the popular WASD movement controls instead. -}
wasd :: SignalGen (Signal (Int, Int))
wasd = do
  w <- isDown WKey
  a <- isDown AKey
  s <- isDown SKey
  d <- isDown DKey

  return $ arrows' <$> w <*> a <*> s <*> d
