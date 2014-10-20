{-| Contains signals that sample input from the keyboard. -}
module FRP.Helm.Keyboard (
  -- * Types
  Key(..),
  -- * Key State
  isDown, keysDown,
  -- * Directions
  arrows, wasd
) where

import Control.Applicative
import Data.List
import Foreign hiding (shift)
import Foreign.C.Types
import FRP.Elerea.Param hiding (Signal)
import FRP.Helm.Sample
import FRP.Helm.Signal

{-| The SDL bindings for Haskell don't wrap this, so we have to use the FFI ourselves. -}
foreign import ccall unsafe "SDL_GetKeyboardState" sdlGetKeyState :: Ptr CInt -> IO (Ptr Word8)

{-| A utility function for getting a list of SDL keys currently pressed.
    Based on <http://coderepos.org/share/browser/lang/haskell/nario/Main.hs?rev=22646#L49>. -}
getKeyState :: IO [Int]
getKeyState = alloca $ \numkeysPtr -> do
  keysPtr <- sdlGetKeyState numkeysPtr
  numkeys <- peek numkeysPtr

  (map fromIntegral . elemIndices 1) <$> peekArray (fromIntegral numkeys) keysPtr

{-| A data structure describing a physical key on a keyboard. -}
data Key
  = AKey
  | BKey
  | CKey
  | DKey
  | EKey
  | FKey
  | GKey
  | HKey
  | IKey
  | JKey
  | KKey
  | LKey
  | MKey
  | NKey
  | OKey
  | PKey
  | QKey
  | RKey
  | SKey
  | TKey
  | UKey
  | VKey
  | WKey
  | XKey
  | YKey
  | ZKey
  | Number1Key
  | Number2Key
  | Number3Key
  | Number4Key
  | Number5Key
  | Number6Key
  | Number7Key
  | Number8Key
  | Number9Key
  | Number0Key
  | ReturnKey
  | EscapeKey
  | BackspaceKey
  | TabKey
  | SpaceKey
  | MinusKey
  | EqualsKey
  | LeftBracketKey
  | RightBracketKey
  | BackslashKey
  | NonUSHashKey
  | SemicolonKey
  | ApostropheKey
  | GraveKey
  | CommaKey
  | PeriodKey
  | SlashKey
  | CapslockKey
  | F1Key
  | F2Key
  | F3Key
  | F4Key
  | F5Key
  | F6Key
  | F7Key
  | F8Key
  | F9Key
  | F10Key
  | F11Key
  | F12Key
  | PrintScreenKey
  | ScrollLockKey
  | PauseKey
  | InsertKey
  | HomeKey
  | PageUpKey
  | DeleteKey
  | EndKey
  | PageDownKey
  | RightKey
  | LeftKey
  | DownKey
  | UpKey
  | NumLockClearKey
  | KeypadDivideKey
  | KeypadMultiplyKey
  | KeypadMinusKey
  | KeypadPlusKey
  | KeypadEnterKey
  | Keypad1Key
  | Keypad2Key
  | Keypad3Key
  | Keypad4Key
  | Keypad5Key
  | Keypad6Key
  | Keypad7Key
  | Keypad8Key
  | Keypad9Key
  | Keypad0Key
  | KeypadPeriodKey
  | NonUSBackslashKey
  | ApplicationKey
  | PowerKey
  | KeypadEqualsKey
  | F13Key
  | F14Key
  | F15Key
  | F16Key
  | F17Key
  | F18Key
  | F19Key
  | F20Key
  | F21Key
  | F22Key
  | F23Key
  | F24Key
  | ExecuteKey
  | HelpKey
  | MenuKey
  | SelectKey
  | StopKey
  | AgainKey
  | UndoKey
  | CutKey
  | CopyKey
  | PasteKey
  | FindKey
  | MuteKey
  | VolumeUpKey
  | VolumeDownKey
  | KeypadCommaKey
  | KeyPadEqualsAs400Key
  | International1Key
  | International2Key
  | International3Key
  | International4Key
  | International5Key
  | International6Key
  | International7Key
  | International8Key
  | International9Key
  | Lang1Key
  | Lang2Key
  | Lang3Key
  | Lang4Key
  | Lang5Key
  | Lang6Key
  | Lang7Key
  | Lang8Key
  | Lang9Key
  | AltEraseKey
  | SysReqKey
  | CancelKey
  | ClearKey
  | PriorKey
  | Return2Key
  | SeparatorKey
  | OutKey
  | OperKey
  | ClearAgainKey
  | CrSelKey
  | ExSelKey
  | Keypad00Key
  | Keypad000Key
  | ThousandSeparatorKey
  | DecimalSeparatorKey
  | CurrencyUnitKey
  | CurrencySubUnitKey
  | KeypadLeftParenKey
  | KeypadRightParenKey
  | KeypadLeftBraceKey
  | KeypadRightBraceKey
  | KeypadTabKey
  | KeypadBackspaceKey
  | KeypadAKey
  | KeypadBKey
  | KeypadCKey
  | KeypadDKey
  | KeypadEKey
  | KeypadFKey
  | KeypadXORKey
  | KeypadPowerKey
  | KeypadPercentKey
  | KeypadLessKey
  | KeypadGreaterKey
  | KeypadAmpersandKey
  | KeypadDoubleAmpersandKey
  | KeypadVerticalBarKey
  | KeypadDoubleVerticalBarKey
  | KeypadColonKey
  | KeypadHashKey
  | KeypadSpaceKey
  | KeypadAtKey
  | KeypadExclamationKey
  | KeypadMemStoreKey
  | KeypadMemRecallKey
  | KeypadMemClearKey
  | KeypadMemAddKey
  | KeypadMemSubstractKey
  | KeypadMemMultiplyKey
  | KeypadMemDivideKey
  | KeypadPlusMinusKey
  | KeypadClearKey
  | KeypadClearEntryKey
  | KeypadBinaryKey
  | KeypadOctalKey
  | KeypadDecimalKey
  | KeypadHexadecimalKey
  | LeftControlKey
  | LeftShiftKey
  | LeftAltKey
  | LeftMetaKey
  | RightControlKey
  | RightShiftKey
  | RightAltKey
  | RightMetaKey
  | ModeKey
  | AudioNextKey
  | AudioPreviousKey
  | AudioStopKey
  | AudioPlayKey
  | AudioMuteKey
  | MediaSelectKey
  | WWWKey
  | MailKey
  | CalculatorKey
  | ComputerKey
  | ACSearchKey
  | ACHomeKey
  | ACBackKey
  | ACForwardKey
  | ACStopKey
  | ACRefreshKey
  | ACBookmarksKey
  | BrightnessDownKey
  | BrightnessUpKey
  | DisplaySwitchKey
  | KeyboardIllumToggleKey
  | KeyboardIllumDownKey
  | KeyboardIllumUpKey
  | EjectKey
  | SleepKey
  | App1Key
  | App2Key
  deriving (Show, Eq, Ord, Read)

{- All integer values of this enum are equivalent to the SDL scancode enum. -}
instance Enum Key where
  fromEnum AKey = 4
  fromEnum BKey = 5
  fromEnum CKey = 6
  fromEnum DKey = 7
  fromEnum EKey = 8
  fromEnum FKey = 9
  fromEnum GKey = 10
  fromEnum HKey = 11
  fromEnum IKey = 12
  fromEnum JKey = 13
  fromEnum KKey = 14
  fromEnum LKey = 15
  fromEnum MKey = 16
  fromEnum NKey = 17
  fromEnum OKey = 18
  fromEnum PKey = 19
  fromEnum QKey = 20
  fromEnum RKey = 21
  fromEnum SKey = 22
  fromEnum TKey = 23
  fromEnum UKey = 24
  fromEnum VKey = 25
  fromEnum WKey = 26
  fromEnum XKey = 27
  fromEnum YKey = 28
  fromEnum ZKey = 29
  fromEnum Number1Key = 30
  fromEnum Number2Key = 31
  fromEnum Number3Key = 32
  fromEnum Number4Key = 33
  fromEnum Number5Key = 34
  fromEnum Number6Key = 35
  fromEnum Number7Key = 36
  fromEnum Number8Key = 37
  fromEnum Number9Key = 38
  fromEnum Number0Key = 39
  fromEnum ReturnKey = 40
  fromEnum EscapeKey = 41
  fromEnum BackspaceKey = 42
  fromEnum TabKey = 43
  fromEnum SpaceKey = 44
  fromEnum MinusKey = 45
  fromEnum EqualsKey = 46
  fromEnum LeftBracketKey = 47
  fromEnum RightBracketKey = 48
  fromEnum BackslashKey = 49
  fromEnum NonUSHashKey = 50
  fromEnum SemicolonKey = 51
  fromEnum ApostropheKey = 52
  fromEnum GraveKey = 53
  fromEnum CommaKey = 54
  fromEnum PeriodKey = 55
  fromEnum SlashKey = 56
  fromEnum CapslockKey = 57
  fromEnum F1Key = 58
  fromEnum F2Key = 59
  fromEnum F3Key = 60
  fromEnum F4Key = 61
  fromEnum F5Key = 62
  fromEnum F6Key = 63
  fromEnum F7Key = 64
  fromEnum F8Key = 65
  fromEnum F9Key = 66
  fromEnum F10Key = 67
  fromEnum F11Key = 68
  fromEnum F12Key = 69
  fromEnum PrintScreenKey = 70
  fromEnum ScrollLockKey = 71
  fromEnum PauseKey = 72
  fromEnum InsertKey = 73
  fromEnum HomeKey = 74
  fromEnum PageUpKey = 75
  fromEnum DeleteKey = 76
  fromEnum EndKey = 77
  fromEnum PageDownKey = 78
  fromEnum RightKey = 79
  fromEnum LeftKey = 80
  fromEnum DownKey = 81
  fromEnum UpKey = 82
  fromEnum NumLockClearKey = 83
  fromEnum KeypadDivideKey = 84
  fromEnum KeypadMultiplyKey = 85
  fromEnum KeypadMinusKey = 86
  fromEnum KeypadPlusKey = 87
  fromEnum KeypadEnterKey = 88
  fromEnum Keypad1Key = 89
  fromEnum Keypad2Key = 90
  fromEnum Keypad3Key = 91
  fromEnum Keypad4Key = 92
  fromEnum Keypad5Key = 93
  fromEnum Keypad6Key = 94
  fromEnum Keypad7Key = 95
  fromEnum Keypad8Key = 96
  fromEnum Keypad9Key = 97
  fromEnum Keypad0Key = 98
  fromEnum KeypadPeriodKey = 99
  fromEnum NonUSBackslashKey = 100
  fromEnum ApplicationKey = 101
  fromEnum PowerKey = 102
  fromEnum KeypadEqualsKey = 103
  fromEnum F13Key = 104
  fromEnum F14Key = 105
  fromEnum F15Key = 106
  fromEnum F16Key = 107
  fromEnum F17Key = 108
  fromEnum F18Key = 109
  fromEnum F19Key = 110
  fromEnum F20Key = 111
  fromEnum F21Key = 112
  fromEnum F22Key = 113
  fromEnum F23Key = 114
  fromEnum F24Key = 115
  fromEnum ExecuteKey = 116
  fromEnum HelpKey = 117
  fromEnum MenuKey = 118
  fromEnum SelectKey = 119
  fromEnum StopKey = 120
  fromEnum AgainKey = 121
  fromEnum UndoKey = 122
  fromEnum CutKey = 123
  fromEnum CopyKey = 124
  fromEnum PasteKey = 125
  fromEnum FindKey = 126
  fromEnum MuteKey = 127
  fromEnum VolumeUpKey = 128
  fromEnum VolumeDownKey = 129
  fromEnum KeypadCommaKey = 133
  fromEnum KeyPadEqualsAs400Key = 134
  fromEnum International1Key = 135
  fromEnum International2Key = 136
  fromEnum International3Key = 137
  fromEnum International4Key = 138
  fromEnum International5Key = 139
  fromEnum International6Key = 140
  fromEnum International7Key = 141
  fromEnum International8Key = 142
  fromEnum International9Key = 143
  fromEnum Lang1Key = 144
  fromEnum Lang2Key = 145
  fromEnum Lang3Key = 146
  fromEnum Lang4Key = 147
  fromEnum Lang5Key = 148
  fromEnum Lang6Key = 149
  fromEnum Lang7Key = 150
  fromEnum Lang8Key = 151
  fromEnum Lang9Key = 152
  fromEnum AltEraseKey = 153
  fromEnum SysReqKey = 154
  fromEnum CancelKey = 155
  fromEnum ClearKey = 156
  fromEnum PriorKey = 157
  fromEnum Return2Key = 158
  fromEnum SeparatorKey = 159
  fromEnum OutKey = 160
  fromEnum OperKey = 161
  fromEnum ClearAgainKey = 162
  fromEnum CrSelKey = 163
  fromEnum ExSelKey = 164
  fromEnum Keypad00Key = 176
  fromEnum Keypad000Key = 177
  fromEnum ThousandSeparatorKey = 178
  fromEnum DecimalSeparatorKey = 179
  fromEnum CurrencyUnitKey = 180
  fromEnum CurrencySubUnitKey = 181
  fromEnum KeypadLeftParenKey = 182
  fromEnum KeypadRightParenKey = 183
  fromEnum KeypadLeftBraceKey = 184
  fromEnum KeypadRightBraceKey = 185
  fromEnum KeypadTabKey = 186
  fromEnum KeypadBackspaceKey = 187
  fromEnum KeypadAKey = 188
  fromEnum KeypadBKey = 189
  fromEnum KeypadCKey = 190
  fromEnum KeypadDKey = 191
  fromEnum KeypadEKey = 192
  fromEnum KeypadFKey = 193
  fromEnum KeypadXORKey = 194
  fromEnum KeypadPowerKey = 195
  fromEnum KeypadPercentKey = 196
  fromEnum KeypadLessKey = 197
  fromEnum KeypadGreaterKey = 198
  fromEnum KeypadAmpersandKey = 199
  fromEnum KeypadDoubleAmpersandKey = 200
  fromEnum KeypadVerticalBarKey = 201
  fromEnum KeypadDoubleVerticalBarKey = 202
  fromEnum KeypadColonKey = 203
  fromEnum KeypadHashKey = 204
  fromEnum KeypadSpaceKey = 205
  fromEnum KeypadAtKey = 206
  fromEnum KeypadExclamationKey = 207
  fromEnum KeypadMemStoreKey = 208
  fromEnum KeypadMemRecallKey = 209
  fromEnum KeypadMemClearKey = 210
  fromEnum KeypadMemAddKey = 211
  fromEnum KeypadMemSubstractKey = 212
  fromEnum KeypadMemMultiplyKey = 213
  fromEnum KeypadMemDivideKey = 214
  fromEnum KeypadPlusMinusKey = 215
  fromEnum KeypadClearKey = 216
  fromEnum KeypadClearEntryKey = 217
  fromEnum KeypadBinaryKey = 218
  fromEnum KeypadOctalKey = 219
  fromEnum KeypadDecimalKey = 220
  fromEnum KeypadHexadecimalKey = 221
  fromEnum LeftControlKey = 224
  fromEnum LeftShiftKey = 225
  fromEnum LeftAltKey = 226
  fromEnum LeftMetaKey = 227
  fromEnum RightControlKey = 228
  fromEnum RightShiftKey = 299
  fromEnum RightAltKey = 230
  fromEnum RightMetaKey = 231
  fromEnum ModeKey = 257
  fromEnum AudioNextKey = 258
  fromEnum AudioPreviousKey = 259
  fromEnum AudioStopKey = 260
  fromEnum AudioPlayKey = 261
  fromEnum AudioMuteKey = 262
  fromEnum MediaSelectKey = 263
  fromEnum WWWKey = 264
  fromEnum MailKey = 265
  fromEnum CalculatorKey = 266
  fromEnum ComputerKey = 267
  fromEnum ACSearchKey = 268
  fromEnum ACHomeKey = 269
  fromEnum ACBackKey = 270
  fromEnum ACForwardKey = 271
  fromEnum ACStopKey = 272
  fromEnum ACRefreshKey = 273
  fromEnum ACBookmarksKey = 274
  fromEnum BrightnessDownKey = 275
  fromEnum BrightnessUpKey = 276
  fromEnum DisplaySwitchKey = 277
  fromEnum KeyboardIllumToggleKey = 278
  fromEnum KeyboardIllumDownKey = 279
  fromEnum KeyboardIllumUpKey = 280
  fromEnum EjectKey = 281
  fromEnum SleepKey = 282
  fromEnum App1Key = 283
  fromEnum App2Key = 284

  toEnum 4 = AKey
  toEnum 5 = BKey
  toEnum 6 = CKey
  toEnum 7 = DKey
  toEnum 8 = EKey
  toEnum 9 = FKey
  toEnum 10 = GKey
  toEnum 11 = HKey
  toEnum 12 = IKey
  toEnum 13 = JKey
  toEnum 14 = KKey
  toEnum 15 = LKey
  toEnum 16 = MKey
  toEnum 17 = NKey
  toEnum 18 = OKey
  toEnum 19 = PKey
  toEnum 20 = QKey
  toEnum 21 = RKey
  toEnum 22 = SKey
  toEnum 23 = TKey
  toEnum 24 = UKey
  toEnum 25 = VKey
  toEnum 26 = WKey
  toEnum 27 = XKey
  toEnum 28 = YKey
  toEnum 29 = ZKey
  toEnum 30 = Number1Key
  toEnum 31 = Number2Key
  toEnum 32 = Number3Key
  toEnum 33 = Number4Key
  toEnum 34 = Number5Key
  toEnum 35 = Number6Key
  toEnum 36 = Number7Key
  toEnum 37 = Number8Key
  toEnum 38 = Number9Key
  toEnum 39 = Number0Key
  toEnum 40 = ReturnKey
  toEnum 41 = EscapeKey
  toEnum 42 = BackspaceKey
  toEnum 43 = TabKey
  toEnum 44 = SpaceKey
  toEnum 45 = MinusKey
  toEnum 46 = EqualsKey
  toEnum 47 = LeftBracketKey
  toEnum 48 = RightBracketKey
  toEnum 49 = BackslashKey
  toEnum 50 = NonUSHashKey
  toEnum 51 = SemicolonKey
  toEnum 52 = ApostropheKey
  toEnum 53 = GraveKey
  toEnum 54 = CommaKey
  toEnum 55 = PeriodKey
  toEnum 56 = SlashKey
  toEnum 57 = CapslockKey
  toEnum 58 = F1Key
  toEnum 59 = F2Key
  toEnum 60 = F3Key
  toEnum 61 = F4Key
  toEnum 62 = F5Key
  toEnum 63 = F6Key
  toEnum 64 = F7Key
  toEnum 65 = F8Key
  toEnum 66 = F9Key
  toEnum 67 = F10Key
  toEnum 68 = F11Key
  toEnum 69 = F12Key
  toEnum 70 = PrintScreenKey
  toEnum 71 = ScrollLockKey
  toEnum 72 = PauseKey
  toEnum 73 = InsertKey
  toEnum 74 = HomeKey
  toEnum 75 = PageUpKey
  toEnum 76 = DeleteKey
  toEnum 77 = EndKey
  toEnum 78 = PageDownKey
  toEnum 79 = RightKey
  toEnum 80 = LeftKey
  toEnum 81 = DownKey
  toEnum 82 = UpKey
  toEnum 83 = NumLockClearKey
  toEnum 84 = KeypadDivideKey
  toEnum 85 = KeypadMultiplyKey
  toEnum 86 = KeypadMinusKey
  toEnum 87 = KeypadPlusKey
  toEnum 88 = KeypadEnterKey
  toEnum 89 = Keypad1Key
  toEnum 90 = Keypad2Key
  toEnum 91 = Keypad3Key
  toEnum 92 = Keypad4Key
  toEnum 93 = Keypad5Key
  toEnum 94 = Keypad6Key
  toEnum 95 = Keypad7Key
  toEnum 96 = Keypad8Key
  toEnum 97 = Keypad9Key
  toEnum 98 = Keypad0Key
  toEnum 99 = KeypadPeriodKey
  toEnum 100 = NonUSBackslashKey
  toEnum 101 = ApplicationKey
  toEnum 102 = PowerKey
  toEnum 103 = KeypadEqualsKey
  toEnum 104 = F13Key
  toEnum 105 = F14Key
  toEnum 106 = F15Key
  toEnum 107 = F16Key
  toEnum 108 = F17Key
  toEnum 109 = F18Key
  toEnum 110 = F19Key
  toEnum 111 = F20Key
  toEnum 112 = F21Key
  toEnum 113 = F22Key
  toEnum 114 = F23Key
  toEnum 115 = F24Key
  toEnum 116 = ExecuteKey
  toEnum 117 = HelpKey
  toEnum 118 = MenuKey
  toEnum 119 = SelectKey
  toEnum 120 = StopKey
  toEnum 121 = AgainKey
  toEnum 122 = UndoKey
  toEnum 123 = CutKey
  toEnum 124 = CopyKey
  toEnum 125 = PasteKey
  toEnum 126 = FindKey
  toEnum 127 = MuteKey
  toEnum 128 = VolumeUpKey
  toEnum 129 = VolumeDownKey
  toEnum 133 = KeypadCommaKey
  toEnum 134 = KeyPadEqualsAs400Key
  toEnum 135 = International1Key
  toEnum 136 = International2Key
  toEnum 137 = International3Key
  toEnum 138 = International4Key
  toEnum 139 = International5Key
  toEnum 140 = International6Key
  toEnum 141 = International7Key
  toEnum 142 = International8Key
  toEnum 143 = International9Key
  toEnum 144 = Lang1Key
  toEnum 145 = Lang2Key
  toEnum 146 = Lang3Key
  toEnum 147 = Lang4Key
  toEnum 148 = Lang5Key
  toEnum 149 = Lang6Key
  toEnum 150 = Lang7Key
  toEnum 151 = Lang8Key
  toEnum 152 = Lang9Key
  toEnum 153 = AltEraseKey
  toEnum 154 = SysReqKey
  toEnum 155 = CancelKey
  toEnum 156 = ClearKey
  toEnum 157 = PriorKey
  toEnum 158 = Return2Key
  toEnum 159 = SeparatorKey
  toEnum 160 = OutKey
  toEnum 161 = OperKey
  toEnum 162 = ClearAgainKey
  toEnum 163 = CrSelKey
  toEnum 164 = ExSelKey
  toEnum 176 = Keypad00Key
  toEnum 177 = Keypad000Key
  toEnum 178 = ThousandSeparatorKey
  toEnum 179 = DecimalSeparatorKey
  toEnum 180 = CurrencyUnitKey
  toEnum 181 = CurrencySubUnitKey
  toEnum 182 = KeypadLeftParenKey
  toEnum 183 = KeypadRightParenKey
  toEnum 184 = KeypadLeftBraceKey
  toEnum 185 = KeypadRightBraceKey
  toEnum 186 = KeypadTabKey
  toEnum 187 = KeypadBackspaceKey
  toEnum 188 = KeypadAKey
  toEnum 189 = KeypadBKey
  toEnum 190 = KeypadCKey
  toEnum 191 = KeypadDKey
  toEnum 192 = KeypadEKey
  toEnum 193 = KeypadFKey
  toEnum 194 = KeypadXORKey
  toEnum 195 = KeypadPowerKey
  toEnum 196 = KeypadPercentKey
  toEnum 197 = KeypadLessKey
  toEnum 198 = KeypadGreaterKey
  toEnum 199 = KeypadAmpersandKey
  toEnum 200 = KeypadDoubleAmpersandKey
  toEnum 201 = KeypadVerticalBarKey
  toEnum 202 = KeypadDoubleVerticalBarKey
  toEnum 203 = KeypadColonKey
  toEnum 204 = KeypadHashKey
  toEnum 205 = KeypadSpaceKey
  toEnum 206 = KeypadAtKey
  toEnum 207 = KeypadExclamationKey
  toEnum 208 = KeypadMemStoreKey
  toEnum 209 = KeypadMemRecallKey
  toEnum 210 = KeypadMemClearKey
  toEnum 211 = KeypadMemAddKey
  toEnum 212 = KeypadMemSubstractKey
  toEnum 213 = KeypadMemMultiplyKey
  toEnum 214 = KeypadMemDivideKey
  toEnum 215 = KeypadPlusMinusKey
  toEnum 216 = KeypadClearKey
  toEnum 217 = KeypadClearEntryKey
  toEnum 218 = KeypadBinaryKey
  toEnum 219 = KeypadOctalKey
  toEnum 220 = KeypadDecimalKey
  toEnum 221 = KeypadHexadecimalKey
  toEnum 224 = LeftControlKey
  toEnum 225 = LeftShiftKey
  toEnum 226 = LeftAltKey
  toEnum 227 = LeftMetaKey
  toEnum 228 = RightControlKey
  toEnum 299 = RightShiftKey
  toEnum 230 = RightAltKey
  toEnum 231 = RightMetaKey
  toEnum 257 = ModeKey
  toEnum 258 = AudioNextKey
  toEnum 259 = AudioPreviousKey
  toEnum 260 = AudioStopKey
  toEnum 261 = AudioPlayKey
  toEnum 262 = AudioMuteKey
  toEnum 263 = MediaSelectKey
  toEnum 264 = WWWKey
  toEnum 265 = MailKey
  toEnum 266 = CalculatorKey
  toEnum 267 = ComputerKey
  toEnum 268 = ACSearchKey
  toEnum 269 = ACHomeKey
  toEnum 270 = ACBackKey
  toEnum 271 = ACForwardKey
  toEnum 272 = ACStopKey
  toEnum 273 = ACRefreshKey
  toEnum 274 = ACBookmarksKey
  toEnum 275 = BrightnessDownKey
  toEnum 276 = BrightnessUpKey
  toEnum 277 = DisplaySwitchKey
  toEnum 278 = KeyboardIllumToggleKey
  toEnum 279 = KeyboardIllumDownKey
  toEnum 280 = KeyboardIllumUpKey
  toEnum 281 = EjectKey
  toEnum 282 = SleepKey
  toEnum 283 = App1Key
  toEnum 284 = App2Key
  toEnum _ = error "FRP.Helm.Keyboard.Key.toEnum: bad argument"

{-| Whether a key is pressed. -}
isDown :: Key -> Signal Bool
isDown k = Signal $ getDown >>= transfer (pure True) update
  where getDown = effectful $ elem (fromEnum k) <$> getKeyState

{-| A list of keys that are currently being pressed. -}
keysDown :: Signal [Key]
keysDown = Signal $ getDown >>= transfer (pure []) update
  where getDown = effectful $ map toEnum <$> getKeyState

{-| A directional tuple combined from the arrow keys. When none of the arrow keys
    are being pressed this signal samples to /(0, 0)/, otherwise it samples to a
    direction based on which keys are pressed. For example, pressing the left key
    results in /(-1, 0)/, the down key /(0, 1)/, up and right /(1, -1)/, etc. -}
arrows :: Signal (Int, Int)
arrows =  arrows' <$> up <*> left <*> down <*> right
  where up    = isDown UpKey
        left  = isDown LeftKey
        down  = isDown DownKey
        right = isDown RightKey


{-| A utility function for setting up a vector signal from directional keys. -}
arrows' :: Bool -> Bool -> Bool -> Bool -> (Int, Int)
arrows' u l d r = (-1 * fromEnum l + 1 * fromEnum r, -1 * fromEnum u + 1 * fromEnum d)

{-| Similar to the 'arrows' signal, but uses the popular WASD movement controls instead. -}
wasd :: Signal (Int, Int)
wasd = arrows' <$> w <*> a <*> s <*> d
  where w = isDown WKey
        a = isDown AKey
        s = isDown SKey
        d = isDown DKey

