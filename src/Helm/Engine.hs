{-| Contains the core engine types and typeclasses. -}
module Helm.Engine (
  -- * Typeclasses
  Engine(..),
  -- * Types
  Cmd(..),
  GameConfig(..),
  Sub(..),
  MouseButton(..),
  Key(..)
) where

import Control.Monad.Trans.State (StateT)
import FRP.Elerea.Param (SignalGen, Signal)
import Linear.V2 (V2)

import Helm.Asset
import Helm.Graphics (Graphics)

class Engine e where
  loadImage  :: e -> IO Image
  loadSound  :: e -> IO Sound
  run        :: e -> GameConfig e m a -> IO ()

  windowSize :: e -> IO (V2 Int)
  runningTime :: e -> IO Double

  mouseMoveSignal :: e -> SignalGen e (Signal [V2 Int])
  mouseDownSignal :: e -> SignalGen e (Signal [(MouseButton, V2 Int)])
  mouseUpSignal :: e -> SignalGen e (Signal [(MouseButton, V2 Int)])
  mouseClickSignal :: e -> SignalGen e (Signal [(MouseButton, V2 Int)])
  keyboardDownSignal :: e -> SignalGen e (Signal [Key])
  keyboardUpSignal :: e -> SignalGen e (Signal [Key])
  keyboardPressSignal :: e -> SignalGen e (Signal [Key])
  windowResizeSignal :: e -> SignalGen e (Signal [V2 Int])

data Cmd e a = Cmd (StateT e IO [a])

data Sub e a = Sub (SignalGen e (Signal [a]))

{-| A data structure describing how to run a game. -}
data GameConfig e m a = GameConfig {
  initialFn          :: (m, Cmd e a),
  updateFn           :: m -> a -> (m, Cmd e a),
  subscriptionsFn    :: Sub e a,
  viewFn             :: m -> Graphics
}

-- Mostly matches the SDL structure, except we don't care about extra buttons (for now).
data MouseButton
  = LeftButton
  | MiddleButton
  | RightButton
  | X1Button
  | X2Button
  | UnknownButton
  deriving (Eq, Ord, Read, Show)

-- Matches the SDL structure, but with clearer constructor names.
data Key
  = ReturnKey
  | EscapeKey
  | BackspaceKey
  | TabKey
  | SpaceKey
  | ExclaimKey
  | QuoteDblKey
  | HashKey
  | PercentKey
  | DollarKey
  | AmpersandKey
  | QuoteKey
  | LeftParenKey
  | RightParenKey
  | AsteriskKey
  | PlusKey
  | CommaKey
  | MinusKey
  | PeriodKey
  | SlashKey
  | Number0Key
  | Number1Key
  | Number2Key
  | Number3Key
  | Number4Key
  | Number5Key
  | Number6Key
  | Number7Key
  | Number8Key
  | Number9Key
  | ColonKey
  | SemicolonKey
  | LessKey
  | EqualsKey
  | GreaterKey
  | QuestionKey
  | AtKey
  | LeftBracketKey
  | BackslashKey
  | RightBracketKey
  | CaretKey
  | UnderscoreKey
  | BackquoteKey
  | AKey
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
  | CapsLockKey
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
  | KeypadEqualsAS400Key
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
  | ThousandsSeparatorKey
  | DecimalSeparatorKey
  | CurrencyUnitKey
  | CurrencySubunitKey
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
  | KeypadXorKey
  | KeypadPowerKey
  | KeypadPercentKey
  | KeypadLessKey
  | KeypadGreaterKey
  | KeypadAmpersandKey
  | KeypadDblAmpersandKey
  | KeypadVerticalBarKey
  | KeypadDblVerticalBarKey
  | KeypadColonKey
  | KeypadHashKey
  | KeypadSpaceKey
  | KeypadAtKey
  | KeypadExclamKey
  | KeypadMemStoreKey
  | KeypadMemRecallKey
  | KeypadMemClearKey
  | KeypadMemAddKey
  | KeypadMemSubtractKey
  | KeypadMemMultiplyKey
  | KeypadMemDivideKey
  | KeypadPlusMinusKey
  | KeypadClearKey
  | KeypadClearEntryKey
  | KeypadBinaryKey
  | KeypadOctalKey
  | KeypadDecimalKey
  | KeypadHexadecimalKey
  | LeftCtrlKey
  | LeftShiftKey
  | LeftAltKey
  | LeftGUIKey
  | RightCtrlKey
  | RightShiftKey
  | RightAltKey
  | RightGUIKey
  | ModeKey
  | AudioNextKey
  | AudioPrevKey
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
  | UnknownKey
  deriving (Eq, Ord, Read, Show)
