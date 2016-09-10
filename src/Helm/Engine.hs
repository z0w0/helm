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

-- | The engine typeclass is implemented by structures that can run a Helm game.
-- Essentially, Helm separates the logic for running a game from the actual interaction with the user -
-- window management, event management (key presses, mouse presses, etc.) are all handled by an instance
-- of the engine typeclass. Meanwhile, the game loop is handled independently by the Helm library itself.
class Engine e where
  loadImage  :: e -> IO (Image a)

  -- | Renders a graphics element to the engine's window.
  render     :: e -> Graphics -> IO ()

  -- | Ticks (or steps) the engine forward. Generally, an engine should use this method to
  -- gather any new input events from the underlying engine and sink them into the signals it provides below.
  -- However, it's foreseeable that the engine might need to do other things during a tick - depending
  -- on the implementation.
  tick       :: e -> IO (Maybe e)

  -- | Cleans up all resources loaded by the engine. This will be run when the engine has stopped running,
  -- hence it should do everything required to free any resources allocated by the engine.
  cleanup    :: e -> IO ()

  -- | A monad containing the current game widnow size.
  windowSize :: e -> IO (V2 Int)

  -- | A monad containing the current game running time.
  runningTime :: e -> IO Double

  -- | The mouse move signal, with events provided by the engine.
  mouseMoveSignal :: e -> SignalGen e (Signal [V2 Int])

   -- | The mouse down signal, with events provided by the engine.
  mouseDownSignal :: e -> SignalGen e (Signal [(MouseButton, V2 Int)])

   -- | The mouse up signal, with events provided by the engine.
  mouseUpSignal :: e -> SignalGen e (Signal [(MouseButton, V2 Int)])

   -- | The mouse click signal, with events provided by the engine.
  mouseClickSignal :: e -> SignalGen e (Signal [(MouseButton, V2 Int)])

   -- | The keyboard down signal, with events provided by the engine.
  keyboardDownSignal :: e -> SignalGen e (Signal [Key])

   -- | The keyboard up signal, with events provided by the engine.
  keyboardUpSignal :: e -> SignalGen e (Signal [Key])

   -- | The keyboard press signal, with events provided by the engine.
  keyboardPressSignal :: e -> SignalGen e (Signal [Key])

   -- | The window resize signal, with events provided by the engine.
  windowResizeSignal :: e -> SignalGen e (Signal [V2 Int])

-- | A subscription is a way to subscribe to a stream of events from a user's interaction with the engine.
-- A subscription is best thought of as a collection of events over time - which is the nature of
-- purely-functional reactive programming (the paradigm that Helm bases it's concepts on). Although Helm uses a departed version
-- of the traditional FRP paradigm, it still follows the concept closely and hence an understanding of FRP
-- will allow you to understnad the library easily.
--
-- Functions throughout the Helm library that return a subscription will first let you map the data
-- related to the event you're subscribing to into another form (specifically, a game action).
-- These game actions are then sent to the update function of your game, i.e. the mapped
-- subscription specifies exactly how game events will interact with your game state (a.k.a. model).
--
-- Here the type variable e is an instance of the 'Engine' typeclass (although that is not
-- enforced here), and the variable a is the game action type used by your game.
data Sub e a = Sub (SignalGen e (Signal [a]))

-- | A command is simply an IO monad with knowledge about the state of the game engine. A command
-- contains a collection of game actions that will be applied to your game's update function to update
-- the game state. A command is similar to a subscription in a way, with the difference being that
-- a command does not change over time, but rather is a regular monad and hence contains a value that
-- from the time of the execution. A good example of the usage of a command vs. a subscription is the game
-- window size - a command would allow you to map the current window size into an action, whereas
-- a subscription would let you subscribe to when the window is resized and then map that event into
-- a game action.
--
-- Just like a subscription, any function that returns a command in the Helm library will
-- first let you map from the original contained value to a game action.
--
-- Here the type variable e is an instance of the 'Engine' typeclass (although that is not
-- enforced here), and the variable a is the game action type used by your game.
data Cmd e a = Cmd (StateT e IO [a])

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
