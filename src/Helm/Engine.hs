-- | Contains the core engine types and classes.
module Helm.Engine (
  -- * Functions
  defaultConfig,
  -- * Typeclasses
  Engine(..),
  -- * Types
  Cmd(..),
  Game(..),
  GameLifecycle(..),
  GameConfig(..),
  FPSLimit(..),
  Sub(..),
  MouseButton(..),
  Key(..)
) where

import Control.Monad.Trans.State (StateT)

import FRP.Elerea.Param (SignalGen, Signal)
import Linear.V2 (V2)

import Helm.Graphics (Graphics)

-- | Represents a backend engine that can run a Helm game.
--
-- Helm separates the logic for running a game from the actual interaction with the user -
-- window management, event management (key presses, mouse presses, etc.) are all handled by a specific instance
-- of the engine typeclass. Meanwhile, the game loop and other core features are handled independently
-- by the Helm library itself.
class Engine e where
  -- | Renders a graphics element to the engine's game window.
  render :: e -> Graphics e -> IO ()

  -- | Ticks (or steps) the engine forward. Generally, an engine should use this method to
  -- gather any new input events from the underlying engine and sink them into the signals it provides below.
  -- Depending on the implementation of the engine, it might be necessary to do other things here too.
  tick :: e -> IO (Maybe e)

  -- | Cleans up all resources loaded by the engine. This will be run when the engine has stopped execution,
  -- hence it should do everything required to free any resources allocated by the engine.
  cleanup :: e -> IO ()

  -- | Get the game window size.
  windowSize :: e -> IO (V2 Int)

  -- | Get the current game running time.
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

-- | Represents a subscription to a stream of events captured from a user's interaction with the engine.
-- A subscription is best thought of as a collection of events over time - which is the nature of
-- functional reactive programming (the paradigm that Helm bases its concepts on).
-- Although Helm uses a departed version of the traditional FRP paradigm, it still shares some
-- concepts. An understanding of FRP will allow you to understand the library easily.
--
-- Functions throughout the Helm library that return a subscription will first let you map the data
-- related to the event you're subscribing to into another form (specifically, a game action).
-- These game actions are then sent to the update function of your game, i.e. the mapped
-- subscription specifies exactly how game events will interact with your game state.
--
-- Here the type variable e is an instance of the 'Engine' typeclass
-- and the variable a is the game action data type used by your game.
newtype Sub e a = Sub (SignalGen e (Signal [a]))

-- | Represents an IO-like monad with knowledge about the state of the game engine. Each command
-- contains a collection of game actions that will be applied to your game's update function to update
-- the game state. This is similar to a subscription in a way, with the difference being that
-- a command does not change over time, but rather is a lazy monad and hence contains a value
-- from the time of the execution. A good example of the usage of a command vs. a subscription is the game
-- window size - a command would allow you to map the current window size into an action, whereas
-- a subscription would let you subscribe to when the window is resized and then map that event into
-- a game action.
--
-- Just like a subscription, any function that returns a command in the Helm library will
-- first let you map from the original contained value to a game action. It's important
-- to note that commands are **evaluated on the main-thread** - which means they can
-- block the rendering process. *Don't execute long-running monads under commands!*
--
-- Here the type variable e is an instance of the 'Engine' typeclass
-- and the variable a is the game action data type used by your game.
newtype Cmd e a = Cmd (StateT e IO [a])

-- | Represents the configuration for a Helm game.
--
-- The type variable e refers to an instance of the 'Engine' class,
-- m refers to a game model type and a refers to a game action type.
data GameLifecycle e m a = GameLifecycle {
  -- | Called when the game starts up. The first value in the tuple
  -- is the initial game model state and then the second value is an optional
  -- command to be run. The command allows you to execute some monads
  -- during game startup and build up some game actions before the game begins
  -- rendering. A good example would be loading a game configuration file,
  -- parsing the file contents and then mapping the parsed contents
  -- to relevant game actions.
  --
  -- If no initial command is required, simply pass 'Cmd.none'
  -- for the second tuple value. Alternatively, if there are a number of commands
  -- to run, call 'Cmd.batch' to combine them into one.
  initialFn :: (m, Cmd e a),

  -- | Called whenever a game action is mapped from a command or subscription.
  -- This is where the actual implementation of a Helm game is provided.
  -- The function is given a game model and an action,
  -- and should produce the new game model state based off of the action.
  --
  -- The first tuple value is the new model state, and then the second
  -- is a command that can be run to produce more game actions.
  -- By having this command returnable here, you can run additional IO logic
  -- based off the game action, and produce more game actions from the result.
  --
  -- Be very careful with what commands you run in the game update function - most importantly,
  -- don't execute long-winding commands or it will block the rendering process!.
  -- Helm will try to intelligently queue recursive commands to prevent blocking rendering.
  -- However, having a game action that returns a specific command from the update function,
  -- which in turn is executed and returns that same game action (which will then in turn return the same command,
  -- and so on) is not recommend. The best way to return commands from the update function is to
  -- to hide them behind conditionals based off your game state, so that they're not run every update function.
  updateFn :: m -> a -> (m, Cmd e a),

  -- | The subscriptions for a game. All the input sources required
  -- to make the game work should be subscribed to and mapped to the relevant
  -- game action type variant.
  --
  -- If no subscriptions are required (i.e. no user input is required),
  -- pass 'Sub.none'. Alternatively, if multiple subscriptions are required
  -- use 'Sub.batch' to combine them.
  subscriptionsFn :: Sub e a,

  -- | Called when the engine is ready to render the game.
  -- The function is given the current state of the game model
  -- and should produce a graphics value to be rendered to the
  -- screen.
  --
  -- Do not rely on this function being called every game tick -
  -- the engine will figure out whether it needs to be called
  -- based off window exposure and whether or not the game model
  -- has changed since the last render.
  viewFn :: m -> Graphics e
}

-- | The default configuration for the helm engine. You should change the values where necessary.
defaultConfig :: GameConfig
defaultConfig = GameConfig { fpsLimit = Limited 120, updateLimit = 10 }

-- | Represents how often a render should be executed.
data FPSLimit
  = Unlimited
  | Limited Int

-- | Helm engine configuration. You should change the values where necessary.
data GameConfig = GameConfig
  { fpsLimit :: FPSLimit -- ^ Represents how often a render should be executed.
  , updateLimit :: Int -- ^ Represents the total amount of times an update should be called per single frame.
  }

-- | Represents the state of a game being run.
--
-- The type variable e refers to an instance of the 'Engine' class,
-- m refers to a game model type and a refers to a game action type.
data Game e m a = Game
  { gameConfig :: GameConfig -- ^ The configuration of the game engine, passed by a user.
  , gameLifecycle :: GameLifecycle e m a  -- ^ The set of game lifeycle functions, passed by a user.
  , gameModel  :: m                 -- ^ The current game model state.
  , dirtyModel :: Bool              -- ^ Whether or not the model has been changed and the game should be rerendered.
  , actionSmp  :: e -> IO [a]       -- ^ A feedable monad that returns actions from mapped subscriptions.
  , lastRender :: Double            -- ^ The last time when a render was called.
  , updateCount :: Int             -- ^ The total amount of time an update was called during stepping engine.
  }

-- | Represents a mouse button that can be pressed on a mouse.
data MouseButton
  = LeftButton
  | MiddleButton
  | RightButton
  | X1Button
  | X2Button
  | UnknownButton
  deriving (Eq, Ord, Read, Show)

-- | Represents a key that can be pressed on the keyboard.
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
  | KeypadNumber1Key
  | KeypadNumber2Key
  | KeypadNumber3Key
  | KeypadNumber4Key
  | KeypadNumber5Key
  | KeypadNumber6Key
  | KeypadNumber7Key
  | KeypadNumber8Key
  | KeypadNumber9Key
  | KeypadNumber0Key
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
