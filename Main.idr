import Event

import FerryJS
import Record

%default total

State : Type
State = Nat

data Msg = Click Int

log : Nat -> JS_IO ()
log n = jscall "console.log(%0)" (Int -> JS_IO ()) (cast n)

clickSchema : Schema
clickSchema = [("clientX", Int)]

partial
mouseClick : Event (Record Main.clickSchema)
mouseClick = 
  let ref = unsafePerformIO $ jscall "document.body" (JS_IO Ptr)
  in fromNativeEvent {to=Record clickSchema} ref "click"

partial
stateEvent : Event Msg
stateEvent = map (\rec => Click (rec .. "clientX")) mouseClick

partial
nextState : State -> Msg -> JS_IO (Maybe (State, Event Msg))
nextState n (Click _) = log n *> pure (Just (n + 1, stateEvent))

partial
program : Program State Msg
program = MkProgram 0 stateEvent nextState

partial
main : JS_IO ()
main = run program
