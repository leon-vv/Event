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
  in ptrToEvent {to=Record clickSchema} Browser (pure ref) "click"

partial
toEvent : State -> Event Msg
toEvent _ = map (\rec => Click (rec .. "clientX")) mouseClick

partial
nextState : State -> Msg -> JS_IO (Maybe (State))
nextState n (Click _) = log n *> pure (Just (n + 1))

partial
program : Program State Msg
program = MkProgram (pure 0) toEvent nextState

partial
main : JS_IO ()
main = run program

