module Event

import Record
import Record.JS

import IdrisScript

%include JavaScript "runtime.js"

public export
Event : Type -> Type
Event t = JS_IO (Maybe t)

export
combine : Event a -> Event a -> Event a
combine ev1 ev2 = do
    a1 <- ev1
    a2 <- ev2
    pure (a1 <|> a2)

setupState : Ptr -> JsFn (Ptr -> JS_IO Ptr) -> JS_IO ()
setupState = jscall "setupState(%0, %1)" (Ptr -> JsFn (Ptr -> JS_IO Ptr) -> JS_IO ())

export
run : (initial: state) -> (state -> JS_IO state) -> JS_IO ()
run init nextState = setupState (believe_me init) (MkJsFn nextStateJS)
  where
    nextStateJS : Ptr -> JS_IO Ptr
    nextStateJS old = do new <- nextState (believe_me old); pure (believe_me new)

namespace JS
  export


