module Event

import Record
import Record.JS

import IdrisScript

%default total

%include JavaScript "runtime.js"
%include Node "runtime.js"

export
Event : Type -> Type
Event t = JS_IO (Maybe t)

export
map : (a -> b) -> JS_IO (Maybe a) -> Event b
map f ev = ev >>= (pure . (map f))

export
combine : Event a -> Event a -> Event a
combine ev1 ev2 = do
    a1 <- ev1
    a2 <- ev2
    pure (a1 <|> a2)

partial
setupState : Ptr -> JsFn (Ptr -> JS_IO Ptr) -> JS_IO ()
setupState = jscall "setupState(%0, %1)" (Ptr -> JsFn (Ptr -> JS_IO Ptr) -> JS_IO ())

namespace JS
  export
  partial
  run : (initial: state) -> (state -> JS_IO state) -> JS_IO ()
  run init nextState = setupState (believe_me init) (MkJsFn nextStateJS)
    where
      nextStateJS : Ptr -> JS_IO Ptr
      nextStateJS old = do new <- nextState (believe_me old); pure (believe_me new)

  export
  partial
  fromEventReference : {auto ip : schemaImp sch FromJSD} -> JSRef -> Event (Record sch)
  fromEventReference {ip} {sch} eventRef = do
    obj <- jscall "%0.getValue()" (JSRef -> JS_IO JSRef) eventRef
    rec <- objectToRecordUnsafe {schema=[("set", Bool), ("value", JSRef)]} obj
    (if rec .. "set" then Functor.map Just (objectToRecordUnsafe {fp=ip} {schema=sch} (rec .. "value"))
                     else pure Nothing)

