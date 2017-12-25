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
  mouseClick_ : JS_IO JSRef
  mouseClick_ = jscall "mouseClick.getValue()" (JS_IO JSRef)

  eventReferenceToRecord : {sc : Schema} -> {auto fp : schemaImp sc FromJSD} -> JSRef -> JS_IO (Maybe (Record sc))
  eventReferenceToRecord {fp} {sc} ref = do 
    rec <- objectToRecordUnsafe {schema=[("set", Bool), ("value", JSRef)]} ref
    (if rec .. "set" then map Just (objectToRecordUnsafe {schema=sc} (rec .. "value"))
                     else pure Nothing)

  export
  mouseClick : Event (Double, Double)
  mouseClick = do
      eventObj <- mouseClick_
      maybeRec <- eventReferenceToRecord {sc=[("clientX", Double), ("clientY", Double)]} eventObj
      (case maybeRec of
           Nothing => pure Nothing
           Just r => pure (Just (r .. "clientX", r .. "clientY")))



