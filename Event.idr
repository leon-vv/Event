module Event

import Record
import Record.JS

import IdrisScript

%default total

%include JavaScript "./Event/runtime.js"
%include Node "./Event/runtime.js"

public export
Event : Type -> Type
Event t = JS_IO (Maybe t)

export
map : (a -> b) -> Event a -> Event b
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
  private
  invertIO : Maybe (JS_IO a) -> JS_IO (Maybe a)
  invertIO (Just io) = do
    v <- io
    pure (Just v)
  invertIO Nothing = pure Nothing

  export
  mapIO : (a -> JS_IO b) -> Event a -> Event b
  mapIO f ev = ev >>= (invertIO . map f)
  
  export
  partial
  run : (initial: JS_IO state) -> (state -> JS_IO state) -> JS_IO ()
  run initIO nextState = (do 
        init <- initIO
        setupState (believe_me init) (MkJsFn nextStateJS))
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

  -- Given a string which evaluates to an event generator, return an IO action
  -- which will return an event
  %inline                     
  export
  partial
  fromGeneratorString : {auto ip : schemaImp sch FromJSD} -> String -> JS_IO (Event (Record sch))
  fromGeneratorString {ip} {sch} s = do
    eventRef <- jscall ("(" ++ s ++ ")()") (JS_IO JSRef)
    pure (fromEventReference {ip=ip} {sch=sch} eventRef)

  export
  partial
  fromGeneratorReference : {auto ip : schemaImp sch FromJSD} -> JSRef -> JS_IO (Event (Record sch))
  fromGeneratorReference {ip} {sch} ref = do
    eventRef <- jscall "%0.apply()" (JSRef -> JS_IO JSRef) ref
    pure (fromEventReference {ip=ip} {sch=sch} eventRef)

  
