module Event

import Record

import FerryJS

%default total

%include JavaScript "event/runtime.js"
%include Node "event/runtime.js"

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
  fromEventReference : {auto fjs : FromJS (Record sch)} -> Ptr -> Event (Record sch)
  fromEventReference {fjs=FromJSFun f} {sch} eventRef = do
    obj <- jscall "%0.getValue()" (Ptr -> JS_IO Ptr) eventRef
    pure $ (let rec = fromJS {to=Record [("set", Bool), ("value", Ptr)]} obj
            in if rec .. "set" then Just (f (rec .. "value"))
                            else Nothing)

  -- Given a string which evaluates to an event generator, return an IO action
  -- which will return an event
  %inline                     
  export
  partial
  fromGeneratorString : {auto fjs: FromJS (Record sch)} -> String -> JS_IO (Event (Record sch))
  fromGeneratorString {fjs} {sch} s = do
    eventRef <- jscall ("(" ++ s ++ ")()") (JS_IO Ptr)
    pure (fromEventReference {fjs=fjs} {sch=sch} eventRef)

  export
  partial
  fromGeneratorReference : {auto fjs: FromJS (Record sch)} -> Ptr -> JS_IO (Event (Record sch))
  fromGeneratorReference {fjs} {sch} ref = do
    eventRef <- jscall "%0.apply()" (Ptr -> JS_IO Ptr) ref
    pure (fromEventReference {fjs=fjs} {sch=sch} eventRef)

  
