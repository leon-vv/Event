module Event

import Record

import FerryJS

import Debug.Error

%default total

%include JavaScript "event/runtime.js"
%include Node "event/runtime.js"

export
data Event : Type -> Type where
  MkEvent : ((a -> JS_IO ()) -> JS_IO (JS_IO ())) -> Event a

export
Functor Event where
  map f (MkEvent setCb) =
    MkEvent (\cb => setCb (\a => cb (f a)))

export
combine : Event a -> Event a -> Event a
combine (MkEvent f1) (MkEvent f2) =
  MkEvent (\cb => do
    rem1 <- f1 cb
    rem2 <- f2 cb
    pure (rem1 *> rem2))

export
fromNativeEventToPtrEvent : Ptr -> String -> Event Ptr
fromNativeEventToPtrEvent evRef name = 
  MkEvent (\cb =>
        let jsFn = MkJsFn cb
        in assert_total $ do
          rem <- jscall
            "addListener(%0, %1, %2)"
            (Ptr -> String -> JsFn (Ptr -> JS_IO ()) -> JS_IO Ptr)
            evRef
            name
            jsFn
          pure (believe_me {b=JS_IO ()} rem))

export
partial
fromNativeEvent : {auto fjs : FromJS to} -> Ptr -> String -> Event to
fromNativeEvent {fjs} evRef name =
  map fromJSUnsafe . fromNativeEventToPtrEvent evRef $ name

partial
%inline
fromNativeEventString : {fjs : FromJS to} -> String -> String -> Event to
fromNativeEventString {fjs} expr name =
  let ptr = unsafePerformIO $ jscall expr (JS_IO Ptr)
  in fromNativeEvent {fjs=fjs} ptr name

public export
record Program state msg where
  constructor MkProgram
  initalState : state
  initalEvent : Event msg
  nextState : state -> msg -> JS_IO . Maybe $ (state, Event msg)

export
partial
run : Program state msg -> JS_IO ()
run (MkProgram s (MkEvent setCb) next) =
  let callback = \msg => (do
                         jscall "removeCb()" (JS_IO ())
                         maybe <- next s msg
                         (case maybe of
                            Just (newState, newEv) => run (MkProgram newState newEv next)
                            Nothing => pure ()))
  in do 
    rem <- setCb callback
    jscall "setRemove(%0)" (JsFn (() -> JS_IO ()) -> JS_IO ()) (MkJsFn (\() => rem))






