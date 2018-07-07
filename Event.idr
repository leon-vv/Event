module Event

import Record

import FerryJS
import FerryJS.Util

import System as S
import Data.IORef
import Debug.Error

%default total

-- To use the error function
%language ElabReflection

%include JavaScript "event/runtime.js"
%include Node "event/runtime.js"

public export
data EventType =
    Single
  | Multiple

export
Callback : Type -> Type
Callback msg = msg -> JS_IO ()

-- An Event is basically something that can be listened for.
-- Given a callback function the Event returns an 'outer' JS_IO
-- that registers the callback. The inner 'JS_IO' can be used
-- to remove the callback again.
export
data Event : EventType -> Type -> Type where
  MkEvent : (Callback msg -> JS_IO (JS_IO ())) -> Event type msg

emptyEvent : a -> Event Single a
emptyEvent a = MkEvent (\cb => do cb a; pure (pure ()))

bindEvent : Event Single a -> (a -> Event Single b) -> Event Single b
bindEvent (MkEvent setCbA) f = MkEvent $ \cb =>
  do
    ioRef <- newIORef' (Nothing {a=JS_IO ()})
    remA <- setCbA (\a =>
              let (MkEvent setCbB) = f a
              in do 
                remB <- setCbB cb
                writeIORef' ioRef (Just remB))
    pure (do
          remA
          maybeRem <- readIORef' ioRef
          (case maybeRem of
            Nothing => pure ()
            Just remB => remB))

-- Todo: also unbind inner event
joinEvent : Event Single (Event Single a) -> Event Single a
joinEvent (MkEvent setCb1) = MkEvent $ \cb => setCb1 (\(MkEvent setCb2) => ignore $ setCb2 cb)


-- It's not known which of the events fires first. So we cannot
-- set a callback while in another callback as done for the functions above.
-- The solution is to manage a bit of state using an IORef.
applyEvent : Event Single (a -> b) -> Event Single a -> Event Single b
applyEvent {a} {b} (MkEvent setCb1) (MkEvent setCb2) =
  MkEvent $ \cb => do
      ioRef <- newIORef' {ffi=FFI_JS} (Nothing {a=Either (a -> b) a})
      rem1 <- setCb1 (\f => do
              maybeVal <- readIORef' ioRef
              case maybeVal of
                Nothing => writeIORef' ioRef (Just $ Left f)
                Just (Right val) => cb (f val)
                Just (Left _) => error "Event: single event produced more often")
      rem2 <- setCb2 (\val => do
              maybeFun <- readIORef' ioRef
              case maybeFun of
                Nothing => writeIORef' ioRef (Just $ Right val)
                Just (Left f) => cb (f val)
                Just (Right _) => error "Event: single event produced more often")
      pure (rem1 *> rem2)


export
Functor (Event t) where
  map f (MkEvent setCb) =
    MkEvent (\cb => setCb (\a => cb (f a)))

export
Applicative (Event Single) where
  pure = emptyEvent
  (<*>) = applyEvent

export
Monad (Event Single) where
  (>>=) = bindEvent
  join = joinEvent


export
combine : Event t a -> Event t a -> Event t a
combine (MkEvent f1) (MkEvent f2) =
  MkEvent (\cb => do
    rem1 <- f1 cb
    rem2 <- f2 cb
    pure (rem1 *> rem2))

public export
data Target = Node | Browser

export
singlifyNativeEvent : Target -> Ptr -> Ptr
singlifyNativeEvent Node ev = unsafePerformIO $ jscall
    "singlifyEvent(%0, %1, %2)"
    (Ptr -> String -> String -> JS_IO Ptr)
    ev
    "on"
    "removeListener"
singlifyNativeEvent Browser ev = unsafePerformIO $ jscall
    "singlifyEvent(%0, %1, %2)"
    (Ptr -> String -> String -> JS_IO Ptr)
    ev
    "addEventListener"
    "removeEventListener"


-- Note: by using believe_me we assume that the representation of JS_IO ()
-- is a normal javascript function. This does not have to be the case.
-- Todo: remove this assumption by holding on to a Ptr and calling the
-- 'remove' function explicitly.
export
ptrToEventPtr : Target -> JS_IO Ptr -> String -> Event type Ptr
ptrToEventPtr t evRefIO name = 
  MkEvent (\cb => assert_total $ case t of
          Node => do
              evRef <- evRefIO
              rem <- jscall "addListenerNode(%0, %1, %2)"
                              (Ptr -> String -> JsFn (Ptr -> JS_IO ()) -> JS_IO Ptr)
                              evRef
                              name
                              (MkJsFn cb)
              pure (believe_me rem)
          Browser => do
              evRef <- evRefIO
              rem <- jscall "addListenerBrowser(%0, %1, %2)"
                            (Ptr -> String -> JsFn (Ptr -> JS_IO ()) -> JS_IO Ptr)
                            evRef
                            name
                            (MkJsFn cb)
              pure (believe_me rem))

export
partial
ptrToEvent : {auto ti : ToIdris to} -> Target -> JS_IO Ptr -> String -> Event type to
ptrToEvent {ti} t evRef name =
  map toIdrisUnsafe . ptrToEventPtr t evRef $ name

partial
%inline
stringExprToEvent : {ti : ToIdris to} -> Target -> String -> String -> Event type to
stringExprToEvent {ti} t expr name =
  ptrToEvent {ti=ti} t (jscall expr (JS_IO Ptr)) name

export
PendingCallback : Type
-- An IO action which removes the callback
PendingCallback = JS_IO ()

public export
data ProgramMsg state msg =
  ProgramStart (Callback msg)
  | ProgramNext state msg

public export
Program : Type -> Type -> Type
Program state msg = ProgramMsg state msg -> JS_IO (Maybe state)

export
listen : Event type msg -> Callback msg -> JS_IO PendingCallback
listen (MkEvent setCb) cb = setCb cb >>= pure

-- Wait for the event and run the JS_IO.
-- I'm not sure this is a valid use case of unsafePerformIO.
export
execute : Event type (JS_IO a) -> JS_IO PendingCallback
execute ev = listen (unsafePerformIO <$> ev) (const $ pure ())

export
unlisten : PendingCallback -> JS_IO ()
unlisten = id 

mutual 

  export
  partial
  run : Program state msg -> JS_IO ()
  run {state} computeState = do
    stateRef <- newIORef' (Nothing {a=state})
    (let callback = callback stateRef computeState
     in do
        initialState <- computeState (ProgramStart callback)
        case initialState of
             Nothing => pure ()
             Just st => writeIORef' stateRef (Just st))

  -- All event listeners in JS are called, even if the first event listener
  -- removes the second event listener. The 'IORef Bool' makes sure the computeState
  -- function is only called once for an event if an object contains multiple listeners.
  partial
  callback : IORef (Maybe state) -> (ProgramMsg state msg -> JS_IO (Maybe state)) -> Callback msg
  callback s computeState msg = do
      currState <- readIORef' s 
      (case currState of
          -- Message comes in while the initial state has not yet been computed.
        Nothing => error "Event module: initial state has not been computed"
        Just currState' => (do
            maybeNextState <- computeState (ProgramNext currState' msg)
            case maybeNextState of
              Just newState => writeIORef' s (Just newState)
              Nothing => exit 0))






