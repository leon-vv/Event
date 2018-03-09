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


export
ptrToEventPtr : Target -> JS_IO Ptr -> String -> Event Ptr
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
ptrToEvent : {auto ti : ToIdris to} -> Target -> JS_IO Ptr -> String -> Event to
ptrToEvent {ti} t evRef name =
  map toIdrisUnsafe . ptrToEventPtr t evRef $ name

partial
%inline
stringExprToEvent : {ti : ToIdris to} -> Target -> String -> String -> Event to
stringExprToEvent {ti} t expr name =
  ptrToEvent {ti=ti} t (jscall expr (JS_IO Ptr)) name

export
Callback : Type -> Type
Callback msg = msg -> JS_IO ()

export
PendingCallback : Type
-- An IO action which removes the callback
PendingCallback = JS_IO ()

public export
record Program state msg where
  constructor MkProgram
  initalState : Callback msg -> JS_IO state

  -- If Nothing is returned,
  -- the program stops
  nextState : state -> msg -> JS_IO (Maybe state)

export
listen : Event msg -> Callback msg -> JS_IO PendingCallback
listen (MkEvent setCb) cb = setCb cb >>= pure

export
unlisten : PendingCallback -> JS_IO ()
unlisten = id 

mutual 

  export
  partial
  run : Program state msg -> JS_IO ()
  run (MkProgram {state} sIO nextState) = do
    stateRef <- newIORef' (Nothing {a=state})
    (let callback = callback stateRef nextState
     in do
       initialState <- sIO callback
       writeIORef' stateRef (Just initialState))

  -- All event listeners in JS are called, even if the first event listener
  -- removes the second event listener. The 'IORef Bool' makes sure the nextState
  -- function is only called once for an event if an object contains multiple listeners.
  partial
  callback : IORef (Maybe state) -> (state -> msg -> JS_IO (Maybe state)) -> msg -> JS_IO ()
  callback s nextState msg = do
      state <- readIORef' s 
      case state of
          -- Message comes in while the initial state has not yet been computed.
          Nothing => error "Event module: initial state has not been computed"
          Just currentState => (do
            maybeNextState <- nextState currentState msg
            case maybeNextState of
              Just nextState => writeIORef' s (Just nextState)
              Nothing => exit 0)






