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

public export
data Target = Node | Browser

export
singlifyNativeEvent : Target -> Ptr -> Ptr
singlifyNativeEvent Node ev = unsafePerformIO $ jscall
  """{
        on: function(evName, f) {
            %0.on(evName, function() {
                f(arguments)
            });
        },
        removeListener: function(evName, f) {
            %0.removeListener(evName, f)
        }
    }""" (Ptr -> JS_IO Ptr) ev
singlifyNativeEvent Browser ev = unsafePerformIO $ jscall
  """{
        addEventListener: function(evName, f) {
            %0.addEventListener(evName, function() {
                f(arguments)
            });
        },
        removeEventListener: function(evName, f) {
            %0.removeEventListener(evName, f)
        }
    }""" (Ptr -> JS_IO Ptr) ev


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

public export
record Program state msg where
  constructor MkProgram
  initalState : JS_IO state
  toEvent : state -> Event msg

  -- If Nothing is returned,
  -- the program stops
  nextState : state -> msg -> JS_IO (Maybe state)

mutual 

  export
  partial
  run : Program state msg -> JS_IO ()
  run (MkProgram sIO toEv nextState) = (do
    s <- sIO
    (let callback = callback s nextState toEv
    in let (MkEvent setCb) = toEv s
    in do rem <- setCb callback
          jscall "setRemove(%0)" (JsFn (() -> JS_IO ()) -> JS_IO ()) (MkJsFn (\() => rem))))

  partial
  callback : state -> (state -> msg -> JS_IO (Maybe state)) -> (state -> Event msg) -> msg -> JS_IO ()
  callback s nextState toEvent msg = (do
      jscall "removeCb()" (JS_IO ())
      maybe <- nextState s msg
      (case maybe of
          Just newState => run (MkProgram (pure newState) toEvent nextState)
          Nothing => pure ()))






