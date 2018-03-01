module Event

import Record

import FerryJS

import Data.IORef

%default total

%include JavaScript "event/runtime.js"
%include Node "event/runtime.js"

export
data Event : Type -> Type where
  MkEvent : ((a -> JS_IO ()) -> JS_IO (JS_IO ())) -> Event a
  EmptyEvent : Event a

export
emptyEvent : Event a
emptyEvent = EmptyEvent

export
Functor Event where
  map f ev = case ev of
    MkEvent setCb => MkEvent (\cb => setCb (\a => cb (f a)))
    EmptyEvent => EmptyEvent

export
combine : Event a -> Event a -> Event a
combine (MkEvent f1) (MkEvent f2) =
  MkEvent (\cb => do
    rem1 <- f1 cb
    rem2 <- f2 cb
    pure (rem1 *> rem2))
combine EmptyEvent ev = ev
combine ev EmptyEvent = ev

export
combineMany : List (Event a) -> Event a
combineMany = foldl combine EmptyEvent

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

public export
record Program where
  constructor MkProgram
  initialState : JS_IO state
  toEvent : state -> Event msg

  -- If Nothing is returned,
  -- the program stops
  nextState : state -> msg -> JS_IO (Maybe state)

combinePrograms : Program -> Program -> Program
combinePrograms (MkProgram {msg=m1} i1 t1 n1) (MkProgram {msg=m2} i2 t2 n2) =
  let init = (do
              i1_ <- i1
              i2_ <- i2
              pure (i1_, i2_))
  in let toEvent = (\(s1, s2) => combine (map Left (t1 s1)) (map Right (t2 s2)))
  in let nextState = (\(s1, s2), msg =>
                case msg of
                  Left m => map (map (`MkPair` s2)) $ n1 s1 m
                  Right m => map (map (MkPair s1)) $ n2 s2 m)
  in (MkProgram {msg=Either m1 m2} init toEvent nextState)

neutralProgram : Program
neutralProgram = MkProgram {msg=()} (pure ()) (const EmptyEvent) (const . const . pure $ Nothing)

Semigroup Program where
  (<+>) = combinePrograms

Monoid Program where
  neutral = neutralProgram

mutual 

  export
  partial
  run : Program -> JS_IO ()
  run (MkProgram sIO toEv nextState) = do
    s <- sIO
    calledReference <- newIORef' False
    (let callback = callback calledReference s nextState toEv
    in (case toEv s of
        MkEvent setCb => do
          rem <- setCb callback
          jscall "setRemove(%0)" (JsFn (() -> JS_IO ()) -> JS_IO ()) (MkJsFn (\() => rem))
        EmptyEvent => pure ()))


  -- All event listeners in JS are called, even if the first event listener
  -- removes the second event listener. The 'IORef Bool' makes sure the nextState
  -- function is only called once for an event if an object contains multiple listeners.
  partial
  callback : IORef Bool -> state -> (state -> msg -> JS_IO (Maybe state)) -> (state -> Event msg) -> msg -> JS_IO ()
  callback boolRef s nextState toEvent msg = do
      called <- readIORef' boolRef
      if called
        then pure ()
        else (do
          writeIORef' boolRef True 
          jscall "removeCb()" (JS_IO ())
          maybe <- nextState s msg
          case maybe of
            Just newState => run (MkProgram (pure newState) toEvent nextState)
            Nothing => pure ())






