module Event

%access public export
%default total

Time : Type
Time = Nat

Timeline : Type -> Type
Timeline t = Time -> t

Event : Type -> Type
Event t = Timeline (Maybe t)

ExtEvent : Type -> Type
ExtEvent t = Timeline (JS_IO (Maybe t))

Feedback : Type -> Type
Feedback state = state -> Timeline state

record Interface (state : Type) (res : Type) where
  constructor MkInterface
  feedback : Feedback state
  toResult : state -> res


