module Event

%access public export
%default total

Time : Type
Time = Nat

Timeline : Type -> Type
Timeline t = Time -> t

map : Timeline a -> (a -> b) -> Timeline b
map tl f = \t => f (tl t)

return : a -> Timeline a
return a t = a

Event : Type -> Type
Event t = Timeline (Maybe t)

combine : Event a -> Event a -> Event a
combine tl1 tl2 t = (tl1 t) <|> (tl2 t)

ExtEvent : Type -> Type
ExtEvent t = Timeline (JS_IO (Maybe t))

Feedback : Type -> Type
Feedback state = state -> Timeline state

foldp : Timeline a -> (st -> a -> st) -> Feedback st
foldp tl f state time = f state (tl time)

record Interface (state : Type) (res : Type) where
  constructor MkInterface
  feedback : Feedback state
  toResult : state -> res


