module Event.JS

import Event

import IdrisScript
import IdrisScript.Arrays

%access public export
%default total


withinProduce : String -> String
withinProduce s = "produceEvent(function(cb) { " ++ s ++ "})(%1)";

ObjectType : Type
ObjectType = JSValue (JSObject "Object")

arrayToMaybe : JSRef -> JS_IO (Maybe ObjectType)
arrayToMaybe ref = do
  packed <- pack ref 
  case packed of
    (JSObject "Array" ** arr) => do
          len <- length arr
          if len /= 0 then do
              hd <- IdrisScript.Arrays.head arr
              pure (case hd of
                  Just (JSObject "Object" ** obj) => Just obj
                  _ => Nothing)
          else (pure Nothing)
    _ => pure Nothing

mouseClick : ExtEvent (ObjectType)
mouseClick =
  let event = jscall
            (withinProduce "document.body.AddEventListener(\"click\", cb);")
            (Int -> JS_IO JSRef)
  in \t => (event (toIntNat t) >>= arrayToMaybe)




