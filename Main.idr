module Main

import Event
import IdrisScript
import Record
import Record.JS

State : Type
State = Int

initialState : JS_IO State
initialState = pure 0

nextState : State -> JS_IO State
nextState st = do
    click <- mouseClick
    (let nextSt = if isJust click then st + 1
                                  else st
     in pure nextSt)

main : JS_IO ()
main = run initialState nextState
