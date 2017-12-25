module Main

import Event
import IdrisScript
import Record
import Record.JS

State : Type
State = Int

initialState : State
initialState = 0

nextState : State -> JS_IO State
nextState st = do
    click <- mouseClick
    (let nextSt = if isJust click then st + 1
                                  else st
     in pure nextSt)

main : JS_IO ()
main = run initialState nextState
