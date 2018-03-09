
function addListenerBrowser(obj, evName, f) {
    obj.addEventListener(evName, f)

    return function() {
        obj.removeEventListener(evName, f)
    }
}

function addListenerNode(obj, evName, f) {
    obj.on(evName, f)

    return function() {
        obj.removeListener(evName, f)
    }
}

function singlifyEvent(obj, onKey, removeKey) {

    // We need to map the passed in listener in 'removeListener'
    // to the new function 'newFun' we create in 'on'
    var functionMap = []

    var newObj = {}

    newObj[onKey] = function(evName, f) {

        var newFun = function() { f(arguments) }
        functionMap.push([f, newFun]) 

        obj[onKey](evName, newFun)
    }

    newObj[removeKey] = function(evName, f) {
        
        for(var i = 0; i < functionMap.length; i++) {
            
            if(functionMap[i][0] == f) {
                obj[removeKey](evName, functionMap[i][1])
                functionMap.splice(i, 1)
                return
            }
        }
    }

    return newObj
}
