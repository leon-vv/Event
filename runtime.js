
var remFun = null

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


function setRemove(f) {
    remFun = f
}

function removeCb() {
    remFun()
}
