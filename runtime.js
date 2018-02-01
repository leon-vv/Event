
var remFun = null

function addListener(obj, on, f) {
    obj.addEventListener(on, f)

    return function() {
        obj.removeEventListener(on, f)
    }
}

function setRemove(f) {
    remFun = f
}

function removeCb() {
    remFun()
}
