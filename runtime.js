
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

function singlifyEventBrowser(ev) {

    return {
        addEventListener: function(evName, f) {
            ev.addEventListener(evName, function() {
                f(arguments)
            });
        },
        removeEventListener: function(evName, f) {
            ev.removeEventListener(evName, f)
        }
    }
}

function singlifyEventNode(ev) {

    return {
        on: function(evName, f) {
            ev.on(evName, function() {
                f(arguments)
            });
        },
        removeListener: function(evName, f) {
            ev.removeListener(f)
        }
    }
}

var remFun = null

function setRemove(f) {
    remFun = f
}

function removeCb() {
    remFun()
}
