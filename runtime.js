
var clearFun = null;

function Event(setCallback) {

    var lastValueSet = false;
    var lastValue = {};

    setCallback(function(e) {
        lastValue = e;
        lastValueSet = true;

        clearFun = function() {
            lastValueSet = false;
        }

        trigger();
    });

    this.getValue = function() {
        return { 
            "set": lastValueSet,
            "value": lastValue
        };
    };
}

var state = null;
var triggerFun = null;

function setupState(state_, triggerFun_) {
    state = state_;
    triggerFun = triggerFun_;
}

function trigger() {
    state = triggerFun(state);
    clearFun();
}
