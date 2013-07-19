function PrologQuery(callbacks) {
    var that = this;
    // Private functions
    function processResponse(obj) {
        if (obj.event === 'halted') {
            if (callbacks.onhalted) {
                callbacks.onhalted.call(obj);
            }
        } else if (obj.event === 'error') {
            if (callbacks.onerror) {
                callbacks.onerror.call(obj)
            }
        } else if (obj.event === 'write') {
           if (callbacks.onoutput) {
                callbacks.onoutput.call(obj)
           }
           that.result();
        } else if (obj.event === 'prompt') {
            if (callbacks.onprompt) {
                callbacks.onprompt.call(obj)
            }
        } else if (obj.event === 'answer') {
            if (callbacks.onanswer) {
                callbacks.onanswer.call(obj)
            }
        } 
    };
    // Public functions
    this.consult_and_ask = function(program, query) {
        $.post('/prolog/consult', program, function(obj) {
            if (obj.success) {
                that.ask(query);
            } else {
                var msg = obj.message;
                callbacks.onerror.call({success: "error", msg:msg})
            }
        });
    }    
    this.ask = function(query) {
        $.get('/prolog/first?goal=' + encodeURIComponent(query), function(obj) {
            processResponse(obj);
        });
    }
    this.more = function() {
        $.get('/prolog/next', function(obj) {
            processResponse(obj);
        });
    }
    this.input = function(string) {
        $.get('/prolog/input?input=' + encodeURIComponent(string), function(obj) {
            processResponse(obj);
        });
    }
    this.stop = function() {
        $.get('/prolog/stop', function(obj) {
            processResponse(obj);
        });
    }
    this.abort = function() {
        $.get("/prolog/abort", function(obj) {
            processResponse(obj);            
        });
    }
    this.result = function() {
        $.get("/prolog/result", function(obj) {
            processResponse(obj);            
        });
    }
};

