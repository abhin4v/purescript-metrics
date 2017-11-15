var Counter = require("metrics/metrics/counter");

var _new = function() {
  return new Counter();
};

var read = function(c) {
  return function() {
    return c.count;
  };
};

var _inc = function(c, v) {
  return function() {
    c.inc(v);
  };
};

var reset = function(c) {
  return function() {
    c.value = 0;
  }
}

exports._new = _new;
exports.read = read;
exports._inc = _inc;
exports.reset = reset;
