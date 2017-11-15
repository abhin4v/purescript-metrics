var Meter = require("metrics/metrics/meter");

var _new = function() {
  return new Meter();
};

var _markN = function(m, n) {
  return function() {
    m.mark(n);
  };
};

var fifteenMinuteRate = function(m) {
  return function() {
    return m.fifteenMinuteRate();
  };
};

var fiveMinuteRate = function(m) {
  return function() {
    return m.fiveMinuteRate();
  };
};

var oneMinuteRate = function(m) {
  return function() {
    return m.oneMinuteRate();
  };
};

var meanRate = function(m) {
  return function() {
    return m.meanRate();
  };
};

var count = function(m) {
  return function() {
    return m.count;
  };
};

exports._new = _new;
exports._markN = _markN;
exports.fifteenMinuteRate = fifteenMinuteRate;
exports.fiveMinuteRate = fiveMinuteRate;
exports.oneMinuteRate = oneMinuteRate;
exports.meanRate = meanRate;
exports.count = count;
