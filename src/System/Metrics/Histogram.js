var Histogram = require("metrics/metrics/histogram");

var clear = function(h) {
  return function() {
    h.clear();
  };
};

var _update = function(h, v) {
  return function() {
    h.update(v);
  };
};

var _percentiles = function(h, ptiles) {
  return function() {
    var scoresM = h.percentiles(ptiles);
    var scores = [];
    for (var i = 0; i < ptiles.length; i++) {
      scores.push(scoresM[ptiles[i]]);
    }
    if (scores[0]) {
      return scores;
    }
    return null;
  };
};

var _variance = function(h) {
  return function() {
    var v = h.variance();
    return isNaN(v) ? null : v;
  };
};
var _mean = function(h) { return function() { return h.mean(); }; };
var _stdDev = function(h) {
  return function() {
    var d = h.stdDev();
    return isNaN(d) ? null : d;
  };
};
var _min = function(h) { return function() { return h.min; }; };
var _max = function(h) { return function() { return h.max; }; };
var _sum = function(h) { return function() { return h.sum; }; };
var count = function(h) { return function() { return h.count; }; };

var _newWithExponentialDecaySampling = function(size, alpha) {
  return function() {
    return Histogram.createExponentialDecayHistogram(size, alpha);
  };
};

var newWithUniformSampling = function(size) {
  return function() {
    return Histogram.createUniformHistogram(size);
  };
};

exports.clear = clear;
exports._update = _update;
exports._percentiles = _percentiles;
exports._variance = _variance;
exports._mean = _mean;
exports._stdDev = _stdDev;
exports._min = _min;
exports._max = _max;
exports._sum = _sum;
exports.count = count;
exports._newWithExponentialDecaySampling = _newWithExponentialDecaySampling;
exports.newWithUniformSampling = newWithUniformSampling;
