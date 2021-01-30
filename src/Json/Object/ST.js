"use strict";

var hasOwnProperty = {}.hasOwnProperty;

exports.new = function () {
  return {};
};

exports._poke = function (k, v, obj) {
  return function () {
    obj[k] = v;
    return obj;
  };
};

var copy = function (obj) {
  return function () {
    var r = {};
    for (var k in obj) {
      if (hasOwnProperty.call(obj, k)) {
        r[k] = obj[k];
      }
    }
    return r;
  };
};

exports.freeze = copy;

exports.thaw = copy;

exports.run = function (f) {
  return f();
};
