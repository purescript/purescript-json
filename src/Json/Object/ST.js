"use strict";

var hasOwnProperty = {}.hasOwnProperty;

exports._new = function () {
  return {};
};

exports._peek = function (just, nothing, k, m) {
  return function () {
    return hasOwnProperty.call(m, k) ? just(m[k]) : nothing;
  };
};

exports._poke = function (k, v, m) {
  return function () {
    m[k] = v;
    return m;
  };
};

exports._delete = function (k, m) {
  return function () {
    delete m[k];
    return m;
  };
};

exports._copyST = function (m) {
  return function () {
    var r = {};
    for (var k in m) {
      if (hasOwnProperty.call(m, k)) {
        r[k] = m[k];
      }
    }
    return r;
  };
};

exports.run = function (f) {
  return f();
};
