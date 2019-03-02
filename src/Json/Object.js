"use strict";

var hasOwnProperty = {}.hasOwnProperty;

exports.keys = function (obj) {
  return Object.keys(obj);
};

exports.values = function (obj) {
  return Object.values(obj);
};

exports._key = function (nothing, just, key, obj) {
  return hasOwnProperty.call(obj, key) ? nothing : just(key);
};
