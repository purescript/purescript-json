"use strict";

var toString = Object.prototype.toString;
var hasOwnProperty = Object.prototype.hasOwnProperty;

exports._parse = function (left, right, s) {
  try {
    return right(JSON.parse(s));
  }
  catch (e) {
    return left(e.message);
  }
};

exports.print = function (j) {
  return JSON.stringify(j);
};

exports.printIndented = function (j) {
  return JSON.stringify(j, null, 2);
};

exports._case = function (isNull, isBool, isNum, isStr, isArr, isObj, j) {
  if (j == null) return isNull(null);
  var ty = typeof j;
  if (ty === "boolean") return isBool(j);
  if (ty === "number") return isNum(j);
  if (ty === "string") return isStr(j);
  if (toString.call(j) === "[object Array]") return isArr(j);
  return isObj(j);
};

exports._null = null;

var coerce = function (x) {
  return x;
};

exports.fromBoolean = coerce;

exports.fromNumberWithDefault = function (x) {
  return function (y) {
    if (isNaN(y) || !isFinite(y)) return x;
    return y;
  };
};

exports.fromInt = coerce;

exports.fromString = coerce;

exports.fromArray = coerce;

exports.fromObject = coerce;

exports._entries = function (tuple, obj) {
  var result = [];
  for (var k in obj) {
    if (hasOwnProperty.call(obj, k)) {
      result[k] = tuple(k, obj[k]);
    }
  }
  return result;
};

exports._lookup = function (nothing, just, key, obj) {
  return hasOwnProperty.call(obj, key) ? just(obj[key]) : nothing;
};
