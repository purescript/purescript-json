"use strict";

var toString = {}.prototype.toString;

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

exports.printPretty = function (j) {
  return JSON.stringify(j, null, 2);
};

exports._case = function (isNull, isBool, isNum, isStr, isArr, isObj, j) {
  var ty = typeof j;
  if (j == null) return isNull();
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
exports.fromNumber = coerce;
exports.fromString = coerce;
exports.fromArray = coerce;
exports.fromObject = coerce;
