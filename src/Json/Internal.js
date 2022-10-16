const toString = Object.prototype.toString;
const hasOwnProperty = Object.prototype.hasOwnProperty;
const coerce = (x) => x;

export const _parse = (left, right, s) => {
  try {
    return right(JSON.parse(s));
  }
  catch (e) {
    return left(e.message);
  }
};

export const print = (j) => JSON.stringify(j);

export const printIndented = (j) => JSON.stringify(j, null, 2);

export const _case = (isNull, isBool, isNum, isStr, isArr, isObj, j) => {
  if (j == null) return isNull(null);
  const ty = typeof j;
  if (ty === "boolean") return isBool(j);
  if (ty === "number") return isNum(j);
  if (ty === "string") return isStr(j);
  if (toString.call(j) === "[object Array]") return isArr(j);
  return isObj(j);
};

export const _null = null;

export const fromBoolean = coerce;

export const fromNumberWithDefault = (fallback) => (n) => isNaN(n) || !isFinite(n) ? fallback : n;

export const fromInt = coerce;

export const fromString = coerce;

export const fromArray = coerce;

export const fromObject = coerce;

export const _entries = (tuple, obj) =>
  Object.entries(obj).map(([k, v]) => tuple(k)(v));

export const _lookup = (nothing, just, key, obj) =>
  hasOwnProperty.call(obj, key) ? just(obj[key]) : nothing;
