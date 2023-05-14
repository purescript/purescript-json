const toString = Object.prototype.toString;
const hasOwnProperty = Object.prototype.hasOwnProperty;

export const _parse = (left, right, s) => {
  try {
    return right(JSON.parse(s));
  }
  catch (e) {
    return left(e.message);
  }
};

export const _fromNumberWithDefault = (fallback, n) => isNaN(n) || !isFinite(n) ? fallback : n;

export const _case = (isNull, isBool, isNum, isStr, isArr, isObj, j) => {
  if (j == null) return isNull(null);
  const ty = typeof j;
  if (ty === "boolean") return isBool(j);
  if (ty === "number") return isNum(j);
  if (ty === "string") return isStr(j);
  if (toString.call(j) === "[object Array]") return isArr(j);
  return isObj(j);
};

export const toArray = (js) => js;
export const fromArray = (js) => js;

export const _fromEntries = (fst, snd, entries) => {
  const result = {};
  for (var i = 0; i < entries.length; i++) {
    result[fst(entries[i])] = snd(entries[i]);
  }
  return result;
};

export const _insert = (k, v, obj) =>
  Object.assign({ [k]: v }, obj);

export const _delete = (k, obj) => {
  if (!Object.hasOwn(obj, k)) return obj;
  const result = Object.assign({}, obj);
  delete result[k];
  return result;
};

export const _entries = (tuple, obj) =>
  Object.entries(obj).map(([k, v]) => tuple(k)(v));

export const _lookup = (nothing, just, key, obj) =>
  hasOwnProperty.call(obj, key) ? just(obj[key]) : nothing;
