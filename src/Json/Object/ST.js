export const _new = () => {
  return {};
};

export const _poke = (k, v, obj) => () => {
  obj[k] = v;
};

const copy = (obj) => () => Object.assign({}, obj);

export const freeze = copy;

export const thaw = copy;

export const run = (f) => f();
