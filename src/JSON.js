const coerce = (x) => x;

export const _null = null;

export const fromBoolean = coerce;

export const fromInt = coerce;

export const fromString = coerce;

export const fromJArray = coerce;

export const fromJObject = coerce;

export const print = (j) => JSON.stringify(j);

export const printIndented = (j) => JSON.stringify(j, null, 2);
