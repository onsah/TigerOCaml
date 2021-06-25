/**
 * Auxuliary functions
 */
export const toSingleString = (arr: string[]): string =>
  arr.reduce((prev, curr) => `${prev}\n${curr}`);

export const camelCased = (str: string): string =>
  `${str[0].toUpperCase()}${str.slice(1)}`;