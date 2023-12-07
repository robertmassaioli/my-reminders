import { useEffect } from 'react';

export function useEffectAsync(
  callback,//: () => Promise<void>,
  deps//: Readonly<A | undefined>
) {
  useEffect(() => {
    callback();
  }, deps);
}