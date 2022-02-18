// Format

import { Label, Syntax } from "./syntax"

export type FormatData = {
  indented?: boolean,
  unannotated?: boolean
}

export type Format<S extends Syntax> =
  {
    [Key in keyof S]:
      S extends Label ? S :
      S[Key] extends (infer T)[] ? (T extends Syntax ? Format<T>[] : T[]) :
      S[Key] extends Syntax ? Format<S[Key]> : S[Key]
  } 
  & FormatData

export function defaultFormat<S extends Syntax>(syntax: S): Format<S> {
  throw new Error()
}