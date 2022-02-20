// Format

import { Label, Syntax } from "./syntax"

export type FormatField = "indented" | "unannotated"
export type FormatData = { [K in FormatField]?: boolean }

export type Format<S extends Syntax> =
  {
    [Key in keyof S]:
      S[Key] extends Label ? S :
      S[Key] extends (infer T)[] ? (T extends Syntax ? Format<T>[] : T[]) :
      S[Key] extends Syntax ? Format<S[Key]> : S[Key]
  } 
  & FormatData

export function defaultFormat<S extends Syntax>(syntax: S): Format<S> {
  throw new Error()
}