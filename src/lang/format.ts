import { Syntax } from "./syntax"

export type FormatField = "indented" | "unannotated"
export type FormatData = { [K in FormatField]?: boolean }
export const defaultFormatData: FormatData = {}

export type Format<S extends Syntax> = S

// export type Format<S extends Syntax> =
//   (
//     S extends IndexTerminal ? S :
//     S extends IndexStepable ?
//       {
//         [Key in keyof S]:
//           Key extends ("case" | "name" | "id" | "holeId") ? S[Key] :
//           S[Key] extends (infer T)[] ? (T extends Syntax ? Format<T>[] : never) :
//           S[Key] extends (infer T) ? (T extends Syntax ? Format<T> : never) :
//           never
//       } :
//     never
//   )
//   & {formatData: FormatData}

export function defaultFormat<S extends Syntax>(syntax: S): Format<S> {
  return syntax
  // switch (syntax.case) {
  //   case "block":
  //   case "term definition":
  //     return {case: "term definition", uniqueTermBinding: defaultFormat(syntax.uniqueTermBinding), type: }
  //   // case "term definition":
  //   // case "constructor":
  //   // case "case":
  //   // case "arrow":
  //   // case "data":
  //   // syntax.
  //   //   return {...formatData: defaultFormatData}
  // }
}

export function unformat<S extends Syntax>(formattedSyntax: Format<S>): S {
  // throw new Error("umimplemented: unformat")
  return formattedSyntax
}
