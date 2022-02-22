import { Block, Context, Term, TermReference, Type } from "./syntax";

export type Inferable = Block | Term | TermReference

// export function infer<(syntax: Syntax, gamma: Context, type: Type): Type

export function infer(syntax: Inferable, gamma: Context, type: Type): Type {
  throw new Error("unimplemented: infer")
}
//   switch (syntax.case) {
//     case "block": {
//       syntax.definitions.forEach(definition => {
//         switch (definition.case) {
//           case "term definition": {
//             gamma = {
//               ...gamma,
//               terms: gamma.terms.set(definition.uniqueTermBinding.id, definition.type),
//               termNames: gamma.termNames.set(definition.uniqueTermBinding.id, definition.uniqueTermBinding.name)
//             }
//             break
//           }
//           case "data definition": break
//         }
//       })
//       return infer(syntax.body, gamma, type)
//     }
//     case "lambda": {
//       switch (type.case) {
//         case "arrow": {

//           break
//         }
//         default: throw new Error("inference of badly-typed term")
//       }
//       break
//     }
//     case "term reference": return gamma.terms.get(syntax.id) as Type
//   }
  // throw new Error("unimplemented: infer")
// }