import { Record } from "immutable";
import { here, Index } from "../../lang";
import { defaultFormat, Format } from "../../lang/format";
import { Block, freshHoleTerm, freshHoleType, freshId, Module } from "../../lang/syntax";
// export default programSlice

// ProgramState

export type ProgramProps = {
  module: Format<Block>,
  focus: Index<Block>,
  mode: Mode
}

export type Mode = 
  | {case: "edit"}
  | {case: "label"}

const n: Symbol = Symbol()
const initialModule: Module = {
  case: "block",
  definitions: [
    {
      case: "data definition",
      typeBinding: {case: "type binding", name: "Nat"},
      constructors: [
        {case: "constructor", uniqueTermBinding: {case: "unique term binding", name: "zero", id: Symbol()}, parameters: []},
        {case: "constructor", uniqueTermBinding: {case: "unique term binding", name: "suc", id: Symbol()}, parameters: [{case: "parameter", label: {case: "label", name: "n"}, type: {case: "data", typeReference: {case: "type reference", name: "Nat"}}}]}
      ]
    },
    {
      case: "term definition",
      uniqueTermBinding: {case: "unique term binding", id: freshId(), name: "idNat"},
      type: {case: "arrow", parameters: [{case: "parameter", label: {case: "label", name: "n"}, type: {case: "data", typeReference: {case: "type reference", name: "Nat"}}}], output: {case: "data", typeReference: {case: "type reference", name: "Nat"}}},
      term: {case: "lambda", termBindings: [{case: "term binding", id: n}], block: {case: "block", definitions: [], body: {case: "neutral", termReference: {case: "term reference", id: n}, args: []}, type: {case: "data", typeReference: {case: "type reference", name: "Nat"}}}}}
  ],
  body: freshHoleTerm(),
  type: freshHoleType()
}

export const defaultProgramProps: ProgramProps = {
  module: defaultFormat(initialModule),
  focus: here,
  mode: {case: "edit"}
}

export class ProgramState extends Record(defaultProgramProps) {}

// export type Slice<State> = {
//   name: string,
//   initialState: State,
//   reducers: { [key: string]: (state: State, action?: infer A) => State }
// }

// programSlice

// export const programSlice = createSlice({
//   name: "program",
//   initialState,
//   reducers: {
//     // module
//     manipulateStatements: (state, action: PayloadAction<{manipulation: ArrayManipulation, case: "data definition" | "term definition"}>) => {},
//     // data definition
//     manipulateConstructors: (state, action: PayloadAction<{manipulation: ArrayManipulation}>) => {},
//     // constructor
//     manipulateDomains: (state, action: PayloadAction<{manipulation: ArrayManipulation}>) => {},
//     // block
//     manipulateBindings: (state, action: PayloadAction<{manipulation: ArrayManipulation}>) => {},
//     // term
//     fillLambdaTerm: (state) => {},
//     fillNeutralTerm: (state, action: PayloadAction<{id: Id, argCount: number}>) => {},
//     dig: (state) => {},
//     // label
//     appendLabel: (state, action: PayloadAction<string>) => {},
//     backspaceLabel: (state) => {},
//     // format
//     toggleFormatAttribute: (state, action: PayloadAction<FormatField>) => {},
//     moveFocus: (state, action: PayloadAction<NavigationDirection>) => {},
//     // mode
//     editMode: (state) => {
//       state.mode = {case: "edit"}
//     },
//     labelMode: (state) => {
//       state.mode = {case: "label"}
//     },
//   }
// })

// // ArrayManipulation

// export type ArrayManipulation = InsertArrayManipulation | RemoveArrayManipulation | MoveArrayManipulation
// export type InsertArrayManipulation = {case: "insert", i: number}
// export type RemoveArrayManipulation = {case: "remove", i: number}
// export type MoveArrayManipulation = {case: "move", i: number, j: number}

// function applyArrayManipulation<A>(
//   state: ProgramState,
//   array: A[],
//   manipulation: ArrayManipulation,
//   handlers: {
//     make: (m: InsertArrayManipulation) => A,
//     remove: (m: RemoveArrayManipulation) => void,
//     move: (m: MoveArrayManipulation) => void
//   }
// ): void {
//   switch (manipulation.case) {
//     case "insert": {
//       let a: A = handlers.make(manipulation)
//       array.splice(manipulation.i, 0, a)
//       break
//     }
//     case "remove": {
//       handlers.remove(manipulation)
//       break
//     }
//     case "move": {
//       handlers.move(manipulation)
//       break
//     }
//   }
// }

// // Navigation

// export type NavigationDirection = "up" | "down" | "left" | "right" | "next" | "previous" | "top"

// export function moveIndex<S1 extends Syntax, S2 extends Syntax>(s: S1, index: Index<S1>, direction: NavigationDirection): S2 {throw new Error()}

