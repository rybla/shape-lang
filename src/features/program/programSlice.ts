import { createSlice, PayloadAction } from "@reduxjs/toolkit";
import { Map } from "immutable";
import { here, Index } from "../../lang";
import { defaultFormat, Format, FormatField } from "../../lang/format";
import { Block, freshBlock, Id, Name, Syntax } from "../../lang/syntax";
export default programSlice

// ProgramState

export type ProgramState = {
  module: Format<Block>,
  focus: Index<Block>,
  mode: Mode
}

export type Mode = 
  | {case: "edit"}
  | {case: "label"}

// initialState

const initialState: ProgramState = {
  module: defaultFormat(freshBlock()),
  focus: here,
  mode: {case: "edit"}
}

// programSlice

export const programSlice = createSlice({
  name: "program",
  initialState,
  reducers: {
    // module
    manipulateStatements: (state, action: PayloadAction<{manipulation: ArrayManipulation, case: "data definition" | "term definition"}>) => {},
    // data definition
    manipulateConstructors: (state, action: PayloadAction<{manipulation: ArrayManipulation}>) => {},
    // constructor
    manipulateDomains: (state, action: PayloadAction<{manipulation: ArrayManipulation}>) => {},
    // block
    manipulateBindings: (state, action: PayloadAction<{manipulation: ArrayManipulation}>) => {},
    // term
    fillLambdaTerm: (state) => {},
    fillNeutralTerm: (state, action: PayloadAction<{id: Id, argCount: number}>) => {},
    dig: (state) => {},
    // label
    appendLabel: (state, action: PayloadAction<string>) => {},
    backspaceLabel: (state) => {},
    // format
    toggleFormatAttribute: (state, action: PayloadAction<FormatField>) => {},
    moveFocus: (state, action: PayloadAction<NavigationDirection>) => {},
    // mode
    editMode: (state) => {
      state.mode = {case: "edit"}
    },
    labelMode: (state) => {
      state.mode = {case: "label"}
    },
  }
})

// ArrayManipulation

export type ArrayManipulation = InsertArrayManipulation | RemoveArrayManipulation | MoveArrayManipulation
export type InsertArrayManipulation = {case: "insert", i: number}
export type RemoveArrayManipulation = {case: "remove", i: number}
export type MoveArrayManipulation = {case: "move", i: number, j: number}

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

// Navigation

export type NavigationDirection = "up" | "down" | "left" | "right" | "next" | "previous" | "top"

export function moveIndex<S1 extends Syntax, S2 extends Syntax>(s: S1, index: Index<S1>, direction: NavigationDirection): S2 {throw new Error()}

