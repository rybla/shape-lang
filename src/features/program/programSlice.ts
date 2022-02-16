import { createSlice, PayloadAction } from "@reduxjs/toolkit";
import { defaultFormat, freshBlock, freshHole, freshParameter, Label, lookupAt, Mode, Module, replaceAt, Term } from "../../lang/syntax";

export type ProgramState = {
  module: Module,
  mode: Mode
}

const initialState: ProgramState = {
  module: {
    case: "module",
    statements: [],
    format: defaultFormat()
  },
  mode: {case: "edit", index: []}
}

export const programSlice = createSlice({
  name: "program",
  initialState,
  reducers: {
    // module
    module_manipulateStatements: (state, action: PayloadAction<{manipulation: ArrayManipulation, sub: "type definition" | "data definition" | "term definition"}>) => {}, // TODO
    // data definition
    dataDefinition_manipulateConstructors: (state, action: PayloadAction<{manipulation: ArrayManipulation}>) => {}, // TODO
    // block
    block_manipulateBindings: (state, action: PayloadAction<{manipulation: ArrayManipulation}>) => {}, // TODO
    // term
    term_fillLambda: (state) => {
      replaceAt<Module, Term>(
        state.module,
        state.mode.index,
        new Map(),
        (target, gamma) => {
          return {
            case: "lambda",
            parameters: [freshParameter()],
            body: freshBlock(),
            format: defaultFormat()
          }
        }
      )
    },
    term_fillNeutral: (state, action: PayloadAction<{label: Label, argCount: number}>) => {
      let args: Term[] = [];
      for (let i = 0; i < action.payload.argCount; i++) args.push(freshHole())
      replaceAt<Module, Term>(
        state.module,
        state.mode.index,
        new Map(),
        (target, gamma) => {
          return {
            case: "neutral",
            applicant: action.payload.label,
            args,
            format: defaultFormat()
          }
        }
      )
    },
    term_dig: (state) => {}, // TODO
    term_apply: (state, action: PayloadAction<{applicant: Label, argCount: number, i: number}>) => {}, // TODO
    // parameter
    parameters_manipulate: (state, action: PayloadAction<{manipulation: ArrayManipulation}>) => {}, // TODO
    // label
    label_append: (state, action: PayloadAction<string>) => {}, // TODO
    label_backspace: (state) => {}, // TODO
    // format
    format_toggleIndented: (state) => {}, // TODO
    format_toggleUnannotated: (state) => {}, // TODO
    // navigation
    navigation_up: (state) => {}, // TODO
    navigation_down: (state) => {}, // TODO
    navigation_left: (state) => {}, // TODO
    navigation_right: (state) => {}, // TODO
    navigation_next: (state) => {}, // TODO
    navigation_previous: (state) => {}, // TODO
    navigation_top: (state) => {}, // TODO
    // mode
    mode_edit: (state) => {
      state.mode = {
        case: "edit",
        index: state.mode.index 
      }
    },
    mode_label: (state) => {
      let label =
        lookupAt<Module, Label>(
          state.module,
          state.mode.index,
          new Map()
        ).target
      state.mode = {
        case: "label",
        index: state.mode.index,
        label
      }
    },
  }
})

export type ArrayManipulation =
  | {case: "insert", i: number}
  | {case: "delete", i: number}
  | {case: "move", i: number, j: number}

export default programSlice