import { createSlice, PayloadAction } from "@reduxjs/toolkit";
import { Block, defaultFormat, freshBlock, freshHole, freshLabel, freshParameter, Label, lookupAt, Mode, replaceAt, Term, TermDefinition } from "../../lang/syntax";

interface ProgramState {
  block: Block,
  mode: Mode
}

const initialState: ProgramState = {
  block: {
    case: "block",
    bindings: [],
    body: freshHole(),
    format: defaultFormat()
  },
  mode: {case: "edit", index: []}
}

export const programSlice = createSlice({
  name: "program",
  initialState,
  reducers: {
    // program
    program_manipulateStatements: (state, action: PayloadAction<{manipulation: ArrayManipulation, sub: "type definition" | "data definition" | "term definition"}>) => {},
    // data definition
    dataDefinition_manipulateConstructors: (state, action: PayloadAction<{manipulation: ArrayManipulation}>) => {},
    // block
    block_manipulateBindings: (state, action: PayloadAction<{manipulation: ArrayManipulation}>) => {},
    // term
    term_fillLambda: (state) => {
      replaceAt<Block, Term>(
        state.block,
        state.mode.index,
        [],
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
      replaceAt<Block, Term>(
        state.block,
        state.mode.index,
        [],
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
    term_dig: (state) => {},
    term_apply: (state, action: PayloadAction<{applicant: Label, argCount: number, i: number}>) => {},
    // parameter
    parameters_manipulate: (state, action: PayloadAction<{manipulation: ArrayManipulation}>) => {},
    // label
    label_append: (state, action: PayloadAction<string>) => {},
    label_backspace: (state) => {},
    // format
    format_toggleIndented: (state) => {},
    format_toggleUnannotated: (state) => {},
    // navigation
    navigation_up: (state) => {},
    navigation_down: (state) => {},
    navigation_left: (state) => {},
    navigation_right: (state) => {},
    navigation_next: (state) => {},
    navigation_previous: (state) => {},
    navigation_top: (state) => {},
    // mode
    mode_edit: (state) => {
      state.mode = {
        case: "edit",
        index: state.mode.index 
      }
    },
    mode_label: (state) => {
      let label = lookupAt<Block, Label>(state.block, state.mode.index, []).target
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

export default programSlice.reducer