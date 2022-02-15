import { createSlice, PayloadAction } from "@reduxjs/toolkit";
import { Block, DeBruijn, defaultFormat, freshBlock, freshHole, freshLabel, freshParameter, Label, lookupAt, Mode, replaceAt, Term } from "../../lang/syntax";

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
    // block
    block_insertBinding: (state) => {},
    block_deleteBinding: (state) => {},
    block_moveBinding: (state, action: PayloadAction<number>) => {},
    // term
    term_fillUniverse: (state) => {
      replaceAt<Block, Term>(
        state.block,
        state.mode.index,
        [],
        (target, gamma) => {
          return {
            case: "universe",
            level: 0,
            format: target.format
          }
        }
      )
    },
    term_fillPi: (state) => {
      replaceAt<Block, Term>(
        state.block,
        state.mode.index,
        [],
        (target, gamma) => {
          return {
            case: "pi",
            parameters: [freshParameter()],
            codomain: freshBlock(),
            format: defaultFormat()
          }
        }
      )
    },
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
    term_fillNeutral: (state, action: PayloadAction<DeBruijn>) => {
      replaceAt<Block, Term>(
        state.block,
        state.mode.index,
        [],
        (target, gamma) => {
          return {
            case: "neutral",
            applicant: action.payload,
            arguments: [], // TODO: calculate via type of applicant
            format: defaultFormat()
          }
        }
      )
    },
    term_dig: (state) => {},
    term_apply: (state, action: PayloadAction<DeBruijn>) => {},
    // parameter
    parameter_insertParameter: (state) => {},
    parameter_deleteParameter: (state) => {},
    parameter_moveParameter: (state, action: PayloadAction<[number, number]>) => {},
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

export const {block_insertBinding, block_deleteBinding, block_moveBinding, term_fillUniverse, term_fillPi, term_fillNeutral, term_dig, term_apply, parameter_insertParameter, parameter_deleteParameter, parameter_moveParameter, label_append, format_toggleIndented, format_toggleUnannotated, navigation_up, navigation_down, navigation_left, navigation_right, navigation_next, navigation_previous, navigation_top, mode_edit, mode_label} = programSlice.actions

export default programSlice.reducer