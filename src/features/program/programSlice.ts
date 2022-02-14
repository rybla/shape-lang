import { createSlice, PayloadAction } from "@reduxjs/toolkit";
import { Block, DeBruijn, defaultFormat, freshHole, Mode, modifyInBlock, Term } from "../../lang/syntax";

interface ProgramState {
  block: Block,
  mode: Mode
}

const initialState: ProgramState = {
  block: {
    bindings: [],
    body: freshHole(),
    format: defaultFormat()
  },
  mode: {
    index: [],
    sub: {case: "edit"}
  },
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
      // TODO: does this look right?
      modifyInBlock<Term>(
        state.block,
        state.mode.index,
        _ => ({
          case: "universe",
          level: 0,
          format: defaultFormat()
        }))
    },
    term_fillPi: (state) => {},
    term_fillLambda: (state) => {},
    term_fillNeutral: (state, action: PayloadAction<DeBruijn>) => {},
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
    mode_term: (state) => {},
    mode_label: (state) => {},
  }
})

export const {} = programSlice.actions

export default programSlice.reducer