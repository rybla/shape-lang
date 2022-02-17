import { createSlice, PayloadAction } from "@reduxjs/toolkit";
import { Map } from "immutable";
import { deleteBinding } from "../../lang/model";
import { Binding, Block, Context, defaultFormat, freshBlock, freshHole, freshLabel, Index, Indexable, Label, lookupAt, Mode, Module, replaceAt, Term, Type } from "../../lang/syntax";

export type ProgramState = {
  module: Module,
  focus: Index,
  mode: Mode
}

const initialState: ProgramState = {
  module: {
    case: "module",
    statements: [],
    format: defaultFormat()
  },
  focus: "top",
  mode: {case: "edit"}
}

// function applyManipulation(manipulation: ArrayManipulation, )

export const programSlice = createSlice({
  name: "program",
  initialState,
  reducers: {
    // module
    manipulateStatements: (state, action: PayloadAction<{manipulation: ArrayManipulation<Type>, case: "data definition" | "term definition"}>) => {}, // TODO
    // data definition
    manipulateConstructors: (state, action: PayloadAction<{manipulation: ArrayManipulation<Type>}>) => {}, // TODO
    // constructor
    manipulateDomains: (state, action: PayloadAction<{manipulation: ArrayManipulation<Type>}>) => {}, // TODO
    // block
    manipulateBindings: (state, action: PayloadAction<{manipulation: ArrayManipulation<Type>}>) => {
      let block = lookupAt<Module, Block>(state.module, state.focus, Map()).target
      switch (action.payload.manipulation.case) {
        case "insert": {
          let binding: Binding = {
            case: "binding",
            label: freshLabel(),
            type: action.payload.manipulation.data,
            term: freshHole(),
            format: defaultFormat()
          }
          block.bindings.splice(action.payload.manipulation.i, 0, binding)
          break;
        }
        case "delete": {
          state.module = deleteBinding(state.module, state.focus)
          break;
        }
      }
    },
    // term
    fillLambda: (state) => {
      replaceAt<Module, Term>(
        state.module,
        state.focus,
        Map(),
        (target, gamma) => ({
            case: "lambda",
            parameters: [],
            body: freshBlock(),
            format: defaultFormat()
        })
      )
    },
    fillNeutral: (state, action: PayloadAction<{label: Label, argCount: number}>) => {
      let args: Term[] = [];
      for (let i = 0; i < action.payload.argCount; i++) args.push(freshHole())
      replaceAt<Module, Term>(
        state.module,
        state.focus,
        Map(),
        (target, gamma) => ({
          case: "neutral",
          applicant: action.payload.label,
          args,
          format: defaultFormat()
        })
      )
    },
    dig: (state) => {
      replaceAt<Module, Term>(
        state.module,
        state.focus,
        Map(),
        (target, gamma) => freshHole()
      )
    },
    // TODO: applyFunction: (state, action: PayloadAction<{applicant: Label, argCount: number, i: number}>) => {}, // TODO
    // parameter
    manipulateParameters: (state, action: PayloadAction<{manipulation: ArrayManipulation<Type>}>) => {}, // TODO
    // label
    appendLabel: (state, action: PayloadAction<string>) => {}, // TODO
    backspaceLabel: (state) => {}, // TODO
    // format
    toggleIndented: (state) => {}, // TODO
    toggleUnannotated: (state) => {}, // TODO
    // navigation
    moveFocus: (state, action: PayloadAction<NavigationDirection>) => {
      state.focus = moveIndex(state.module, state.focus, action.payload)
    }, // TODO
    // mode
    editMode: (state) => {
      state.mode = {
        case: "edit"
      }
    },
    labelMode: (state) => {
      let label =
        lookupAt<Module, Label>(
          state.module,
          state.focus,
          Map()
        ).target
      state.mode = {
        case: "label",
        label
      }
    },
  }
})

export type NavigationDirection = "up" | "down" | "left" | "right" | "next" | "previous" | "top"

export type ArrayManipulation<A> =
  | {case: "insert", i: number, data: A}
  | {case: "delete", i: number}
  | {case: "move", i: number, j: number}

export function moveIndex(module: Module, index: Index, direction: NavigationDirection): Index {
  throw new Error("unimplemented")
}

export default programSlice
