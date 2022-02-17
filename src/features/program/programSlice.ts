import { createSlice, PayloadAction } from "@reduxjs/toolkit";
import { Map } from "immutable";
import { removeBinding, removeConstructor, removeDomain, removeStatement } from "../../lang/model";
import { Binding, Block, Constructor, Context, DataDefinition, defaultFormat, FormatField, freshBlock, freshHole, freshHoleType, freshLabel, Index, Indexable, Label, Lambda, lookupAt, Mode, Module, Parameter, replaceAt, Statement, Term, Type } from "../../lang/syntax";

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

// function applyArrayManipulation<A>(manipulation: ArrayManipulation, make: () => A, remove: () => void)

export const programSlice = createSlice({
  name: "program",
  initialState,
  reducers: {
    // module
    manipulateStatements: (state, action: PayloadAction<{manipulation: ArrayManipulation, case: "data definition" | "term definition"}>) => {
      switch (action.payload.manipulation.case) {
        case "insert": {
          let statement: Statement;
          switch (action.payload.case) {
            case "data definition": {
              statement = {
                case: "data definition",
                label: freshLabel(),
                constructors: [],
                format: defaultFormat()
              }
              break
            }
            case "term definition": {
              statement = {
                case: "term definition",
                label: freshLabel(),
                type: freshHoleType(),
                block: freshBlock(),
                format: defaultFormat()
              }
              break
            }
          }
          state.module.statements.splice(action.payload.manipulation.i, 0, statement)
          break
        }
        case "remove": {
          state.module = removeStatement(state.module, state.focus)
          break
        }
        case "move": {throw new Error()}
      }
    },
    // data definition
    manipulateConstructors: (state, action: PayloadAction<{manipulation: ArrayManipulation}>) => {
      switch (action.payload.manipulation.case) {
        case "insert": {
          let constructor: Constructor = {
            case: "constructor",
            label: freshLabel(),
            domains: [],
            format: defaultFormat()
          }
          let dataDefinition: DataDefinition = lookupAt<Module, DataDefinition>(state.module, state.focus, Map()).target
          dataDefinition.constructors.splice(action.payload.manipulation.i, 0, constructor)
          break
        }
        case "remove": {
          state.module = removeConstructor(state.module, state.focus)
          state.focus = moveIndex(state.module, state.focus, "up")
          break
        }
        case "move": {throw new Error()}
      }
    },
    // constructor
    manipulateDomains: (state, action: PayloadAction<{manipulation: ArrayManipulation}>) => {}, // TODO
    // block
    manipulateBindings: (state, action: PayloadAction<{manipulation: ArrayManipulation}>) => {
      let block = lookupAt<Module, Block>(state.module, state.focus, Map()).target
      switch (action.payload.manipulation.case) {
        case "insert": {
          let binding: Binding = {
            case: "binding",
            label: freshLabel(),
            type: freshHoleType(),
            term: freshHole(),
            format: defaultFormat()
          }
          block.bindings.splice(action.payload.manipulation.i, 0, binding)
          break
        }
        case "remove": {
          state.module = removeBinding(state.module, state.focus)
          break
        }
        case "move": {throw new Error()}
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
    // label
    appendLabel: (state, action: PayloadAction<string>) => {
      switch (state.mode.case) {
        case "label": {
          state.mode.label.value = state.mode.label.value.concat(action.payload)
          break
        }
        default: break;
      }
    },
    backspaceLabel: (state) => {
      switch (state.mode.case) {
        case "label": {
          state.mode.label.value = state.mode.label.value.slice(0, state.mode.label.value.length - 2)
          break
        }
        default: break;
      }
    },
    // format
    toggleFormatAttribute: (state, action: PayloadAction<FormatField>) => {
      let node = lookupAt<Module, Indexable>(state.module, state.focus, Map()).target
      if (!node.format.has(action.payload)) node.format.set(action.payload, false)
      node.format.set(action.payload, !node.format.get(action.payload))
    },
    moveFocus: (state, action: PayloadAction<NavigationDirection>) => {
      state.focus = moveIndex(state.module, state.focus, action.payload)
    },
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

export type ArrayManipulation =
  | {case: "insert", i: number}
  | {case: "remove", i: number}
  | {case: "move", i: number, j: number}

export function moveIndex(module: Module, index: Index, direction: NavigationDirection): Index {
  throw new Error("unimplemented")
}

export default programSlice
