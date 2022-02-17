import { createSlice, PayloadAction } from "@reduxjs/toolkit";
import { Map } from "immutable";
import { removeBinding, removeConstructor, removeDomain, removeStatement } from "../../lang/model";
import { Binding, Block, Constructor, Context, DataDefinition, defaultFormat, FormatField, freshBlock, freshHole, freshHoleType, freshLabel, Index, Indexable, Label, Lambda, lookupAt, Mode, Module, Parameter, replaceAt, Statement, Term, TermDefinition, Type } from "../../lang/syntax";

export default programSlice

// programSlice

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

export const programSlice = createSlice({
  name: "program",
  initialState,
  reducers: {
    // module
    manipulateStatements: (state, action: PayloadAction<{manipulation: ArrayManipulation, case: "data definition" | "term definition"}>) => {
      applyArrayManipulation(
        state,
        state.module.statements,
        action.payload.manipulation,
        {
          make: (m) => {
            switch (action.payload.case) {
              case "data definition": return {case: "data definition", label: freshLabel(), constructors: [], format: defaultFormat() } as DataDefinition
              case "term definition": return { case: "term definition", label: freshLabel(), type: freshHoleType(), block: freshBlock(), format: defaultFormat()} as TermDefinition
            }
          },
          remove: (m) => {
            state.module = removeStatement(state.module, state.focus)
            state.focus = moveIndex(state.module, state.focus, "up")
          },
          move: (m) => {throw new Error()}
        }
      )
    },
    // data definition
    manipulateConstructors: (state, action: PayloadAction<{manipulation: ArrayManipulation}>) => {
      applyArrayManipulation(
        state,
        lookupAt<Module, DataDefinition>(state.module, state.focus, Map()).target.constructors,
        action.payload.manipulation,
        {
          make: (m) => ({case: "constructor", label: freshLabel(), domains: [], format: defaultFormat()} as Constructor),
          remove: (m) => {
            state.module = removeConstructor(state.module, state.focus)
            state.focus = moveIndex(state.module, state.focus, "up")
          },
          move: (m) => {throw new Error()}
        },
      )
    },
    // constructor
    manipulateDomains: (state, action: PayloadAction<{manipulation: ArrayManipulation}>) => {}, // TODO
    // block
    manipulateBindings: (state, action: PayloadAction<{manipulation: ArrayManipulation}>) => {
      applyArrayManipulation(
        state,
        lookupAt<Module, Block>(state.module, state.focus, Map()).target.bindings,
        action.payload.manipulation,
        {
          make: (m) => ({case: "binding", label: freshLabel(), type: freshHoleType(), term: freshHole(), format: defaultFormat()} as Binding),
          remove: (m) => {
            state.module = removeBinding(state.module, state.focus)
            state.focus = moveIndex(state.module, state.focus, "up")
          },
          move: (m) => {throw new Error()}
        }
      )
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

// ArrayManipulation

export type ArrayManipulation = InsertArrayManipulation | RemoveArrayManipulation | MoveArrayManipulation
export type InsertArrayManipulation = {case: "insert", i: number}
export type RemoveArrayManipulation = {case: "remove", i: number}
export type MoveArrayManipulation = {case: "move", i: number, j: number}

function applyArrayManipulation<A>(
  state: ProgramState,
  array: A[],
  manipulation: ArrayManipulation,
  handlers: {
    make: (m: InsertArrayManipulation) => A,
    remove: (m: RemoveArrayManipulation) => void,
    move: (m: MoveArrayManipulation) => void
  }
): void {
  switch (manipulation.case) {
    case "insert": {
      let a: A = handlers.make(manipulation)
      array.splice(manipulation.i, 0, a)
      break
    }
    case "remove": {
      handlers.remove(manipulation)
      break
    }
    case "move": {
      handlers.move(manipulation)
      break
    }
  }
}

// Navigation

export type NavigationDirection = "up" | "down" | "left" | "right" | "next" | "previous" | "top"

export function moveIndex(module: Module, index: Index, direction: NavigationDirection): Index {
  throw new Error("unimplemented")
}

