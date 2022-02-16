import { useDispatch, useSelector } from "react-redux";
import "./Program.module.css";
import { ProgramState } from "./programSlice";
import {
  Binding,
  Block,
  Constructor,
  Mode,
  Module,
  Statement,
  Term,
  Type,
} from "../../lang/syntax";
import store from "../../app/store";

export default function Program() {
  const module = useSelector<ProgramState, Module>((state) => state.module);
  const mode = useSelector<ProgramState, Mode>((state) => state.mode);
  const dispatch = useDispatch<typeof store.dispatch>();

  function renderModule(module: Module): JSX.Element {}

  function renderStatement(statement: Statement): JSX.Element {}

  function renderConstructor(constructor: Constructor): JSX.Element {}

  function renderBlock(block: Block): JSX.Element {}

  function renderBinding(binding: Binding): JSX.Element {}

  function renderType(type: Type): JSX.Element {}

  function renderTerm(term: Term): JSX.Element {}

  function renderLabel(term: Term): JSX.Element {}

  return <div className="Program">{renderModule(module)}</div>;
}
