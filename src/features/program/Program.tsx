import { useDispatch, useSelector } from "react-redux";
import "./Program.module.css";
import { ProgramState } from "./programSlice";
import {
  Binding,
  Block,
  Constructor,
  Context,
  Mode,
  Module,
  Statement,
  Term,
  Type,
  typeOfConstructor,
} from "../../lang/syntax";
import store from "../../app/store";

export default function Program() {
  const module = useSelector<ProgramState, Module>((state) => state.module);
  const mode = useSelector<ProgramState, Mode>((state) => state.mode);
  const dispatch = useDispatch<typeof store.dispatch>();

  function renderModule(module: Module, gamma: Context): JSX.Element {
    module.statements.forEach((statement) => {
      switch (statement.case) {
        case "data definition": {
          statement.constructors.forEach((constructor) => {
            gamma.set(
              constructor.label,
              typeOfConstructor(statement.label, constructor)
            );
          });
          break;
        }
        case "term definition": {
          gamma.set(statement.label, statement.type);
          break;
        }
      }
    });
    return (
      <div className="module">
        {module.statements.map((statement) =>
          renderStatement(statement, gamma)
        )}
      </div>
    );
  }

  function renderStatement(statement: Statement, gamma: Context): JSX.Element {}

  function renderConstructor(
    constructor: Constructor,
    gamma: Context
  ): JSX.Element {}

  function renderBlock(block: Block, gamma: Context): JSX.Element {}

  function renderBinding(binding: Binding, gamma: Context): JSX.Element {}

  function renderType(type: Type, gamma: Context): JSX.Element {}

  function renderTerm(term: Term, gamma: Context): JSX.Element {}

  function renderLabel(term: Term, gamma: Context): JSX.Element {}

  return <div className="Program">{renderModule(module)}</div>;
}
