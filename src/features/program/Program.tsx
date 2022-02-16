import { useDispatch, useSelector } from "react-redux";
import "./Program.module.css";
import { ProgramState } from "./programSlice";
import {
  Binding,
  Block,
  Constructor,
  Context,
  Label,
  Mode,
  Module,
  Parameter,
  Statement,
  Term,
  Type,
  typeOfConstructor,
} from "../../lang/syntax";
import store from "../../app/store";
import { List, Map } from "immutable";

const data_symbol = <span>data</span>;
const arrow_symbol = <span>-&gt;</span>;

export default function Program() {
  const module = useSelector<ProgramState, Module>((state) => state.module);
  const mode = useSelector<ProgramState, Mode>((state) => state.mode);
  const dispatch = useDispatch<typeof store.dispatch>();

  function renderModule(module: Module, gamma: Context): JSX.Element {
    module.statements.forEach((statement) => {
      switch (statement.case) {
        case "data definition": {
          statement.constructors.forEach((constructor) => {
            gamma = gamma.set(
              constructor.label,
              typeOfConstructor(statement.label, constructor)
            );
          });
          break;
        }
        case "term definition": {
          gamma = gamma.set(statement.label, statement.type);
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

  function renderStatement(statement: Statement, gamma: Context): JSX.Element {
    switch (statement.case) {
      case "data definition": {
        return (
          <div className="statement data-definition">
            <div className="header">
              {data_symbol}
              {renderLabel(statement.label)}
            </div>
            <div className="constructors">
              {statement.constructors.map((construtor) =>
                renderConstructor(construtor, gamma)
              )}
            </div>
          </div>
        );
      }
      case "term definition": {
        return (
          <div className="statement term-definition">
            <div className="header">{renderLabel(statement.label)}</div>
            <div className="signature">{renderType(statement.type, gamma)}</div>
            <div className="implementation">
              {renderBlock(statement.block, gamma)}
            </div>
          </div>
        );
      }
    }
  }

  function renderConstructor(
    constructor: Constructor,
    gamma: Context
  ): JSX.Element {
    return (
      <div className="constructor">
        <div className="label">{renderLabel(constructor.label)}</div>
        <div className="domains">
          {constructor.domains.map((type) => renderType(type, gamma))}
        </div>
      </div>
    );
  }

  function renderBlock(block: Block, gamma: Context): JSX.Element {
    block.bindings.forEach((binding) => {
      gamma = gamma.set(binding.label, binding.type);
    });
    return (
      <div className="block">
        <div className="bindings">
          {block.bindings.map((binding) => renderBinding(binding, gamma))}
        </div>
        <div className="body">{renderTerm(block.body, gamma)}</div>
      </div>
    );
  }

  function renderBinding(binding: Binding, gamma: Context): JSX.Element {
    return (
      <div className="binding">
        {renderLabel(binding.label)}
        {renderType(binding.type, gamma)}
        {renderTerm(binding.term, gamma)}
      </div>
    );
  }

  function renderType(type: Type, gamma: Context): JSX.Element {
    switch (type.case) {
      case "arrow": {
        return (
          <div className="type arrow">
            {intersperseRight(
              type.domains.map((type) => renderType(type, gamma)),
              arrow_symbol
            )}
            {renderType(type.codomain, gamma)}
          </div>
        );
      }
      case "data": {
        return <div className="type data">{renderLabel(type.label)}</div>;
      }
    }
  }

  function renderTerm(term: Term, gamma: Context): JSX.Element {
    switch (term.case) {
      case "lambda": {
        return (
          <div className="term lambda">
            <div className="parameters">
              {term.parameters.map((param) => renderParameter(param, gamma))}
            </div>
          </div>
        );
      }
      case "neutral": {
        return (
          <div className="term neutral">
            {renderLabel(term.applicant)}
            <div className="arguments">
              {term.args.map((arg) => renderTerm(arg, gamma))}
            </div>
          </div>
        );
      }
      case "hole": {
        return <div className="term hole">{renderHoleId(term.holeId)}</div>;
      }
    }
  }

  function renderParameter(param: Parameter, gamma: Context): JSX.Element {
    return (
      <div className="parameter">
        {renderLabel(param.label)}
        {renderType(param.domain, gamma)}
      </div>
    );
  }

  function renderLabel(label: Label): JSX.Element {
    return <div className="label">{label.value}</div>;
  }

  function renderHoleId(holeId: Symbol): JSX.Element {
    return <div className="holeId">?</div>;
  }

  return <div className="Program">{renderModule(module, Map())}</div>;
}

function intercalate<A>(list: List<A>, sep: A): List<A> {
  return intersperseLeft(list, sep).delete(0);
}

function intersperseRight<A>(list: List<A>, sep: A): List<A> {
  return list.flatMap((a) => [a, sep]);
}

function intersperseLeft<A>(list: List<A>, sep: A): List<A> {
  return list.flatMap((a) => [sep, a]);
}
