import { useDispatch, useSelector } from "react-redux";
import "./Program.module.css";
import { ProgramState } from "./programSlice";
import {
  Binding,
  BindingIndex,
  Block,
  BlockIndex,
  Constructor,
  ConstructorIndex,
  Context,
  Label,
  Mode,
  Module,
  ModuleIndex,
  Parameter,
  ParameterIndex,
  Statement,
  StatementIndex,
  Term,
  TermIndex,
  Type,
  TypeIndex,
  typeOfConstructor,
} from "../../lang/syntax";
import store from "../../app/store";
import { Map } from "immutable";

const data_punc = <span className="punctuation">data</span>;
const alt_punc = <span className="punctuation">|</span>;
const arrow_punc = <span className="punctuation">-&gt;</span>;
const comma_punc = <span className="punctuation">,</span>;
const lparen_punc = <span className="punctuation">(</span>;
const rparen_punc = <span className="punctuation">)</span>;
const assign_punc = <span className="punctuation">=</span>;
const colon_punc = <span className="punctuation">:</span>;

export default function Program() {
  const module = useSelector<ProgramState, Module>((state) => state.module);
  const mode = useSelector<ProgramState, Mode>((state) => state.mode);
  const focus = useSelector<ProgramState, ModuleIndex>((state) => state.focus);
  const dispatch = useDispatch<typeof store.dispatch>();

  function renderModule(
    module: Module,
    gamma: Context,
    index: ModuleIndex
  ): JSX.Element {
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
          renderStatement(statement, gamma, index.index)
        )}
      </div>
    );
  }

  function renderStatement(
    statement: Statement,
    gamma: Context,
    index: StatementIndex
  ): JSX.Element {
    switch (statement.case) {
      case "data definition": {
        return (
          <div className="statement data-definition">
            <div className="header">
              {data_punc}
              {renderLabel(statement.label)}
              {assign_punc}
            </div>
            <div className="constructors">
              {intersperseLeft(
                statement.constructors.map((construtor) =>
                  renderConstructor(construtor, gamma)
                ),
                alt_punc
              )}
            </div>
          </div>
        );
      }
      case "term definition": {
        return (
          <div className="statement term-definition">
            {renderLabel(statement.label)}
            {colon_punc}
            {renderType(statement.type, gamma)}
            {assign_punc}
            {renderBlock(statement.block, gamma)}
          </div>
        );
      }
    }
  }

  function renderConstructor(
    constructor: Constructor,
    gamma: Context,
    index: ConstructorIndex
  ): JSX.Element {
    return (
      <div className="constructor">
        {renderLabel(constructor.label)}
        {colon_punc}
        {constructor.domains.map((type) => renderType(type, gamma))}
      </div>
    );
  }

  function renderBlock(
    block: Block,
    gamma: Context,
    index: BlockIndex
  ): JSX.Element {
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

  function renderBinding(
    binding: Binding,
    gamma: Context,
    index: BindingIndex
  ): JSX.Element {
    return (
      <div className="binding">
        {renderLabel(binding.label)}
        {colon_punc}
        {renderType(binding.type, gamma)}
        {assign_punc}
        {renderTerm(binding.term, gamma)}
      </div>
    );
  }

  function renderType(
    type: Type,
    gamma: Context,
    index: TypeIndex
  ): JSX.Element {
    switch (type.case) {
      case "arrow": {
        return (
          <div className="type arrow">
            {intersperseRight(
              type.domains.map((type) => renderType(type, gamma)),
              arrow_punc
            )}
            {renderType(type.codomain, gamma)}
          </div>
        );
      }
      case "data": {
        return <div className="type data">{renderLabel(type.label)}</div>;
      }
      case "hole": {
        return <div className="type hole">{renderHoleTermId(type.holeId)}</div>;
      }
    }
  }

  function renderTerm(
    term: Term,
    gamma: Context,
    index: TermIndex
  ): JSX.Element {
    switch (term.case) {
      case "lambda": {
        let gammaBody = gamma;
        term.parameters.forEach(
          (param) => (gammaBody = gammaBody.set(param.label, param.domain))
        );
        return (
          <div className="term lambda">
            {lparen_punc}
            {intercalate(
              term.parameters.map((param) => renderParameter(param, gamma)),
              comma_punc
            )}
            {rparen_punc}
            {arrow_punc}
            {renderBlock(term.body, gammaBody)}
          </div>
        );
      }
      case "neutral": {
        return (
          <div className="term neutral">
            {renderLabel(term.applicant)}
            {lparen_punc}
            {term.args.map((arg) => renderTerm(arg, gamma))}
            {rparen_punc}
          </div>
        );
      }
      case "hole": {
        return <div className="term hole">{renderHoleTermId(term.holeId)}</div>;
      }
    }
  }

  function renderParameter(
    param: Parameter,
    gamma: Context,
    index: ParameterIndex
  ): JSX.Element {
    return (
      <div className="parameter">
        {lparen_punc}
        {renderLabel(param.label)}
        {colon_punc}
        {renderType(param.domain, gamma)}
        {rparen_punc}
      </div>
    );
  }

  function renderLabel(label: Label): JSX.Element {
    return <div className="label">{label.value}</div>;
  }

  function renderHoleTermId(holeId: Symbol): JSX.Element {
    return <div className="holeId">?</div>;
  }

  return <div className="Program">{(renderModule(module, Map()), focus)}</div>;
}

function intercalate<A>(list: A[], sep: A): A[] {
  let listNew = intersperseRight(list, sep);
  listNew.splice(listNew.length - 1, 1);
  return listNew;
}

function intersperseRight<A>(list: A[], sep: A): A[] {
  return list.flatMap((a) => [a, sep]);
}

function intersperseLeft<A>(list: A[], sep: A): A[] {
  return list.flatMap((a) => [sep, a]);
}
