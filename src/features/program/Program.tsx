import { useDispatch, useSelector } from "react-redux";
import "./Program.module.css";
import { Mode, ProgramState } from "./programSlice";
import store from "../../app/store";
import { Map } from "immutable";
import {
  Binding,
  Case,
  Constructor,
  Context,
  Definition,
  Module,
  Name,
  Parameter,
  Reference,
  Term,
  Type,
  UniqueBinding,
} from "../../lang/syntax";
import { Index } from "../../lang";
import { Format } from "../../lang/format";

const data_punc = <span className="punctuation">data</span>;
const alt_punc = <span className="punctuation">|</span>;
const arrow_punc = <span className="punctuation">-&gt;</span>;
const comma_punc = <span className="punctuation">,</span>;
const lparen_punc = <span className="punctuation">(</span>;
const rparen_punc = <span className="punctuation">)</span>;
const assign_punc = <span className="punctuation">=</span>;
const colon_punc = <span className="punctuation">:</span>;

export default function Program() {
  const module = useSelector<ProgramState, Format<Module>>(
    (state) => state.module
  );
  const mode = useSelector<ProgramState, Mode>((state) => state.mode);
  const focus = useSelector<ProgramState, Index<Module>>(
    (state) => state.focus
  );
  const dispatch = useDispatch<typeof store.dispatch>();

  function renderModule(
    module: Format<Module>,
    gamma: Context,
    index: Index<Module>
  ): JSX.Element {
    throw new Error();
  }
  function renderDefinition(
    definition: Format<Definition>,
    gamma: Context,
    index: Index<Module>
  ): JSX.Element {
    throw new Error();
  }
  function renderConstructor(
    constructor: Format<Constructor>,
    gamma: Context,
    index: Index<Module>
  ) {
    throw new Error();
  }
  function renderType(
    type: Format<Type>,
    gamma: Context,
    index: Index<Module>
  ): JSX.Element {
    throw new Error();
  }
  function renderTerm(
    term: Format<Term>,
    gamma: Context,
    index: Index<Module>
  ): JSX.Element {
    throw new Error();
  }
  function renderCase(
    case_: Format<Case>,
    gamma: Context,
    index: Index<Module>
  ): JSX.Element {
    throw new Error();
  }
  function renderParameter(
    param: Format<Parameter>,
    gamma: Context,
    index: Index<Module>
  ): JSX.Element {
    throw new Error();
  }
  function renderUniqueBinding(
    binding: Format<UniqueBinding>,
    gamma: Context,
    index: Index<Module>
  ): JSX.Element {
    throw new Error();
  }
  function renderBinding(
    binding: Format<Binding>,
    gamma: Context,
    index: Index<Module>
  ): JSX.Element {
    throw new Error();
  }
  function renderName(
    name: Format<Name>,
    gamma: Context,
    index: Index<Module>
  ): JSX.Element {
    throw new Error();
  }
  function renderReference(
    reference: Format<Reference>,
    gamma: Context,
    index: Index<Module>
  ): JSX.Element {
    throw new Error();
  }

  function renderHoleTermId(holeId: Symbol): JSX.Element {
    return <div className="holeId">?</div>;
  }

  return (
    <div className="Program">
      {(renderModule(module, Map(), { case: "here" }), focus)}
    </div>
  );
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
