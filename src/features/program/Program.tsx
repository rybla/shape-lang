import styles from "./Program.module.css";
import { ProgramState } from "./ProgramState";
import {
  ArrowType,
  Case,
  Constructor,
  Context,
  Definition,
  emptyContext,
  Label,
  Module,
  Parameter,
  Term,
  TermBinding,
  TermReference,
  Type,
  TypeBinding,
  typeOfConstructor,
  TypeReference,
  UniqueTermBinding,
} from "../../lang/syntax";
import { concatIndex, here, Index } from "../../lang";
import { defaultFormat, Format, unformat } from "../../lang/format";
import { infer } from "../../lang/typing";

const data_keyword = <span className={styles.keyword}>data</span>;
const match_keyword = <span className={styles.keyword}>match</span>;
const with_keyword = <span className={styles.keyword}>with</span>;
const alt_punctuation = <span className={styles.punctuation}>|</span>;
const arrow_punctuation = <span className={styles.punctuation}>-&gt;</span>;
const comma_punctuation = <span className={styles.punctuation}>,</span>;
const lparen_punctuation = <span className={styles.punctuation}>(</span>;
const rparen_punctuation = <span className={styles.punctuation}>)</span>;
const assign_punctuation = <span className={styles.punctuation}>=</span>;
const colon_punctuation = <span className={styles.punctuation}>:</span>;

export default function Program() {
  var programState: ProgramState = new ProgramState();

  function renderBlock(
    block: Format<Module>,
    gamma: Context,
    index: Index<Module>,
    isModule: boolean = false
  ): JSX.Element {
    console.log("render block");
    block.definitions.forEach((definition) => {
      switch (definition.case) {
        case "term definition": {
          gamma = {
            ...gamma,
            nameTypes: gamma.nameTypes.set(
              definition.uniqueTermBinding.name,
              definition.type
            ),
            idNames: gamma.idNames.set(
              definition.uniqueTermBinding.id,
              definition.uniqueTermBinding.name
            ),
          };
          break;
        }
        case "data definition": {
          definition.constructors.forEach(
            (constructor) =>
              (gamma = {
                ...gamma,
                nameTypes: gamma.nameTypes.set(
                  constructor.uniqueTermBinding.name,
                  typeOfConstructor(
                    {
                      case: "type reference",
                      name: definition.typeBinding.name,
                    },
                    constructor
                  )
                ),
              })
          );
        }
      }
    });
    return (
      <div
        className={isModule ? `${styles.module} ${styles.block}` : styles.block}
      >
        {block.definitions.map((definition, i) =>
          renderDefinition(
            definition,
            gamma,
            concatIndex(index, { case: "definitions", i, index: here })
          )
        )}
        {renderTerm(
          block.body,
          gamma,
          block.type,
          concatIndex(index, { case: "body", index: here })
        )}
      </div>
    );
  }
  function renderDefinition(
    definition: Format<Definition>,
    gamma: Context,
    index: Index<Module>
  ): JSX.Element {
    console.log("render definition");
    switch (definition.case) {
      case "term definition": {
        return (
          <div className={`${styles.termDefinition} ${styles.definition}`}>
            {renderUniqueTermBinding(
              definition.uniqueTermBinding,
              gamma,
              concatIndex(index, { case: "uniqueTermBinding", index: here })
            )}
            {colon_punctuation}
            {renderType(
              definition.type,
              gamma,
              concatIndex(index, { case: "type", index: here })
            )}
            {assign_punctuation}
            {renderTerm(
              definition.term,
              gamma,
              definition.type,
              concatIndex(index, { case: "term", index: here })
            )}
          </div>
        );
      }
      case "data definition": {
        return (
          <div className={`${styles.dataDefinition} ${styles.definition}`}>
            {data_keyword}
            {renderTypeBinding(
              definition.typeBinding,
              gamma,
              concatIndex(index, { case: "typeBinding", index: here })
            )}
            {assign_punctuation}
            {definition.constructors.map((constructor, i) =>
              renderConstructor(
                { case: "type reference", name: definition.typeBinding.name },
                constructor,
                {
                  ...gamma,
                  types: gamma.types.push(definition.typeBinding.name),
                },
                concatIndex(index, { case: "constructors", i, index: here })
              )
            )}
          </div>
        );
      }
    }
  }
  function renderConstructor(
    typeReference: TypeReference,
    constructor: Format<Constructor>,
    gamma: Context,
    index: Index<Module>
  ) {
    console.log("render constructor");
    return (
      <div className={styles.constructor_}>
        {alt_punctuation}
        {renderUniqueTermBinding(
          constructor.uniqueTermBinding,
          gamma,
          concatIndex(index, { case: "uniqueTermBinding", index: here })
        )}
        {colon_punctuation}
        {renderType(
          defaultFormat(
            typeOfConstructor(typeReference, unformat<Constructor>(constructor))
          ),
          gamma,
          concatIndex(index, { case: "type", index: here })
        )}
      </div>
    );
  }
  function renderType(
    type: Format<Type>,
    gamma: Context,
    index: Index<Module>
  ): JSX.Element {
    console.log("render type");
    switch (type.case) {
      case "arrow": {
        return (
          <div className={`${styles.arrow} ${styles.type}`}>
            {lparen_punctuation}
            {type.parameters.map((param, i) =>
              renderParameter(
                param,
                gamma,
                concatIndex(index, { case: "parameters", i, index: here })
              )
            )}
            {rparen_punctuation}
            {arrow_punctuation}
            {renderType(
              type.output,
              gamma,
              concatIndex(index, { case: "output", index: here })
            )}
          </div>
        );
      }
      case "data": {
        return (
          <div className={`${styles.data} ${styles.type}`}>
            {renderTypeReference(
              type.typeReference,
              gamma,
              concatIndex(index, { case: "typeReference", index: here })
            )}
          </div>
        );
      }
      case "hole": {
        return (
          <div className={`${styles.hole} ${styles.type}`}>
            {renderHoleId(type.holeId)}
          </div>
        );
      }
    }
  }
  function renderTerm(
    term: Format<Term>,
    gamma: Context,
    type: Type,
    index: Index<Module>
  ): JSX.Element {
    console.log("render term: " + term.case);
    switch (term.case) {
      case "lambda": {
        // let type = infer(term, gamma) as ArrowType;
        let gammaBlock = gamma;
        term.termBindings.forEach((termBinding, i) => {
          gammaBlock = {
            ...gammaBlock,
            idNames: gammaBlock.idNames.set(
              termBinding.id,
              (type as ArrowType).parameters[i].label.name
            ),
          };
        });
        return (
          <div className={`${styles.lambda} ${styles.term}`}>
            {lparen_punctuation}
            {intercalate(
              term.termBindings.map((termBinding, i) =>
                renderTermBinding(
                  termBinding,
                  gamma,
                  concatIndex(index, { case: "termBindings", i, index: here }),
                  (type as ArrowType).parameters[i].label
                )
              ),
              comma_punctuation
            )}
            {rparen_punctuation}
            {arrow_punctuation}
            {renderBlock(
              term.block,
              gammaBlock,
              concatIndex(index, { case: "block", index: here })
            )}
          </div>
        );
      }
      case "neutral": {
        if (term.args.length === 0) {
          return (
            <div className={`${styles.neutral} ${styles.term}`}>
              {renderTermReference(
                term.termReference,
                gamma,
                concatIndex(index, { case: "termReference", index: here })
              )}
            </div>
          );
        } else {
          let arrowType: ArrowType = gamma.nameTypes.get(
            gamma.idNames.get(term.termReference.id) as string
          ) as ArrowType;
          return (
            <div className={`${styles.neutral} ${styles.term}`}>
              {renderTermReference(
                term.termReference,
                gamma,
                concatIndex(index, { case: "termReference", index: here })
              )}
              {lparen_punctuation}
              {intercalate(
                term.args.map((arg, i) =>
                  renderTerm(
                    arg,
                    gamma,
                    arrowType.parameters[i].type,
                    concatIndex(index, { case: "args", i, index: here })
                  )
                ),
                comma_punctuation
              )}
              {rparen_punctuation}
            </div>
          );
        }
      }
      case "match": {
        return (
          <div className={`${styles.match} ${styles.term}`}>
            {match_keyword}
            {renderTerm(
              term.term,
              gamma,
              { case: "data", typeReference: term.typeReference },
              concatIndex(index, { case: "term", index: here })
            )}
            {colon_punctuation}
            {renderType(
              { case: "data", typeReference: term.typeReference },
              gamma,
              concatIndex(index, { case: "typeReference", index: here })
            )}
            {with_keyword}
            {term.cases.map((case_, i) =>
              renderCase(
                case_,
                gamma,
                type,
                concatIndex(index, { case: "cases", i, index: here })
              )
            )}
          </div>
        );
      }
      case "hole": {
        return (
          <div className={`${styles.hole} ${styles.term}`}>
            {renderHoleId(term.holeId)}
          </div>
        );
      }
    }
  }
  function renderCase(
    case_: Format<Case>,
    gamma: Context,
    type: Type,
    index: Index<Module>
  ): JSX.Element {
    console.log("render case");
    let arrowType = gamma.nameTypes.get(
      gamma.idNames.get(case_.termReference.id) as string
    ) as ArrowType;
    let gammaBlock = gamma;
    case_.termBindings.forEach((termBinding, i) => {
      gammaBlock = {
        ...gammaBlock,
        idNames: gammaBlock.idNames.set(
          termBinding.id,
          arrowType.parameters[i].label.name
        ),
      };
    });
    return (
      <div className={styles.case}>
        {alt_punctuation}
        {renderTermReference(
          case_.termReference,
          gamma,
          concatIndex(index, { case: "termReference", index: here })
        )}
        {lparen_punctuation}
        {intercalate(
          case_.termBindings.map((termBinding, i) =>
            renderTermBinding(
              termBinding,
              gamma,
              concatIndex(index, { case: "termBindings", i, index: here }),
              arrowType.parameters[i].label
            )
          ),
          comma_punctuation
        )}
        {rparen_punctuation}
        {arrow_punctuation}
        {renderBlock(
          case_.block,
          gammaBlock,
          concatIndex(index, { case: "block", index: here })
        )}
      </div>
    );
  }
  function renderParameter(
    param: Format<Parameter>,
    gamma: Context,
    index: Index<Module>
  ): JSX.Element {
    console.log("render parameter");
    return (
      <div className={styles.parameter}>
        {renderLabel(param.label)}
        {colon_punctuation}
        {renderType(
          param.type,
          gamma,
          concatIndex(index, { case: "type", index: here })
        )}
      </div>
    );
  }
  function renderUniqueTermBinding(
    uniqueTermBinding: Format<UniqueTermBinding>,
    gamma: Context,
    index: Index<Module>
  ): JSX.Element {
    return (
      <div className={styles.uniqueTermBinding}>{uniqueTermBinding.name}</div>
    );
  }
  function renderTermBinding(
    termBinding: Format<TermBinding>,
    gamma: Context,
    index: Index<Module>,
    label: Label
  ): JSX.Element {
    return <div className={styles.termBinding}>{renderLabel(label)}</div>;
  }
  function renderTypeBinding(
    typeBinding: Format<TypeBinding>,
    gamma: Context,
    index: Index<Module>
  ): JSX.Element {
    return <div className={styles.typeBinding}>{typeBinding.name}</div>;
  }
  function renderTermReference(
    termReference: Format<TermReference>,
    gamma: Context,
    index: Index<Module>
  ): JSX.Element {
    return (
      <div className={styles.termReference}>
        {gamma.idNames.get(termReference.id) as string}
      </div>
    );
  }
  function renderTypeReference(
    typeReference: Format<TypeReference>,
    gamma: Context,
    index: Index<Module>
  ): JSX.Element {
    return <div className={styles.typeReference}>{typeReference.name}</div>;
  }
  function renderLabel(label: Format<Label>): JSX.Element {
    return <div className={styles.label}>{label.name}</div>;
  }
  function renderHoleId(holeId: Symbol): JSX.Element {
    return <div className={styles.holeId}>?</div>;
  }

  return (
    <div className={styles.Program}>
      {renderBlock(programState.module, emptyContext(), here)}
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
