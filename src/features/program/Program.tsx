import styles from "./Program.module.css";
import { ProgramState } from "./ProgramState";
import {
  addConstructor,
  adddIdName,
  addTermBinding,
  addTypeName,
  addUniqueTermBinding,
  ArrowType,
  Block,
  Case,
  Constructor,
  Context,
  Definition,
  emptyContext,
  Label,
  LambdaTerm,
  Module,
  Name,
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

const let_keyword = <span className={styles.keyword}>let</span>;
const data_keyword = <span className={styles.keyword}>data</span>;
const match_keyword = <span className={styles.keyword}>match</span>;
const with_keyword = <span className={styles.keyword}>with</span>;
const alt_punctuation = <span className={styles.punctuation}>|</span>;
const arrow_punctuation = <span className={styles.punctuation}>-&gt;</span>;
const mapsto_punctuation = <span className={styles.punctuation}>=&gt;</span>;
const lparen_punctuation = <span className={styles.punctuation}>(</span>;
const rparen_punctuation = <span className={styles.punctuation}>)</span>;
const assign_punctuation = <span className={styles.punctuation}>=</span>;
const colon_punctuation = <span className={styles.punctuation}>:</span>;
const space_punctuation = (
  <span className={`${styles.space} ${styles.punctuation}`}></span>
);
const comma_punctuation = (
  <span className={styles.punctuation}>,{space_punctuation}</span>
);

export default function Program() {
  var programState: ProgramState = new ProgramState();

  function renderModule(module_: Format<Module>): JSX.Element {
    let gamma: Context = emptyContext();
    module_.definitions.forEach((definition) => {
      switch (definition.case) {
        case "term definition": {
          gamma = addUniqueTermBinding(
            gamma,
            definition.uniqueTermBinding,
            definition.type
          );
          break;
        }
        case "data definition": {
          gamma = addTypeName(gamma, definition.typeBinding.name);
          definition.constructors.forEach(
            (constructor) =>
              (gamma = addConstructor(
                gamma,
                definition.typeBinding.name,
                constructor
              ))
          );
        }
      }
    });
    return (
      <div className={styles.block}>
        {module_.definitions.map((definition, i) =>
          renderDefinition(definition, gamma, {
            case: "definitions",
            i,
            index: here,
          })
        )}
      </div>
    );
  }

  function renderBlock(
    block: Format<Block>,
    gamma: Context,
    type: Type,
    index: Index<Module>
  ): JSX.Element {
    console.log("render block");
    block.definitions.forEach((definition) => {
      switch (definition.case) {
        case "term definition": {
          gamma = addUniqueTermBinding(
            gamma,
            definition.uniqueTermBinding,
            type
          );
          break;
        }
        case "data definition": {
          definition.constructors.forEach(
            (constructor) =>
              (gamma = addConstructor(
                gamma,
                definition.typeBinding.name,
                constructor
              ))
          );
        }
      }
    });
    return (
      <div className={styles.block}>
        {block.definitions.map((definition, i) =>
          renderDefinition(
            definition,
            gamma,
            concatIndex(index, { case: "definitions", i, index: here })
          )
        )}
        {renderTerm(
          block.term,
          gamma,
          type,
          concatIndex(index, { case: "term", index: here })
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
        switch (definition.type.case) {
          case "arrow": {
            let arrow: ArrowType = definition.type;
            let lambda: LambdaTerm = definition.term as LambdaTerm;
            let gammaBlock: Context = gamma;
            lambda.termBindings.forEach((termBinding, i) => {
              gammaBlock = addTermBinding(
                gammaBlock,
                termBinding,
                arrow.parameters[i].label.name,
                arrow.parameters[i].type
              );
            });
            return (
              <div className={`${styles.termDefinition} ${styles.definition}`}>
                {let_keyword}
                {space_punctuation}
                {renderUniqueTermBinding(
                  definition.uniqueTermBinding,
                  gamma,
                  concatIndex(index, { case: "uniqueTermBinding", index: here })
                )}
                {lparen_punctuation}
                {intercalate(
                  arrow.parameters.map((param, i) =>
                    renderParameter(
                      param,
                      gamma,
                      concatIndex(index, {
                        case: "type",
                        index: { case: "parameters", i, index: here },
                      })
                    )
                  ),
                  comma_punctuation
                )}
                {rparen_punctuation}
                {colon_punctuation}
                {space_punctuation}
                {renderType(
                  arrow.output,
                  gamma,
                  concatIndex(index, {
                    case: "type",
                    index: { case: "output", index: here },
                  })
                )}
                {space_punctuation}
                {assign_punctuation}
                {space_punctuation}
                {renderBlock(
                  lambda.block,
                  gammaBlock,
                  arrow.output,
                  concatIndex(index, {
                    case: "term",
                    index: { case: "block", index: here },
                  })
                )}
              </div>
            );
          }
          case "data":
          case "hole": {
            return (
              <div className={`${styles.termDefinition} ${styles.definition}`}>
                {let_keyword}
                {space_punctuation}
                {renderUniqueTermBinding(
                  definition.uniqueTermBinding,
                  gamma,
                  concatIndex(index, { case: "uniqueTermBinding", index: here })
                )}
                {space_punctuation}
                {colon_punctuation}
                {space_punctuation}
                {renderType(
                  definition.type,
                  gamma,
                  concatIndex(index, { case: "type", index: here })
                )}
                {space_punctuation}
                {assign_punctuation}
                {space_punctuation}
                {renderTerm(
                  definition.term,
                  gamma,
                  definition.type,
                  concatIndex(index, { case: "term", index: here })
                )}
              </div>
            );
          }
        }
        break;
      }
      case "data definition": {
        return (
          <div className={`${styles.dataDefinition} ${styles.definition}`}>
            {data_keyword}
            {space_punctuation}
            {renderTypeBinding(
              definition.typeBinding,
              gamma,
              concatIndex(index, { case: "typeBinding", index: here })
            )}
            {space_punctuation}
            {assign_punctuation}
            {space_punctuation}
            {intercalate(
              definition.constructors.map((constructor, i) =>
                renderConstructor(
                  definition.typeBinding.name,
                  constructor,
                  addTypeName(gamma, definition.typeBinding.name),
                  concatIndex(index, { case: "constructors", i, index: here })
                )
              ),
              space_punctuation
            )}
          </div>
        );
      }
    }
  }
  function renderConstructor(
    name: Name,
    constructor: Format<Constructor>,
    gamma: Context,
    index: Index<Module>
  ): JSX.Element {
    console.log("render constructor");
    if (constructor.parameters.length === 0) {
      return (
        <div className={styles.constructor_}>
          {alt_punctuation}
          {space_punctuation}
          {renderUniqueTermBinding(
            constructor.uniqueTermBinding,
            gamma,
            concatIndex(index, { case: "uniqueTermBinding", index: here })
          )}
        </div>
      );
    } else {
      return (
        <div className={styles.constructor_}>
          {alt_punctuation}
          {space_punctuation}
          {renderUniqueTermBinding(
            constructor.uniqueTermBinding,
            gamma,
            concatIndex(index, { case: "uniqueTermBinding", index: here })
          )}
          {lparen_punctuation}
          {constructor.parameters.map((param, i) =>
            renderParameter(
              param,
              gamma,
              concatIndex(index, { case: "parameters", i, index: here })
            )
          )}
          {rparen_punctuation}
        </div>
      );
    }
  }
  function renderType(
    type: Format<Type>,
    gamma: Context,
    index: Index<Module>
  ): JSX.Element {
    console.log("render type", type, index);
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
            {space_punctuation}
            {arrow_punctuation}
            {space_punctuation}
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
        let arrow: ArrowType = type as ArrowType;
        let gammaBlock = gamma;
        term.termBindings.forEach((termBinding, i) => {
          gammaBlock = adddIdName(
            gammaBlock,
            termBinding.id,
            arrow.parameters[i].label.name
          );
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
            {space_punctuation}
            {rparen_punctuation}
            {space_punctuation}
            {mapsto_punctuation}
            {renderBlock(
              term.block,
              gammaBlock,
              type,
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
          let arrowType: ArrowType = gamma.idTypes.get(
            term.termReference.id
          ) as ArrowType;
          return (
            <div className={`${styles.neutral} ${styles.term}`}>
              {renderTermReference(
                term.termReference,
                gamma,
                concatIndex(index, { case: "termReference", index: here })
              )}
              {space_punctuation}
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
            {space_punctuation}
            {renderTerm(
              term.term,
              gamma,
              { case: "data", typeReference: term.typeReference },
              concatIndex(index, { case: "term", index: here })
            )}
            {colon_punctuation}
            {space_punctuation}
            {renderTypeReference(
              term.typeReference,
              gamma,
              concatIndex(index, { case: "typeReference", index: here })
            )}
            {space_punctuation}
            {with_keyword}
            {space_punctuation}
            {intercalate(
              term.cases.map((case_, i) =>
                renderCase(
                  case_,
                  gamma,
                  type,
                  concatIndex(index, { case: "cases", i, index: here })
                )
              ),
              space_punctuation
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
    let arrowType = gamma.idTypes.get(case_.termReference.id) as ArrowType;
    let gammaBlock = gamma;
    case_.termBindings.forEach((termBinding, i) => {
      gammaBlock = addTermBinding(
        gamma,
        termBinding,
        arrowType.parameters[i].label.name,
        arrowType.parameters[i].type
      );
    });
    if (case_.termBindings.length === 0) {
      return (
        <div className={styles.case}>
          {alt_punctuation}
          {space_punctuation}
          {renderTermReference(
            case_.termReference,
            gamma,
            concatIndex(index, { case: "termReference", index: here })
          )}
          {space_punctuation}
          {mapsto_punctuation}
          {space_punctuation}
          {renderBlock(
            case_.block,
            gammaBlock,
            type,
            concatIndex(index, { case: "block", index: here })
          )}
        </div>
      );
    } else {
      return (
        <div className={styles.case}>
          {alt_punctuation}
          {space_punctuation}
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
          {space_punctuation}
          {mapsto_punctuation}
          {space_punctuation}
          {renderBlock(
            case_.block,
            gammaBlock,
            type,
            concatIndex(index, { case: "block", index: here })
          )}
        </div>
      );
    }
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
        {space_punctuation}
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
    <div className={styles.Program}>{renderModule(programState.module)}</div>
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

// function intersperseLeft<A>(list: A[], sep: A): A[] {
//   return list.flatMap((a) => [sep, a]);
// }
