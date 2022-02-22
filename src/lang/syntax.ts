import { List, Map } from "immutable";

// Syntax

export type Syntax =
  | Module
  | Block
  | Definition
  | Constructor
  | Type
  | Term
  | Case
  | Parameter
  | Label
  | TermBinding
  | UniqueTermBinding
  | TermReference
  | TypeBinding
  | TypeReference;

// Module

// The `Module` is at the top-level.
export type Module = {
  case: "module";
  definitions: Definition[];
};

// Block

export type Block = {
  case: "block";
  definitions: Definition[];
  term: Term;
};

// Definition

export type Definition = DataDefinition | TermDefinition;

export type TermDefinition = {
  case: "term definition";
  uniqueTermBinding: UniqueTermBinding;
  type: Type;
  term: Term;
};

export type DataDefinition = {
  case: "data definition";
  typeBinding: TypeBinding;
  constructors: Constructor[];
};

export type Constructor = {
  case: "constructor";
  uniqueTermBinding: UniqueTermBinding;
  parameters: Parameter[];
};

export const typeOfConstructor = (name: Name, constructor: Constructor): Type =>
  constructor.parameters.length > 0
    ? {
        case: "arrow",
        parameters: constructor.parameters,
        output: {
          case: "data",
          typeReference: { case: "type reference", name },
        },
      }
    : { case: "data", typeReference: { case: "type reference", name } };

// Type

export type Type = ArrowType | BaseType;
export type BaseType = DataType | HoleType;

export type ArrowType = {
  case: "arrow";
  parameters: Parameter[];
  output: DataType | HoleType;
};

export type DataType = {
  case: "data";
  typeReference: TypeReference;
};

export type HoleType = {
  case: "hole";
  holeId: HoleId;
  weakening: Type[];
};

// Term

export type Term = LambdaTerm | NeutralTerm | MatchTerm | HoleTerm;

export type LambdaTerm = {
  case: "lambda";
  termBindings: TermBinding[];
  block: Block;
};

export type NeutralTerm = {
  case: "neutral";
  termReference: TermReference;
  args: Term[];
};

export type MatchTerm = {
  case: "match";
  typeReference: TypeReference;
  term: Term;
  cases: Case[];
};

export type Case = {
  case: "case";
  termReference: TermReference; // of the constructor
  termBindings: TermBinding[];
  block: Block;
};

export type HoleTerm = {
  case: "hole";
  holeId: HoleId;
};

// Parameter

export type Parameter = {
  case: "parameter";
  label: Label;
  type: Type;
};

export type Label = {
  case: "label";
  name: Name;
};

// Binding and UniqueTermBinding

export type TermBinding = {
  case: "term binding";
  id: Id;
};

export type UniqueTermBinding = {
  case: "unique term binding";
  name: Name;
  id: Id;
};

export type TypeBinding = {
  case: "type binding";
  name: Name;
};

// Reference

export type TermReference = {
  case: "term reference";
  id: Id;
};

export type TypeReference = {
  case: "type reference";
  name: Name;
};

// Name

export type Name = string;

// Id (unique)

export type Id = Symbol;

// HoleId (unique)

export type HoleId = Symbol;

// Fresh

export function freshBlock(): Block {
  return {
    case: "block",
    definitions: [],
    term: freshHoleTerm(),
  };
}

export function freshHoleTerm(): HoleTerm {
  const holeId: unique symbol = Symbol();
  return {
    case: "hole",
    holeId,
  };
}

export function freshHoleType(): HoleType {
  const holeId: unique symbol = Symbol();
  return {
    case: "hole",
    holeId,
    weakening: [],
  };
}

export function freshId(): Id {
  const id: unique symbol = Symbol();
  return id;
}

export function freshName(): Name {
  throw new Error("umimplemented: freshName");
}

// export function freshName(): Name {
//   return ""
// }

// export function freshHoleTerm(): HoleTerm {
//   const holeId: unique symbol = Symbol()
//   return {
//     case: "hole",
//     holeId,
//     weakening: [],
//     substitution: []
//   }
// }

// export function freshHoleType(): HoleType {
//   const holeId: unique symbol = Symbol()
//   return {
//     case: "hole",
//     holeId
//   }
// }

// // Creates an empty block (0 bindings) with a fresh hole
// export function freshBlock(): Block {
//   return {
//     case: "block",
//     definitions: [],
//     term: freshHoleTerm()
//   }
// }

// export function freshParameter(domain: Type): Parameter {
//   return {
//     case: "parameter",
//     name: freshName(),
//     domain
//   }
// }

// Context

export type Context = {
  types: List<Name>;
  idTypes: Map<Id, Type>;
  nameIds: Map<Name, List<Id>>;
  idNames: Map<Id, Name>;
};

export function addTypeName(gamma: Context, name: Name): Context {
  return {
    ...gamma,
    types: gamma.types.push(name),
  };
}

export function addIdType(gamma: Context, id: Id, type: Type): Context {
  return {
    ...gamma,
    idTypes: gamma.idTypes.set(id, type),
  };
}

export function addNameId(gamma: Context, name: Name, id: Id): Context {
  return {
    ...gamma,
    nameIds: gamma.nameIds.update(name, (ids) =>
      ids === undefined ? List([id]) : ids.push(id)
    ),
  };
}

export function adddIdName(gamma: Context, id: Id, name: Name): Context {
  return {
    ...gamma,
    idNames: gamma.idNames.set(id, name),
  };
}

export function addTermBinding(
  gamma: Context,
  termBinding: TermBinding,
  name: Name,
  type: Type
): Context {
  gamma = addNameId(gamma, name, termBinding.id);
  gamma = adddIdName(gamma, termBinding.id, name);
  gamma = addIdType(gamma, termBinding.id, type);
  return gamma;
}

export function addUniqueTermBinding(
  gamma: Context,
  uniqueTermBinding: UniqueTermBinding,
  type: Type
): Context {
  gamma = addNameId(gamma, uniqueTermBinding.name, uniqueTermBinding.id);
  gamma = adddIdName(gamma, uniqueTermBinding.id, uniqueTermBinding.name);
  gamma = addIdType(gamma, uniqueTermBinding.id, type);
  return gamma;
}

export function addConstructor(
  gamma: Context,
  name: Name,
  constructor: Constructor
): Context {
  gamma = addUniqueTermBinding(
    gamma,
    constructor.uniqueTermBinding,
    typeOfConstructor(name, constructor)
  );
  return gamma;
}

export function emptyContext(): Context {
  return { types: List(), idTypes: Map(), nameIds: Map(), idNames: Map() };
}

// Weakening

export type Weakening = Id[];

// Substitution

export type Substitution<A, B> = SubstitutionItem<A, B>[];
export type SubstitutionItem<A, B> = {
  key: A;
  value: B;
};
