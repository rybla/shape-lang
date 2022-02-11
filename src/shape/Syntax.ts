import { List } from "immutable";

// Program

export type Program = List<Declaration>

export type Declaration = DeclareData | DeclareType | DeclareTerm

export type DeclareTerm = {
  label: Label,
  signature: Type, 
  value: Block,
  format: Format
}

export type DeclareType = {
  label: Label,
  parameters: List<Type>,
  value: Type,
  format: Format
}

export type DeclareData = {
  label: Label,
  parameters: List<Type>,
  constructors: List<Constructor>,
  format: Format
}

export type Constructor = {
  label: Label,
  parameters: List<Type>,
  codomain: Type,
  format: Format
}

// Kind

export type Kind = ArrowKind | UnitKind | HoleKind

export type ArrowKind = {
  case: "arrow",
  arguments: List<Kind>,
  codomain: Kind,
  format: Format
}

export type UnitKind = {
  case: "unit",
  format: Format
}

export type HoleKind = {
  case: "hole",
  holeId: Symbol,
  format: Format
}

// Type

export type Type = ArrowType | NeutralType | HoleType

export type ArrowType = {
  case: "arrow",
  arguments: List<Type>,
  codomain: Type,
  format: Format
}

export type NeutralType = {
  case: "neutral",
  applicant: Label, 
  arguments: List<Type>,
  format: Format
}

export type HoleType = {
  case: "hole",
  holeId: Symbol,
  format: Format
}

// Block

export type Block = {
  bindings: List<BlockBinding>,
  body: Term,
  format: Format
}

export type BlockBinding = {
  label: Label,
  signature: Type,
  value: Term,
  format: Format
}

// Term

export type Term = Lambda |  Neutral | Match | Hole

export type Lambda = {
  case: "lambda",
  parameters: List<LambdaParameter>,
  body: Block,
  format: Format
}

export type LambdaParameter = {
  label: Label,
  domain: Type,
  format: Format
}

export type Neutral = {
  case: "neutral",
  applicant: Label,
  arguments: List<Term>,
  format: Format
}

export type Match = {
  case: "match",
  argument: Term,
  branches: List<MatchBranch>,
  format: Format
}

export type MatchBranch = {
  pattern: MatchPattern,
  body: Block,
  format: Format
}

export type MatchPattern = {
  label: Label,
  paramaters: List<Label>,
  format: Format
}

export type Hole = {
  case: "hole",
  holeId: Symbol,
  weakening: List<Label>,
  substitution: Substitution<Label, Term>,
  format: Format
}

// Label

export type Label = {value: string}

export function freshLabel(): Label {
  return {value: ""}
}

export function freshHole(): Hole {
  const holeId: unique symbol = Symbol()
  return {
    case: "hole",
    holeId,
    weakening: List(),
    substitution: List(),
    format: defaultFormat()
  }
}

// Format

export type Format = {
  indented?: boolean
  unannotated?: boolean
}

export function defaultFormat(): Format {
  return {}
}

// Context

export type Context = List<ContextItem>
export type ContextItem = {
  label: Label,
  signature: Term,
  value: Term | undefined
}

// Substitution

export type Substitution<A, B> = List<SubstitutionItem<A, B>>
export type SubstitutionItem<A, B> = [A, B]

// Index

export type Index = List<IndexProgram | IndexDeclaration | IndexKind | IndexType | IndexTerm>

export type IndexProgram =
  | {case: "here"}
  | {
      case: "declaration",
      i: number,
      index: IndexDeclaration
    }

export type IndexDeclaration =
  | {case: "here"}
  | {
      case: "data",
      sub:
        | {case: "label"}
        | {case: "parameter", i: number, index: IndexType}
        | {case: "constructor", i: number, index: IndexConstructor}
    }
  | {
      case: "type",
      sub:
        | {case: "label"}
        | {case: "parameter", i: number}
        | {case: "value", index: IndexType}
    }
  | {
      case: "term",
      sub:
        | {case: "label"}
        | {case: "signature", index: IndexType}
        | {case: "value", index: IndexTerm}
    }

export type IndexConstructor = 
  | {case: "here"}
  | {case: "label"}
  | {case: "parameter", i: number, index: IndexType}
  | {case: "codomain", index: IndexType}

export type IndexKind = 
  | {case: "here"}
  | {
      case: "arrow",
      sub:
        | {case: "argument", i: number, index: IndexKind}
        | {case: "codomain", index: IndexKind}
    }

export type IndexType = 
  | {case: "here"}
  | {
      case: "arrow",
      sub:
        | {case: "argument", i: number, index: IndexType}
        | {case: "codomain", index: IndexType}
    }
  | {
      case: "neutral",
      sub:
        | {case: "applicant"}
        | {case: "argument", i: number, index: IndexType}
    }

export type IndexBlock = 
  | {case: "here"}
  | {case: "binding", i: number, index: IndexBlockBinding}
  | {case: "body", index: IndexTerm}

export type IndexBlockBinding = 
  | {case: "here"}
  | {case: "label"}
  | {case: "signature", index: IndexType}
  | {case: "value", index: IndexTerm}
  | {case: "here"}

export type IndexTerm = 
  | {case: "here"}
  | {
      case: "lambda",
      sub:
        | {case: "parameter", i: number, index: IndexLambdaParameter}
        | {case: "body", index: IndexBlock}
    }
  | {
      case: "neutral",
      sub:
        | {case: "applicant"}
        | {case: "argument", i: number, index: IndexTerm}
    }
  | {
      case: "match",
      sub:
        | {case: "argument", index: IndexTerm}
        | {case: "branch", i: number, index: IndexMatchBranch}
    }
  | {case: "here"}

export type IndexMatchBranch = 
  | {case: "here"}
  | {case: "pattern", index: IndexMatchPattern}
  | {case: "body", index: IndexTerm}

export type IndexMatchPattern = 
  | {case: "here"}
  | {case: "label"}
  | {case: "parameter", i: number}

export type IndexLambdaParameter =
  | {case: "here"}
  | {case: "label"}
  | {case: "domain"}
