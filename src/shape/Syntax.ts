import { List, Record, RecordOf } from "immutable";

export type Term = Universe | Pi | Lambda | Neutral | Hole

export type BlockProps = {
  bindings: List<{label: Label, signature: Term, value: Term}>,
  body: Term,
  format: Format
}
// export type Block = RecordOf<BlockProps>
// export const makeBlock = (props: BlockProps): Block => Record<BlockProps>(props)({})

export const defaultBlockProps: BlockProps = {}
export class Block extends Record(defaultBlockProps) {}

export type LambdaProps = {
  case: "lambda",
  parameters: List<{label: Label, domain: Term}>,
  body: Block,
  format: Format
}
export type Lambda = RecordOf<LambdaProps>
export const makeLambda = (props: LambdaProps): Lambda => Record<LambdaProps>(props)({})

export type UniverseProps = {
  case: "universe",
  level: number,
  format: Format
}
export type Universe = RecordOf<UniverseProps>
export const makeUniverse = (props: UniverseProps): Universe => Record<UniverseProps>(props)({})

export type PiProps = {
  case: "pi",
  parameters: List<{label: Label, domain: Term}>,
  codomain: Block,
  format: Format
}
export type Pi = RecordOf<PiProps>
export const makePi = (props: PiProps): Pi => Record<PiProps>(props)({})

export type NeutralProps = {
  case: "neutral",
  applicant: DeBruijn,
  arguments: List<Term>,
  format: Format
}
export type Neutral = RecordOf<NeutralProps>
export const makeNeutral = (props: NeutralProps): Neutral => Record<NeutralProps>(props)({})

export type HoleProps = {
  case: "hole",
  holeId: Symbol,
  weakening: DeBruijn,
  substitution: Substitution<DeBruijn, Term>,
  format: Format
}
export type Hole = RecordOf<HoleProps>
export const makeHole = (props: HoleProps): Hole => Record<HoleProps>(props)({})

// Variables references are DeBruijn levels
export type DeBruijn = number

export type Label = {value: string}

export function freshLabel(): Label {
  return {value: ""}
}

export function freshHole(): Hole {
  const holeId: unique symbol = Symbol()
  return makeHole({
    case: "hole",
    holeId,
    weakening: 0,
    substitution: List(),
    format: defaultFormat()
  })
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

export type Index = List<IndexStep>

export type IndexStep = 
  // block
  | "bindings" | "label" | "signature" | "value" | "body"
  // universe
  | "level"
  // pi
  | "parameters" | "label" | "domain" | "codomain"
  // lambda
  | "parameters" | "label" | "domain" | "body" 
  // neutral
  | "applicant" | "arguments" 
  // hole
  | "holeId" | "weakening" | "substitution"
  // list index
  | number

// ? should be deprecated by above, using record's built-in deep `.getIn`
// export type IndexStep =
//   | {
//       case: "block",
//       sub:
//         | {
//             case: "binding",
//             i: number,
//             sub: {case: "label"} | {case: "signature"} | {case: "value"},
//           }
//         | {case: "body"},
//     }
//   | {
//       case: "pi",
//       sub:
//         | {
//             case: "parameter",
//             i: number,
//             sub: {case: "label"} | {case: "signature"}
//           }
//         | {case: "codomain"}
//     }
//   | {
//       case: "lambda",
//       sub:
//         | {
//             case: "parameter",
//             i: number,
//             sub: {case: "label"} | {case: "signature"}
//           }
//         | {case: "body"}
//     }
//   | {
//       case: "neutral",
//       sub:
//         | {case: "applicant"}
//         | {case: "argument", i: number},
//     }

