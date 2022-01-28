// A transformation is a function that inputs a Term and outputs a Term.

import { Ctx, Term } from "./Grammar";

type State = {freshHoleId: number}

type Transformation = (state: State, gamma: Ctx, alpha: Term, a: Term) => [State, Term]
