namespace Maelstrom

type Domain = string
type Invocation = string
type WaveLocation = Domain * Invocation
type Amplitude<'T> = 'T
type Wave<'T> = WaveLocation * Amplitude<'T>

type Well<'T> = 'T

type Surge<'W, 'A> = Wave<'A> -> Well<'W> -> Well<'W>
type Ripple<'W, 'A> = (Wave<'A> -> Surge<'W, 'A>) -> Wave<'A> -> Surge<'W, 'A>
type WellGuardian<'T> = (Well<'T> -> Well<'T>) -> Well<'T> -> Well<'T>

type Tide<'W, 'A> = WaveLocation * Surge<'W, 'A>

type Maelstrom<'W, 'A> = {
  lifewell : Well<'W>;
  invoke : Wave<'A> -> Maelstrom<'W, 'A> -> Maelstrom<'W, 'A>;
  ripples : list<Ripple<'W, 'A>>;
  wellGuardians : list<int * WellGuardian<'W>>;
  reflection : list<string>;
}

type Result =
| Ok
| Error of string
type Finder<'T, 'R> = Well<'T> -> 'R
type Runner<'T, 'R> = 'R -> Well<'T> -> Result