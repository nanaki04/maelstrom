namespace Maelstrom

type Domain = string
type Invocation = string
type WaveLocation = Domain * Invocation
type Amplitude = Record
type Wave = WaveLocation * Amplitude

type Well<'T> = 'T

type Surge<'T> = Wave -> Well<'T> -> Well<'T>
type Ripple<'T> = (Wave -> Surge<'T>) -> Wave -> Surge<'T>
type WellGuardian<'T> = (Well<'T> -> Well<'T>) -> Well<'T> -> Well<'T>

type Tide<'T> = WaveLocation * Surge<'T>

type Maelstrom<'T> = {
  lifewell : Well<'T>;
  invoke : Wave -> Maelstrom<'T> -> Maelstrom<'T>;
  ripples : list<Ripple<'T>>;
  wellGuardians : list<int * WellGuardian<'T>>;
  _metaData : int
}

type Result =
| Ok
| Error of string
type Finder<'T, 'R> = Well<'T> -> 'R
type Runner<'T, 'R> = 'R -> Well<'T> -> Result