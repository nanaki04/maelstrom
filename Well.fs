namespace Maelstrom

module Well =
  open Reflection

  let private guard<'T> (update : Well<'T> -> Well<'T>) (guardians : list<int * WellGuardian<'T>>) =
    guardians
    |> List.map (fun (_, guardian) -> guardian)
    |> List.fold (fun next guardian -> fun well -> guardian next well) update
  let refreshLifewell<'W, 'A> (update : Well<'W> -> Well<'W>) (maelstrom : Maelstrom<'W, 'A>) =
    guard<'W> update maelstrom.wellGuardians maelstrom.lifewell
    |> fun well -> { maelstrom with lifewell = well }