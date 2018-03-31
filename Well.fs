namespace Maelstrom

module Well =
  let private guard<'T> (update : Well<'T> -> Well<'T>) (guardians : list<int * WellGuardian<'T>>) =
    guardians
    |> List.map (fun (_, guardian) -> guardian)
    |> List.fold (fun next guardian -> fun well -> guardian next well) update
  let refreshLifewell<'T> (update : Well<'T> -> Well<'T>) (maelstrom : Maelstrom<'T>) =
    guard<'T> update maelstrom.wellGuardians maelstrom.lifewell
    |> fun well -> { maelstrom with lifewell = well }