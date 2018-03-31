namespace Maelstrom

module WellGuardians =
  
  let watcher<'T, 'R> finder runner =
    fun next well ->
      next well
      |> fun refreshedWell -> (finder refreshedWell, refreshedWell)
      |> fun (result, refreshedWell) -> (result, refreshedWell, result = finder well)
      |> function
      | (_, refreshedWell, true) ->
        refreshedWell
      | (result, refreshedWell, _) ->
        runner result refreshedWell |> ignore
        refreshedWell