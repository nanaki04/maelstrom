namespace Maelstrom

module WellGuardians =
  
  let watcher<'T, 'R when 'R : equality> (finder : 'T -> 'R) runner =
    fun (next : Well<'T> -> Well<'T>) (well : Well<'T>) ->
      next well
      |> fun refreshedWell -> (finder refreshedWell, refreshedWell)
      |> fun (result, refreshedWell) -> (result, refreshedWell, result = finder well)
      |> function
      | (_, refreshedWell, true) ->
        refreshedWell
      | (result, refreshedWell, _) ->
        runner result refreshedWell
        refreshedWell