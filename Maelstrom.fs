namespace Maelstrom

module Mealstrom =
  type private Invoker<'T> = Wave -> Surge<'T>
  type private GroupedInvoker<'T> = Domain * Invoker<'T>

  let surgeId = fun _ well -> well

  let private invokeRipples<'T> (invoke: Invoker<'T>) (ripples : list<Ripple<'T>>) =
    ripples
    |> List.fold (fun next ripple -> fun wave -> ripple next wave) (fun wave -> invoke wave)
  
  let private groupTides<'T> (tides : list<Tide<'T>>) =
    tides
    |> Seq.groupBy (fun ((domain, _), _) -> domain)
    
  let private surgeOrNext<'T> (next : (_ -> Surge<'T>)) (((_, tideInvocation), surge) : Tide<'T>) (wave : Wave) =
    let ((_, waveInvocation), _) = wave
    if tideInvocation = waveInvocation then surge else next wave
  let private funnelTides<'T> ((domain : Domain), (tides : seq<Tide<'T>>)) =
    (domain, Seq.fold surgeOrNext (fun _ -> surgeId) tides)
    
  let private invokeSurge<'T> (invoker : Invoker<'T>) (wave : Wave) (maelstrom : Maelstrom<'T>) =
    let surge : Surge<'T> = invokeRipples invoker maelstrom.ripples wave
    Well.refreshLifewell<'T> (surge wave) maelstrom
    
  let private invokeOrNext<'T> (next : (_ -> Surge<'T>)) ((domain : Domain), (invoker : Invoker<'T>)) (wave : Wave) =
    let ((waveDomain, _), _) = wave
    if waveDomain = domain then invoker wave else next wave
  let private funnelInvokers<'T> (invokers : seq<GroupedInvoker<'T>>) =
    invokers
    |> Seq.fold invokeOrNext<'T> (fun _ -> surgeId)
  
  let private invokeTides<'T> (tides : list<Tide<'T>>) =
    tides
    |> groupTides<'T>
    |> Seq.map funnelTides<'T>
    |> funnelInvokers<'T>
    |> invokeSurge<'T>
    
  let invoke<'T> lifewell tides ripples guardians =
    {
      lifewell = lifewell;
      invoke = invokeTides<'T> tides;
      ripples = List.rev ripples;
      wellGuardians = guardians |> List.map (fun guardian -> (0, guardian));
      _metaData = 0
    }
    
  let flow<'T> (wave : Wave) (maelstrom : Maelstrom<'T>) =
    maelstrom.invoke wave maelstrom
    
  let private unguard wellGuardianId maelstrom =
    { maelstrom with
        wellGuardians = List.filter (fun (id, _) -> (wellGuardianId = id) = false) maelstrom.wellGuardians
    }
        
  let guard wellGuardian maelstrom =
    let guardianId = maelstrom._metaData
    (unguard guardianId, {
      maelstrom with
        wellGuardians = (guardianId, wellGuardian) :: maelstrom.wellGuardians;
        _metaData = maelstrom._metaData + 1
    })
