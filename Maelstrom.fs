namespace Maelstrom

module Mealstrom =
  open Reflection
  
  type private IdGenerator () =
    let mutable lastId = 0
    
    member m.Next () =
      lastId <- lastId + 1
      lastId
  
  type private Invoker<'W, 'A> = Wave<'A> -> Surge<'W, 'A>
  type private GroupedInvoker<'W, 'A> = Domain * Invoker<'W, 'A>

  let surgeId = fun _ well -> well

  let private invokeRipples<'W, 'A> (invoke: Invoker<'W, 'A>) (ripples : list<Ripple<'W, 'A>>) =
    ripples
    |> List.fold (fun next ripple -> fun wave -> ripple next wave) (fun wave -> invoke wave)
  
  let private groupTides<'W, 'A> (tides : list<Tide<'W, 'A>>) =
    tides
    |> Seq.groupBy (fun ((domain, _), _) -> domain)
    
  let private surgeOrNext<'W, 'A> (next : (_ -> Surge<'W, 'A>)) (((_, tideInvocation), surge) : Tide<'W, 'A>) (wave : Wave<'A>) =
    let ((_, waveInvocation), _) = wave
    if tideInvocation = waveInvocation then surge else next wave
  let private funnelTides<'W, 'A> ((domain : Domain), (tides : seq<Tide<'W, 'A>>)) =
    (domain, Seq.fold surgeOrNext (fun _ -> surgeId) tides)
    
  let private invokeSurge<'W, 'A> (invoker : Invoker<'W, 'A>) (wave : Wave<'A>) (maelstrom : Maelstrom<'W, 'A>) =
    let surge : Surge<'W, 'A> = invokeRipples invoker maelstrom.ripples wave
    Well.refreshLifewell<'W, 'A> (surge wave) maelstrom
    
  let private invokeOrNext<'W, 'A> (next : (_ -> Surge<'W, 'A>)) ((domain : Domain), (invoker : Invoker<'W, 'A>)) (wave : Wave<'A>) =
    let ((waveDomain, _), _) = wave
    if waveDomain = domain then invoker wave else next wave
  let private funnelInvokers<'W, 'A> (invokers : seq<GroupedInvoker<'W, 'A>>) =
    invokers
    |> Seq.fold invokeOrNext<'W, 'A> (fun _ -> surgeId)
  
  let private invokeTides<'W, 'A> (tides : list<Tide<'W, 'A>>) =
    tides
    |> groupTides<'W, 'A>
    |> Seq.map funnelTides<'W, 'A>
    |> funnelInvokers<'W, 'A>
    |> invokeSurge<'W, 'A>
    
  let private flow<'W, 'A> (wave : Wave<'A>) (maelstrom : Maelstrom<'W, 'A>) =
    maelstrom
    |> clear
    |> maelstrom.invoke wave
    
  let private unguard wellGuardianId maelstrom =
    { 
      maelstrom with
        wellGuardians = List.filter (fun (id, _) -> (wellGuardianId = id) = false) maelstrom.wellGuardians
    }
        
  let private guard guardianId wellGuardian maelstrom =
    {
      maelstrom with
        wellGuardians = (guardianId, wellGuardian) :: maelstrom.wellGuardians;
    }
    
  type private WaveManager<'W, 'A> (maelstrom) =
    let mutable queue : (Maelstrom<'W, 'A> -> Maelstrom<'W, 'A>) list = []
    let mutable maelstrom : Maelstrom<'W, 'A> = maelstrom
    let idGenerator = new IdGenerator ()
    
    let rec next () =
      if queue.Length > 0 then
        let command = List.head queue
        maelstrom <- command maelstrom
        queue <- List.tail queue
        next ()
      else
        ()
            
    let enqueue command =
      queue <- command::(List.rev queue) |> List.rev
      if queue.Length = 1 then next () else ()
      
    member m.Flow wave =
      enqueue <| flow wave
    
    member m.Guard wellGuardian =
      let guardianId = idGenerator.Next ()
      enqueue <| guard guardianId wellGuardian
      fun () -> enqueue <| unguard guardianId
      
    member m.Fetch () =
      let { lifewell = well } = maelstrom
      well
          
  let invoke<'W, 'A> lifewell tides ripples guardians =
    let maelstrom = {
      lifewell = lifewell;
      invoke = invokeTides<'W, 'A> tides;
      ripples = List.rev ripples;
      wellGuardians = guardians |> List.map (fun guardian -> (0, guardian));
      reflection = List.empty;
    }
    
    let waveManager = new WaveManager<'W, 'A> (maelstrom)
    (waveManager.Flow, waveManager.Fetch, waveManager.Guard)