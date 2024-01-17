open Riot

module Test_app = struct
  [@@@warning "-8"]

  type Riot.Message.t += Loop_stop

  let loop count =
    match receive () with
    | Loop_stop -> Log.debug (fun f -> f "dead at %d%!" count)

  let main t0 () =
    Logger.info (fun f -> f "boot test app");
    let pids =
      List.init 100_000 (fun _i ->
          spawn (fun () ->
              Logger.debug (fun f -> f "spawned %a" Pid.pp (self ()));
              loop 0))
    in

    let rec alive_wait_loop pids =
      match pids with
      | [] -> ()
      | pid :: pids' ->
          if Process.is_alive pid then alive_wait_loop pids'
          else alive_wait_loop pids
    in
    alive_wait_loop pids;

    Logger.info (fun f ->
        let t1 = Ptime_clock.now () in
        let delta = Ptime.diff t1 t0 in
        let delta = Ptime.Span.to_float_s delta in
        f "spawned %d processes in %.3fs" (List.length pids) delta);

    List.iter (fun pid -> send pid Loop_stop) pids;

    wait_pids pids;

    Logger.info (fun f ->
        let t1 = Ptime_clock.now () in
        let delta = Ptime.diff t1 t0 in
        let delta = Ptime.Span.to_float_s delta in
        f "spawned/awaited %d processes in %.3fs" (List.length pids) delta);

    let rec wait_loop () =
      Logger.info (fun f -> f "Counting processes...");
      let proc_count = Seq.length (processes ()) in
      Logger.info (fun f -> f "%d processes left" proc_count);
      sleep 0.1;
      wait_loop ()
    in
    wait_loop ()

  let start () =
    Runtime.set_log_level (Some Info);
    Logger.set_log_level (Some Info);
    let t0 = Ptime_clock.now () in
    Ok (spawn_link (main t0))
end

let () = Riot.start ~workers:0 ~apps:[ (module Logger); (module Test_app) ] ()
