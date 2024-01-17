open Util

type priority_queues = {
  high : Process.t Weak_ptr.t Lf_queue.t;
  normal : Process.t Weak_ptr.t Lf_queue.t;
  low : Process.t Weak_ptr.t Lf_queue.t;
}

let make_priority_queues () =
  {
    high = Lf_queue.create ();
    normal = Lf_queue.create ();
    low = Lf_queue.create ();
  }

type t = { alive : Proc_set.t; queue : priority_queues }

let create () = { queue = make_priority_queues (); alive = Proc_set.create () }
let size t = Proc_set.size t.alive
let is_empty t = size t = 0

let remove t proc =
  let ptr = Weak_ptr.make proc in
  Proc_set.remove t.alive ptr

let queue t proc =
  let ptr = Weak_ptr.make proc in
  if Proc_set.contains t.alive ptr then
    Log.trace (fun f -> f "Skipped process %a from run_queue" Pid.pp proc.pid)
  else (
    Log.trace (fun f -> f "Added process %a to run_queue" Pid.pp proc.pid);
    Proc_set.add t.alive ptr;
    match Atomic.get proc.flags.priority with
    | High -> Lf_queue.push t.queue.high ptr
    | Normal -> Lf_queue.push t.queue.normal ptr
    | Low -> Lf_queue.push t.queue.low ptr)

let next t =
  let queue =
    match
      ( Lf_queue.is_empty t.queue.high,
        Lf_queue.is_empty t.queue.normal,
        Lf_queue.is_empty t.queue.low )
    with
    | false, _, _ -> t.queue.high
    | _, false, _ -> t.queue.normal
    | _, _, _ -> t.queue.low
  in
  match Lf_queue.pop queue with
  | Some ptr ->
      Proc_set.remove t.alive ptr;
      Weak_ptr.get ptr
  | None -> None
