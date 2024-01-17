module Exn = struct
  exception Name_already_registered of string * Pid.t
end

type t = {
  processes : (string, Pid.t) Hashtbl.t;
  names : (Pid.t, string) Hashtbl.t;
  lock : Mutex.t;
}

let create () =
  {
    lock = Mutex.create ();
    processes = Hashtbl.create 0;
    names = Hashtbl.create 0;
  }

let register t name pid =
  Mutex.protect t.lock @@ fun () ->
  if Hashtbl.mem t.processes name then
    raise (Exn.Name_already_registered (name, pid));
  Hashtbl.add t.processes name pid;
  Hashtbl.add t.names pid name

let unregister t name =
  Mutex.protect t.lock @@ fun () ->
  let pid = Hashtbl.find t.processes name in
  Hashtbl.remove t.processes name;
  Hashtbl.remove t.names pid

let remove t pid =
  Mutex.protect t.lock @@ fun () ->
  (match Hashtbl.find_opt t.names pid with
  | Some name -> Hashtbl.remove t.processes name
  | None -> ());
  Hashtbl.remove t.names pid

let find_pid t name =
  Mutex.protect t.lock @@ fun () -> Hashtbl.find_opt t.processes name
