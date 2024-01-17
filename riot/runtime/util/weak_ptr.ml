type 'a t = 'a Weak.t

let make x =
  let weak_arr = Weak.create 1 in
  Weak.set weak_arr 0 (Some x);
  weak_arr

let get weak_ptr = Weak.get weak_ptr 0
