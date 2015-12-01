
let remove_non_alpha = Str.global_replace (Str.regexp "[^a-z ]+") ""

let explode = Str.split (Str.regexp " ") 

let strip (ans:string) : string =
  let exp_lower = explode (String.trim (String.lowercase ans)) in
  let clean a x = if x<>"" then a ^ (x ^ " ") else a in
    String.trim (List.fold_left clean "" (List.map remove_non_alpha exp_lower))
