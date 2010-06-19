let rec string_of_template (template: string) (values: string list) : string =
  try
    let re = Str.regexp "%s" in
    let first_index = Str.search_forward re template 0 in
    let head = String.sub template 0 first_index in
    let next_index = first_index + 2 in
    let rest = String.sub template next_index ((String.length template) - next_index) in
    let replaced_rest = string_of_template rest (List.tl values) in
    head ^ (List.hd values) ^ replaced_rest
  with
  | Not_found -> template
  | Failure _ -> invalid_arg "Not enough values in replacement list."

let cannot_reach file func = 
  failwith (string_of_template "[%s] In function %s: Cannot reach this point!" [file; func])

let invalid_arg file reason =
  invalid_arg (string_of_template "[%s] %s" [file; reason])
  