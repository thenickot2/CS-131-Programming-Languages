let rec member a b = match b with
	[] -> false
	| x::xs -> (a=x) || member a xs;;

let rec remove a = function
	| [] -> []
	| x::xs -> if x=a then remove a xs else if (member a xs) then x::remove a xs else x::xs
	
let rec subset a b = match a with [] -> true
	| x::xs -> (member x b) && (subset xs b);;

let rec equal_sets a b = match (a,b) with ([],[]) -> true
	| (x,[]) -> false
	| ([],x) -> false
	| (x::xs,b) -> (member x b) && (equal_sets xs (remove x b));;
	
let proper_subset a b = (if equal_sets a b then false else subset a b);;

let rec set_diff a b = match b with [] -> a
	| x::xs -> if (member x a) then set_diff (remove x a) xs else set_diff a xs;;
	
let rec computed_fixed_point eq f x = if (eq x (f x)) then x
	else computed_fixed_point eq f (f x);;
	
let rec computed_periodic_point eq f p x = if (p=0) then x
	else if (eq x (f x)) then computed_periodic_point eq f (p - 1) x
	else computed_periodic_point eq f p (f x);;
	
	
	
type ('nonterminal, 'terminal) symbol =
	| N of 'nonterminal
	| T of 'terminal;;
	
let rec isTerminal terminals term = match term with
	N n -> if List.mem_assoc n terminals then true else false
	| T _ -> true;;
	
let rec parse_rule terminals rule = match rule with 
	(a, terminal_list) -> if List.for_all (isTerminal terminals) terminal_list then true else false;;
	
let rec add_terminals terminals rules =
    let updateTerminals = List.filter (parse_rule terminals) rules in
    if equal_sets updateTerminals terminals then updateTerminals else add_terminals updateTerminals rules;;

let member_reversed the_list item = member item the_list;;

let rec filter_blind_alleys g = match g with 
	(start, rules_list) ->
		let terminating_rules = add_terminals [] rules_list in
		let valid_rules = List.filter (member_reversed terminating_rules) rules_list in
		(start, valid_rules);;
		