(* PART 1 *)
type ('nonterminal, 'terminal) symbol =
	| N of 'nonterminal
	| T of 'terminal;;

let rec convert_helper rule_list acceptor nts = match rule_list with
	|[] -> acceptor
	|(head::tail) -> match head with
		(a,b) -> if a == nts then convert_helper tail (acceptor@[b]) nts 
			else convert_helper tail acceptor nts;;

let convert_grammar gram1 = match gram1 with
	(start,rule_list) -> (start, convert_helper rule_list []);;

(* PART 2 *)
let rec rule_matcher rule_list current rule rem_rules fragment derivation =
	match fragment with
	|[] -> None
	| head::tail -> match rule with
		| T term ->
			if term = head then
			and_matcher rule_list current rem_rules tail derivation
			else None
		| N nonterm -> matcher rule_list nonterm fragment derivation
and and_matcher rule_list current rules fragment derivation = match rules with
	| [] -> Some(derivation, fragment)
	| head::tail ->
		let temp = rule_matcher rule_list current head tail fragment derivation in
		match temp with
			| None -> None
			| Some(d,f) -> and_matcher rule_list current tail f d
and or_matcher rule_list current rules fragment derivation = 
	match rules with
	| [] -> None
	| head::tail ->
		let temp = and_matcher rule_list current head fragment (derivation@[(current,head)]) in
		match temp with
			| None -> or_matcher rule_list current tail fragment derivation
			| Some(deriv,frag_remaining) -> Some(deriv,frag_remaining)
and matcher rule_list current fragment derivation = match (rule_list current) with
	| [] -> None
	| rules -> or_matcher rule_list current rules fragment derivation
;;
let rec parse_prefix gram acceptor fragment = match gram with (start, rule_list) ->
	let b = matcher rule_list start fragment [] in
	match b with
		| None -> None
		| Some(deriv, frag_part_remaining) -> acceptor deriv frag_part_remaining
;;