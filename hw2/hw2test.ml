let accept_all derivation string = Some (derivation, string)
let accept_empty_suffix derivation = function
   | [] -> Some (derivation, [])
   | _ -> None

type string_nonterminals =
  | Chars | Letter | A | B | C

let string_grammar =
  (Chars,
   function
     | Chars ->
         [[N Letter; N Chars];
          [N Letter]]
     | Letter ->
	  [[N A];
	  [N B];
	  [N C];
	  [T"("; N Chars; T")"]]
     | A ->
	  [[T"A"];
	  [T"a"]]
     | B ->
	  [[T"B"];
	  [T"b"]]
	 | C ->
	  [[T"C"];
	  [T"c"]])

let test_1 =
  ((parse_prefix string_grammar accept_all ["A"]) = Some ([(Chars, [N Letter]); (Letter, [N A]); (A, [T "A"])], []))
let test_2 =
  ((parse_prefix string_grammar accept_all ["A"; "b"; "C"]) = Some (
	 [(Chars, [N Letter; N Chars]); (Letter, [N A]); (A, [T "A"]);
     (Chars, [N Letter; N Chars]); (Letter, [N B]); (B, [T "b"]);
     (Chars, [N Letter]); (Letter, [N C]); (C, [T "C"])],
    []))
;;
