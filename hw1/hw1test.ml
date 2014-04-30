let member_test0 = member 1 [1;2;3]
let member_test1 = member 3 [1;2;3]
let member_test2 = member 4 [1;2;3]
let member_test3 = member 2 [1;2;2;3]

let remove_test0 = remove 1 [1;2;3]
let remove_test1 = remove 2 [1;2;2;3]
let remove_test2 = remove 4 [1;2;2;3]

let subset_test0 = subset [1] [1;2;3]
let subset_test1 = subset [1;3] [1;2;3]
let subset_test2 = subset [1;2;3;] [1;2;3]
let subset_test3 = subset [1;2;3;4] [1;2;3]

let proper_subset_test0 = proper_subset [1;2;3] [1;2;3]

let set_diff_test0 = set_diff [] [1;2;3]
let set_diff_test1 = set_diff [1;2;2] [1;2;3]
let set_diff_test2 = set_diff [1;2;3] [1;2;2]
let set_diff_test2 = set_diff [1;2;3] []

let computed_fixed_point_test0 = computed_fixed_point (=) (fun x -> x *. x) 10. = infinity
let computed_fixed_point_test1 = computed_fixed_point (=) (fun x -> x * x) 1 = 1

let computed_periodic_point_test0 = computed_periodic_point (=) (fun x -> x *x) 1 1 = 1

type awksub_nonterminals =
  | Expr | Lvalue | Incrop | Binop | Num

let awksub_rules =
   [Expr, [T"("; N Expr; T")"];
    Expr, [N Num];
    Expr, [N Expr; N Binop; N Expr];
    Expr, [N Lvalue];
    Expr, [N Incrop; N Lvalue];
    Expr, [N Lvalue; N Incrop];
    Lvalue, [T"$"; N Expr];
    Incrop, [T"++"];
    Incrop, [T"--"];
    Binop, [T"+"];
    Binop, [T"-"];
    Num, [T"0"];
    Num, [T"1"];
    Num, [T"2"];
    Num, [T"3"];
    Num, [T"4"];
    Num, [T"5"];
    Num, [T"6"];
    Num, [T"7"];
    Num, [T"8"];
    Num, [T"9"]]

let filter_blind_alleys_test0 = filter_blind_alleys ("3+3", awksub_rules) = ("3+3", awksub_rules)
let filter_blind_alleys_test1 = filter_blind_alleys ("3+3+", awksub_rules) = ("3+3", awksub_rules)
;;