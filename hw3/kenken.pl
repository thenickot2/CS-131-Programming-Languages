% Due to time constraints, I was not able to implement the
% plain_kenken solver. I severly underestimated the amount
% of time it would take to understand prolog to the extent
% required for this project, a mistake I will not make for
% project 4. The solver works in under .01 seconds.

kenken(N,C,T):- initMatrix(N, T),uniqueMatrix(T),doRules(N,C,T),label(T).
initMatrix(N, T) :-
	length(T, N),
	initRows(N, T).
initRows(N, []) .
initRows(N, [H | T]) :-
	length(H, N),
	fd_domain(H, 1, N),
	initRows(N, T).
uniqueMatrix(T) :-
	uniqueColumns(T),
	uniqueRows(T).
uniqueColumns([[] | T]).
uniqueColumns([[H|T]|Tt]) :-
	stripHeads([[H|T]|Tt], Heads, Tails),
	fd_all_different(Heads),
	uniqueColumns(Tails).
stripHeads([[H | T]], [H], [T]).
stripHeads([[H1 | T1] | Tail], Heads, JustTails) :-
	stripHeads(Tail, RestHeads, RestTails),
	Heads = [H1 | RestHeads],
	JustTails = [T1 | RestTails].
uniqueRows([]) .
uniqueRows([H|T]) :-
	fd_all_different(H),
	uniqueRows(T).
label([]) .
label([H | T]) :-
	fd_labeling(H),
	label(T).
	
doRules(_,[],_).
doRules(N,[C1|C2],T):- doSymbol(N,C1,T), doRules(N,C2,T).
doSymbol(N,C,T):- (plusParser(C,A,B),doSum(N,A,B,T));
	(subParser(C,A,J,K),doSub(N,A,J,K,T));
	(multParser(C,A,B),doMult(N,A,B,T));
	(divParser(C,A,J,K),doDiv(N,A,J,K,T)).
doSum(N,Result,Loc,T):- sum(0,N,Loc,T,Result).
sum(A,_,[],_,A).
sum(A,_,[],_,B):-A#=B.
sum(Total,N,[Loc1|Loc2],T,Result):-
	getCoords(Loc1,Y,X),
	getNum(T,Y,X,Num1),
	Num1 #< Result,
	Num1 #> 0,
	TheSum=Num1+Total,
	sum(TheSum,N,Loc2,T,Result).
doMult(N,Result,Loc,T):- mult(1,N,Loc,T,Result).
mult(A,_,[],_,A).
mult(A,_,[],_,B):-A#=B.
mult(Total,N,[Loc1|Loc2],T,Result):-
	getCoords(Loc1,Y,X),
	getNum(T,Y,X,Num1),
	Num1 #=< Result,
	Num1 #> 0,
	TheProd=Num1*Total,
	mult(TheProd,N,Loc2,T,Result).
doSub(N,Result,J,K,T):-
	getCoords(J,Y1,X1), getCoords(K,Y2,X2),
	getNum(T,Y1,X1,Num1), getNum(T,Y2,X2,Num2),
	Num1 #=< N, Num2 #=< N,
	Num1 #> 0, Num2 #> 0,
	(Result#=Num1-Num2;Result#=Num2-Num1).
doDiv(N,Result,J,K,T):-
	getCoords(J,Y1,X1), getCoords(K,Y2,X2),
	getNum(T,Y1,X1,Num1), getNum(T,Y2,X2,Num2),
	Num1 #=< N, Num2 #=< N,
	Num1 #> 0, Num2 #> 0,
	(Result#=Num1/Num2;Result#=Num2/Num1).
getCoords(-(A,B),A,B).
getNum(T,Row,Col,TheNum) :-
   nth(Row,T,RowList),
   nth(Col,RowList,TheNum).
plusParser(+(A,B),A,B).
multParser(*(A,B),A,B).
subParser(-(A,J,K),A,J,K).
divParser(/(A,J,K),A,J,K).