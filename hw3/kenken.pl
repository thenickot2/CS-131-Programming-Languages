kenken(N,C,T):- matrix(T,N),doRules(N,C,T).
matrix(T, N) :-
        length(T, N),
        maplist(length_(N), T).
length_(L, Ls) :- length(Ls, L).
checkValid([]).
checkValid([T|T2]):-
	T #>0, T#<255, valid(T2).
doRules(_,[],_).
doRules(N,[C1|C2],T):- doSymbol(N,C1,T), doRules(N,C2,T).
doSymbol(N,C,T):- (plusParser(C,A,B),doSum(N,A,B,T));
	(subParser(C,A,J,K),doSub(N,A,J,K,T));
	(multParser(C,A,B),doMult(N,A,B,T));
	(divParser(C,A,J,K),doDiv(N,A,J,K,T)).
doSum(N,Result,Loc,T):- sum(0,N,Loc,T,Result).
sum(A,_,[],_,A).
sum(Total,N,[Loc1|Loc2],T,Result):-
	getCoords(Loc1,Y,X),
	getNum(T,Y,X,Num1),
	Num1 #< Result,
	Num1 #> 0,
	TheSum#=Num1+Total,
	sum(TheSum,N,Loc2,T,Result).
doMult(N,Result,Loc,T):- mult(1,N,Loc,T,Result).
mult(A,_,[],_,A).
mult(Total,N,[Loc1|Loc2],T,Result):-
	getCoords(Loc1,Y,X),
	getNum(T,Y,X,Num1),
	Num1 #< Result,
	Num1 #> 0,
	TheSum #=# Num1*Total,
	mult(TheSum,N,Loc2,T,Result).
doSub(N,Result,J,K,T):-
	getCoords(J,Y1,X1), getCoords(K,Y2,X2),
	getNum(T,Y1,X1,Num1), getNum(T,Y2,X2,Num2),
	Num1 #< 255, Num2 #< 255,
	Num1 #> 0, Num2 #> 0,
	(Result#=Num1-Num2;Result#=Num2-Num1).
doDiv(N,Result,J,K,T):-
	getCoords(J,Y1,X1), getCoords(K,Y2,X2),
	getNum(T,Y1,X1,Num1), getNum(T,Y2,X2,Num2),
	Num1 #< 255, Num2 #< 255,
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


kenken_testcase(
  6,
  [
   +(11, [1-1, 2-1]),
   /(2, 1-2, 1-3),
   *(20, [1-4, 2-4]),
   *(6, [1-5, 1-6, 2-6, 3-6]),
   -(3, 2-2, 2-3),
   /(3, 2-5, 3-5),
   *(240, [3-1, 3-2, 4-1, 4-2]),
   *(6, [3-3, 3-4]),
   *(6, [4-3, 5-3]),
   +(7, [4-4, 5-4, 5-5]),
   *(30, [4-5, 4-6]),
   *(6, [5-1, 5-2]),
   +(9, [5-6, 6-6]),
   +(8, [6-1, 6-2, 6-3]),
   /(2, 6-4, 6-5)
  ]
).