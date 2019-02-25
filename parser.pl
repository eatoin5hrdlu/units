
:- current_stream(0, read, ConIn), set_stream(ConIn, tty(true)), 
   current_stream(1, write, ConOut), set_stream(ConOut, tty(true)), 
   current_stream(2, write, ConErr), set_stream(ConErr, tty(true)).

:- set_prolog_flag(double_quotes, codes).

% sample("foobar = att - 3.0C - 3.0C").
sample("foobar = att + 3.0C + 10uNpm + 4.5ps/mN + 9MN - 5m3s-1 - 10m3s-1").

scale(1.0e-18) --> "a",!.
scale(1.0e-15) --> "f",!.
scale(1.0e-12) --> "p",!.
scale(1.0e-9)  --> "n",!.
scale(1.0e-6) --> "u", !.
scale(0.001)  --> "m".  % possibly milli (but could be meters!)
scale(1000)   --> "k";"K",!.
scale(1000000)--> "M",!.
scale(1.0e9)  --> "G",!.
scale(1.0e12) --> "T",!.
scale(1.0e15) --> "P",!.
scale(1) --> "".

unit(force(newton)) --> "N",!.
unit(length(meter)) --> "m",!.
unit(time(second)) --> "s",!.
unit(temperature(centegrade)) --> "C",!.
unit(temperature(farenheit)) --> "F",!.

scaled_unit(scale(Scale,Unit:Dim)) --> scale(Scale), unit(Unit), opt_dim(Dim).

opt_dim(N) --> integer(N), !.
opt_dim(1) --> "".	

units(A/B) --> scaled_unit(A), divide, !, units(B).
units(A-B) --> scaled_unit(A), connector,  !, units(B).
units(A-B) --> scaled_unit(A), units(B), !.
units(A)   --> scaled_unit(A).


number(float(Float)) --> digits(Integer),
			 ".",
			 !,
			 digits(Fraction),
			 { append(Integer,[0'.|Fraction],FChars),
			   number_codes(Float, FChars) }.
number(int(I)) --> integer(I).


integer(NI) --> "-", pos_integer(I), !, { NI is -I }.
integer(I)  -->      pos_integer(I).

pos_integer(I) --> digits(Intcodes),
		   !,
		   { number_codes(I,Intcodes) }.

digits([D|Ds]) --> [D],
		   { memberchk(D,"0123456789") },
		   !,
		   rest_digits(Ds).

rest_digits([D|Ds]) --> [D],
			{ memberchk(D,"0123456789") },
			!,
			rest_digits(Ds).
rest_digits([])     --> [].

variable([C|Cs]) --> [C], { char_type(C,alpha) },
		     !,
		     rest_variable(Cs).

rest_variable([C|Cs]) --> [C], { char_type(C,prolog_identifier_continue) },
			  !,
			  rest_variable(Cs).
rest_variable([]) --> [].


expression(variable(Name)) --> variable(Chars), !, {name(Name,Chars)}.
expression(value(Number,Units)) --> number(Number), !, units(Units).

expression2(A+B) --> expression(A), plus, !, expression2(B).
expression2(A/B) --> expression(A), divide, !, expression2(B).
expression2(A*B) --> expression(A), times, !, expression2(B).
expression2(A-B) --> expression(A), minus, !, expression2(B).
expression2(A)   --> expression(A).

equation(LHS=RHS) --> expression(LHS), equals, expression2(RHS).

equals --> opt_space, "=", opt_space.

connector --> "-".

plus --> opt_space, "+", opt_space.
minus --> opt_space, "-", opt_space.
times --> opt_space, "*", opt_space.
divide --> opt_space, "/", opt_space.

opt_space --> [S], { char_type(S,space) }, !, opt_space.
opt_space --> [].

check_units(R=L, Scale, Units) :-
    check_units(R,Scale,Units),
    check_units(L,Scale,Units).
check_units(A+B, Scale, Units) :-
    check_units(A, Scale, Units),
    check_units(B, Scale, Units).
check_units(A-B, Scale, Units) :-
    check_units(A,ScaleA,UnitsA),
    check_units(B,ScaleB,UnitsB),
    Scale is ScaleA*ScaleB,
    Units = UnitsA-UnitsB.
check_units(A/B, Scale, Units) :-
    check_units(A, ScaleA, UnitsA),
    check_units(B, ScaleB, UnitsB),
    Scale is ScaleA/ScaleB,
    Units = UnitsA/UnitsB.

% Trivially equivalent
equivalent(scaled_unit(Scale,Unit),
	   scaled_unit(Scale,Unit)) :- !.
% Evalutate Scale and Units separately?
equivalent(scaled_unit(_S1,U1),
	   scaled_unit(_S2,U2)) :- writeln(nyi(scaled_unit(U1,U2))),!.

% These are all equivalent:
%          nm/ps = nmps-1 = m/ms = km/s = kms-1
%
% Turn all of these into   scale(1000.0,length(meter)/time(second))
% scale(1e-9,length(meter))/scale(1e-12,time(sec)) = 
% Reduce scale to a single multiplier and find sort order for units.

reduce(scale(S1,U1)/scale(S2,U2), scale(S,U1/U2)) :-
    TS is S1/S2,
    ( TS > 1.0 -> S is integer(TS) ; TS = S ).
reduce(scale(S1,U1)-scale(S2,U2), scale(S,U1-U2)) :-
    TS is S1*S2,
    ( TS > 1.0 -> S is integer(TS) ; TS = S ).

test :-
    sample(Data),
    equation(E,Data,[]),
    writeq(E).

% OUTPUT
% variable(foobar) = variable(att)
% + (value(float(3.0),scale(1,temperature(centegrade):1))
% + (value(int(10),scale(1.0e-6,force(newton):1)-scale(1.0e-12,length(meter):1))
% + ( value(float(4.5),scale(1.0e-12,time(second):1)
%                        /
%                       (scale(1,length(meter):1)-scale(1,force(newton):1))
%          )
%   )
% + (value(int(9),scale(1000000,force(newton):1))
% - (value(int(5),scale(1,length(meter):3)-scale(1,time(second): -1))
% - value(int(10),scale(1,length(meter):3)-scale(1,time(second): -1)))))))


