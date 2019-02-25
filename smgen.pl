% usage ./smgen.pl

deltaValue(octave, Sign, VN:PC, ['l',VN,' = delta(',VN,', ',Sign,PC,'*pm);']).
deltaValue(odespy, Sign, VN:PC, ['l',VN,' = delta(',VN,', ',Sign,PC,'*pm);']).

% Code Wrapper Fragments:  Pre [Assignments] Mid [Equations] Post
% template(Language, Pre, Mid, Post)

template(octave, Name, ['function xdot = ', Name, '(x,t)
  global kg ad ec pp fr h0 vol;
  pm = 1;'],
 [ 'inhibit = 1.1;
  kgp = lkg*inhibit;
  kgpp = kgp*inhibit;','\n'],
 ['endfunction','\n']).

template(odespy, Name, ['def function xdot = ',Name,'(x,t)
  global kg ad ec pp fr h0 vol;
  pm = 1;'],
 [ 'inhibit = 1.1;
  kgp = lkg*inhibit;
  kgpp = kgp*inhibit;','\n'],
 ['\n']).

gencode(Language, Name, Spec, Sign,
[ Pre, '\n',
  Assignments, '\n',
  Mid, '\n',
  Equations,'\n',
  Post ]) :-
    template(Language, Name, Pre, Mid, Post),
    maplist(deltaValue(Language, Sign), Spec, Assignments),
    diffeq(Language, Equations).

diffeq(octave,
       ['xdot(1)= h0*fr*vol/60 +(log(2)/lkg)*x(1) - fr*x(1)/60 - (lad/vol)*x(4)*x(1);\n',
	'xdot(2) = (lad/vol)*x(4)*x(1) + (log(2)/kgp)*x(2) - fr*x(2)/60  -  x(2)/lec;\n',
	'xdot(3) =  x(2)/lec   + (log(2)/kgpp)*x(3)  - fr*x(3)/60;\n',
	'xdot(4) =  lpp*x(3)/60                - fr*x(4)/60;\n']).

diffeq(python, ['\t\treturn [',
	'h0*fr*vol/60 +(log(2)/lkg)*x(1) - fr*x(1)/60 - (lad/vol)*x(4)*x(1)', ',' ,
	 '(lad/vol)*x(4)*x(1) + (log(2)/kgp)*x(2) - fr*x(2)/60  -  x(2)/lec', ',' ,
	 'x(2)/lec   + (log(2)/kgpp)*x(3)  - fr*x(3)/60', ',' ,
	 'lpp*x(3)/60                - fr*x(4)/60', ' ]','\n']).

% filename(Language, Name, FileName).	
filename(octave, Name, File) :- concat_atom([Name,'.m'],File).
filename(odespy, Name, File) :- concat_atom([Name,'.py'],File).

	
test :- genmodels(octave).

genmodels(Language) :-
    Spec = [kg:0.02, ad:0.1, ec:0.1, pp:0.05],
    writecode(Language, Spec, f2, ''),
    writecode(Language, Spec, g2, '-').

flat_format([])    --> [].
flat_format([H|T]) --> flat_format(H), flat_format(T).
flat_format(stmt(octave, A))       --> !, [A, ';\n'].
flat_format(stmt(python, Tabs, A)) --> !, tabs(Tabs), [A, '\n'].
flat_format(A) --> [A].

writecode(Language, Spec, Name, Sign) :-
    gencode(Language, Name, Spec, Sign, Code),
    filename(Language, Name, Filename),
    tell(Filename),
    flatten(Code, FlatCode),
    maplist(write, FlatCode),
    told.

