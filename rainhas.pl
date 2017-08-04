%% ------------------- VERSÃO GERAR E TESTAR COM PERMUTAÇÕES -------------------

% rainhas_p(Q, N) is nondet
% Verdadeiro se Q é uma solução de tamanho N com N rainhas.
% Este predicado constrói as possíves soluções do N-rainhas.
rainhas_p(Q, N) :-
	sequencia(1, N, R),
	permutacao(R, Q),
	solucao(Q).

% permutacao(?L, ?P) is nondet
% Verdadeiro se P é uma permutação da lista L

permutacao([], []).

permutacao(L, [X|T]) :-
    x_select(X, L, R),
    permutacao(R, T).

%% -------------------------- VERSÃO COM BACKTRACKING --------------------------
% rainhas_n(?Q, +N) is nondet
% predicado "interface" para o rainhas/3
rainhas_n(Q, N) :-
	rainhas_n(Q, N, N).

% rainhas_n(?Q, +N, +T) is nondet
% Verdadeiro se Q é uma solução de tamanho T com N rainhas.
% Este é um predicado auxiliar e não deve ser chamado diretamente.
% Sua prova deve ser realizada com N = T.
% Exemplo:
%  ?- rainhas_n(Q, 5, 5).
%  Q = [4, 2, 5, 3, 1] ;
%  Q = [3, 5, 2, 4, 1] ;
%  Q = [5, 3, 1, 4, 2] ;

rainhas_n([R|Rs], N, T) :-
	T > 0, !,
	T0 is T-1,
	rainhas_n(Rs, N, T0),
	entre(1, N, R),
	solucao([R|Rs]).

rainhas_n([], _, 0).

%% solucao(+Q) is semidet
% Verdadeiro se Q é uma solução N-rainhas
% Este predicado apenas verifica se Q é uma solução, e não a constrói.
% Exemplo:
%  ?- solucao([4, 5, 3, 2, 1]).
%   true.

solucao(Q) :-
	not_repetidos(Q),
	seguro(Q),
	!.

% sequencia(+I, +F, ?S) is semidet
% Verdadeiro se S é uma lista com os números inteiros entre I e F (inclusive)
sequencia(I, F, R1) :-
	x_findall(I, F, R1)	.

% not_repetido(?L) is det
%
% Verdadeiro se L é uma lista sem elementos repetidos
% Exemplo:
%  not_repetidos([1,2,1]).
%  false.

not_repetidos([]):-
	true.

not_repetidos([X|XS]) :-
	not(membro(X, XS)),
	not_repetidos(XS).

%% entre(+I, +F, ?V) is nondet
% Verdadeiro se V é um número natural entre I e F (inclusive).
% Exemplo:
%  ?- entre(1, 3, V).
%  V = 1;
%  V = 2;
%  V = 3;
%  false.

entre(I, F, V) :-
	I < F,
	V = I.

entre(I, F, V) :-
	I == F, !,
	V = I.

entre(I, F, V) :-
	I < F,
	I1 is I+1,
	entre(I1, F, V).

%seguro(?L) is det
%
%Verdadeiro se L é uma lista com elementos permitidos para a solução
% Exemplio:
%  ?- seguro([2,4,3,1]).
%  true.

seguro([]).

seguro([L|Ls]) :-
	not_proibido(L, Ls,1),
	seguro(Ls).

% not_probido(+Y, +L, +Coluna_atual). is det
%
% Verdadeiro se é seguro adicionar Y em L considerando a Coluna Atual
% Exemplo.
%  not_proibido(4, [1,3,2], 4).
%  true.
not_proibido(_,[],_).

not_proibido(Y,[Y1|Ys],Coluna_Atual) :-
	Y1-Y=\=Coluna_Atual,
	Y-Y1=\=Coluna_Atual,
	Prox_Coluna is Coluna_Atual + 1,
	not_proibido(Y,Ys,Prox_Coluna).

% membro(?X, ?XS) is nondet
%
% Verdadeiro se X é um elemento de XS
% Exemplo:
%  ?- Membro(3, [1,2,3].
%  true.

membro(X, [X | _]).

membro(X, [_ | XS]) :-
    membro(X, XS).


% x_select(?El, +L, -T). is non det
%
% Verdadeiro se El é removido de L e se torna T
% Exemplo:
%  x_select(1, [1,2,3], T).
%  T = [2,3]

x_select(El,[El|T],T).
x_select(El,[H|T],[H|S]) :-
      x_select(El,T,S).

% x_findall(+I,+F,-R). is det
%
% Verdadeiro se R é uma lista com com valores ordenados entre I e F.
% Exemplo:
%  ?- x_findall(1,3,R).
%  R = [1,2,3].

x_findall(X,X,R):-
	R = [X], !.

x_findall(I,F,R) :-
	I1 is I + 1,
	x_findall(I1, F, R1),
	R = [I | R1].














