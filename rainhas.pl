﻿%% ------------------- VERSÃO GERAR E TESTAR COM PERMUTAÇÕES -------------------

% rainhas_p(Q, N) is nondet
% Verdadeiro se Q é uma solução de tamanho N com N rainhas.
% Este predicado constrói as possíves soluções do N-rainhas.
rainhas_p(Q, N) :-
	sequencia(1, N, R),
	permutacao(R, Q),
	solucao(Q).


% sequencia(+I, +F, ?S) is semidet
% Verdadeiro se S é uma lista com os números inteiros entre I e F (inclusive)
sequencia(I, F, []) :-
	true.

sequencia(I, F, [X|XS]) :-
	sequencia(I, F, XS),
	entre(I,F,X),
	true.


% permutacao(?L, ?P) is nondet
% Verdadeiro se P é uma permutação da lista L
permutacao(L, P) :-
	% ... continuar
	.

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
%  ...
rainhas_n([R|Rs], N, T) :-
	T > 0, !,
	T0 is T-1,
	rainhas_n(Rs, N, T0),
	entre(1, N, R),
	solucao([R|Rs]).

rainhas_n([], _, 0).

%% entre(+I, +F, ?V) is nondet
% Verdadeiro se V é um número natural entre I e F (inclusive).
% Exemplo:
%  ?- entre(1, 3, V).
%  V = 1;
%  V = 2;
%  V = 3;
%  false.
entre(I, F, V) :-
	V > I,
	V =< F,
	true.

%% solucao(+Q) is semidet
% Verdadeiro se Q é uma solução N-rainhas
% Este predicado apenas verifica se Q é uma solução, e não a constrói.
% Exemplo:
%  ?- solucao([4, 5, 3, 2, 1]).
%   true.
solucao(Q) :-
	% ... continuar
	.










