% 51948 Iuri Campos
:- use_module(library(clpfd)). % para poder usar transpose/2
:- set_prolog_flag(answer_write_options,[max_depth(0)]). % ver listas completas
:- ['puzzlesAcampar.pl']. % Ficheiro dado. No Mooshak tera mais puzzles.
% Atencao: nao deves copiar nunca os puzzles para o teu ficheiro de codigo
% Segue-se o codigo


/* 
vizinhanca((L, C), Vizinhanca) e verdade se Vizinhanca e uma lista de celulas que sao vizinhas de (L, C), para cima, para baixo, para a esquerda e para a direita.

Exemplo: 
?- vizinhanca((3,4), Vizinhanca).
Vizinhanca = [(2,4),(3,3),(3,5),(4,4)].
*/
vizinhanca((L, C), Vizinhanca):-
    PrimeiroL is L-1,
    SegundoL is L+1,
    PrimeiroC is C-1,
    SegundoC is C+1,
    Vizinhanca = [(PrimeiroL,C),(L,PrimeiroC),(L,SegundoC),(SegundoL,C)].

/*
vizinhancaAlargada((L, C), VizinhancaAlargada) e verdade se VizinhancaAlargada e uma lista de celulas que sao vizinhas de (L, C), para cima, para baixo, para a esquerda, para a direita e nas diagonais.

Exemplo:   
?- vizinhancaAlargada((3,4), VizinhancaAlargada).
VizinhancaAlargada = [(2,3),(2,4),(2,5),(3,3),(3,5),(4,3),(4,4),(4,5)].
*/
vizinhancaAlargada((L, C), VizinhancaAlargada):-
    PrimeiroL is L-1,
    SegundoL is L+1,
    PrimeiroC is C-1,
    SegundoC is C+1,
    VizinhancaAlargada = [(PrimeiroL,PrimeiroC),(PrimeiroL,C),(PrimeiroL, SegundoC),(L,PrimeiroC),(L,SegundoC),(SegundoL,PrimeiroC),(SegundoL,C),(SegundoL, SegundoC)].



/*
todasCelulas(Tabuleiro, TodasCelulas) e verdade se TodasCelulas e uma lista de todas as celulas do Tabuleiro.
todasCelulas(Tabuleiro, TodasCelulas, Objecto) e verdade se TodasCelulas e uma lista de todas as celulas do Tabuleiro que contem o Objecto.

Exemplo:
?- todasCelulas([[a,_,_],[_,_,_],[_,_,_]], TodasCelulas).
TodasCelulas = [(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)].
?- todasCelulas([[a,_,_],[_,_,_],[_,_,_]], TodasCelulas, a).
TodasCelulas = [(1,1)].
*/
todasCelulas(T, TodasCelulas):-
    ContadorL is 0,
    todasCelulas(T, ContadorL ,TodasCelulas).

todasCelulas(T, TodasCelulas, Objecto):-
    todasCelulas_aux(T, 0, TodasCelulas, Objecto).

todasCelulas([],_, []).
todasCelulas([H|T], ContadorL, TodasCelulas):-
    % Extrair cada linha
    ContadorL1 is ContadorL+1,
    ContadorC is 1,
    % Processar a linha
    todasCelulas(H, ContadorL1, ContadorC,TodasCelulas1),
    % Avaliar o resto do tabuleiro/linhas
    todasCelulas(T ,ContadorL1 ,TodasCelulas2),
    append(TodasCelulas1,TodasCelulas2, TodasCelulas).

todasCelulas([], _ ,_,[]).
todasCelulas([_|T], ContadorL, ContadorC ,TodasCelulas):-
    % Numerar cada celula e adicionar a lista
    ContadorC1 is ContadorC+1,
    todasCelulas(T, ContadorL, ContadorC1 ,TodasCelulas1),
    TodasCelulas = [(ContadorL, ContadorC)|TodasCelulas1].

todasCelulas_aux([],_, [],_).
todasCelulas_aux([H|T], ContadorL, TodasCelulas, Objecto):-
    % Extrair cada linha
    ContadorL1 is ContadorL+1,
    ContadorC is 1,
    % Processar a linha
    todasCelulas_aux(H, ContadorL1, ContadorC,TodasCelulas1, Objecto),
    % Avaliar o resto do tabuleiro/linhas
    todasCelulas_aux(T ,ContadorL1 ,TodasCelulas2, Objecto),
    append(TodasCelulas1,TodasCelulas2, TodasCelulas).

todasCelulas_aux([], _ ,_,[],_).
todasCelulas_aux([H|T], ContadorL, ContadorC ,TodasCelulas,Objecto):-
    % Numerar cada celula e adicionar a lista caso seja igual ao objecto
    var(Objecto),
    var(H),!,
    ContadorC1 is ContadorC+1,
    todasCelulas_aux(T, ContadorL, ContadorC1 ,TodasCelulas1, Objecto),
    TodasCelulas = [(ContadorL, ContadorC)|TodasCelulas1].

todasCelulas_aux([H|T], ContadorL, ContadorC ,TodasCelulas,Objecto):-
    % Numerar cada celula e adicionar a lista caso seja igual ao objecto
    ContadorC1 is ContadorC+1,
    todasCelulas_aux(T, ContadorL, ContadorC1 ,TodasCelulas1, Objecto),
    H==Objecto,
    TodasCelulas = [(ContadorL, ContadorC)|TodasCelulas1].

todasCelulas_aux([_|T], ContadorL, ContadorC ,TodasCelulas,Objecto):-
    % Caso nao seja igual ao objecto
    ContadorC1 is ContadorC+1,
    todasCelulas_aux(T, ContadorL, ContadorC1 ,TodasCelulas1, Objecto),
    TodasCelulas = TodasCelulas1.




/*
calculaObjectosTabuleiro(Tabuleiro, LinhasNum, ColunasNum, Objecto) e verdade se LinhasNum e uma lista com o numero de Objectos em cada linha do Tabuleiro e ColunasNum e uma lista com o numero de Objectos em cada coluna do Tabuleiro.
Exemplo:
?- calculaObjectosTabuleiro(([[a,_,_],[_,_,_],[_,_,_]], LinhasNum, ColunasNum, a).
LinhasNum = [1,0,0],
ColunasNum = [1,0,0].
*/

calculaObjectosTabuleiro(Tabuleiro, ContagemLinhas, ContagemColunas, Objecto):-
    transpose(Tabuleiro, TabuleiroT),
    calculaObjectosTabuleiroLinhas(Tabuleiro,ContagemLinhas, Objecto),
    calculaObjectosTabuleiroLinhas(TabuleiroT,ContagemColunas, Objecto).

calculaObjectosTabuleiroLinhas([],[],_).
calculaObjectosTabuleiroLinhas([Linha|RestoTabuleiro],ContagemLinhas, Objecto):-
    contador(Linha, Objecto, Numero),
    calculaObjectosTabuleiroLinhas(RestoTabuleiro, ContagemLinhas1, Objecto),
    ContagemLinhas=[Numero|ContagemLinhas1].

% Auxiliar para calculaTabuleiros - Conta o numero de objectos numa lista
contador([], _,0).
contador([H|T], Objecto, Quantidade):-
    var(Objecto),
    var(H),!,
    contador(T, Objecto, Quantidade1),
    Quantidade is Quantidade1+1.
contador([H|T], Objecto, Quantidade):-
    H==Objecto,!,
    contador(T, Objecto, Quantidade1),
    Quantidade is Quantidade1+1.
contador([_|T], Objecto, Quantidade):-
    contador(T, Objecto, Quantidade).


/*
celulaVazia(Tabuleiro, (L, C)) e verdade se a celula (L, C) do Tabuleiro esta vazia, ou seja, nao contem nem tendas nem arvores.
Exemplo:
?- celulaVazia([[a,_,_],[_,_,_],[_,_,_]], (1,2)).
true.
*/

celulaVazia(Tabuleiro, (L, C)):-
    Tester = [a, t],
    nth1(L, Tabuleiro, Lista),
    nth1(C, Lista, Elemento),
    \+ var(Elemento),
    member(Elemento, Tester),!, fail.
celulaVazia(_, (_, _)).

/*
insereObjectoCelula(Tabuleiro, Objecto, (L, C)) e verdade se Tabuleiro e o resultado de inserir Objecto na celula (L, C) do Tabuleiro.
Exemplo:
?- insereObjectoCelula([[a,_,_],[_,_,_],[_,_,_]], t, (1,2)).
[[a,t,_],[_,_,_],[_,_,_]].
*/
insereObjectoCelula(Tabuleiro, TendaOuRelva, (L,C)) :-
    insereObjectoCelula(Tabuleiro, TendaOuRelva,0, L, C, NovoTabuleiro),
    Tabuleiro = NovoTabuleiro.
insereObjectoCelula([], _, _, _,_, []).
insereObjectoCelula([Linha|RestoTabuleiro], TendaOuRelva, L_actual, L,C, NovoTabuleiro) :-
    L_actual1 is L_actual + 1,
    L_actual1 ==L,!,
    substituiCelula(Linha, TendaOuRelva, 0, C, NovaLinha),
    NovoTabuleiro = [NovaLinha|RestoTabuleiro].
insereObjectoCelula([Linha|RestoTabuleiro], TendaOuRelva, L_actual, L,C, NovoTabuleiro) :-
    L_actual1 is L_actual+1,
    insereObjectoCelula(RestoTabuleiro, TendaOuRelva, L_actual1, L,C, RestoTabuleiro),
    NovoTabuleiro = [Linha|RestoTabuleiro].

% substituiCelula(Linha, NovoElemento, C_actual, C, NovaLinha) auxiliar de insereObjectoCelula.
substituiCelula([],_,_,_,[]).
substituiCelula([H|Resto],NovoElemento,C_actual,C , NovaLista) :-
    C_actual1 is C_actual+1,
    C_actual1 == C,
    var(H),!,
    NovaLista = [NovoElemento|Resto].   
substituiCelula([Elemento|Resto],NovoElemento,C_actual,C , NovaLinha) :-
    C_actual1 is C_actual+1,
    substituiCelula(Resto, NovoElemento,C_actual1,C, NovaLista),  
    NovaLinha = [Elemento|NovaLista].


/*
insereObjectoEntrePosicoes(Tabuleiro, Objecto, (L1, C1), (L2, C2)) e verdade se Tabuleiro e o resultado de inserir Objecto entre as posicoes (L1, C1) e (L2, C2) do Tabuleiro.
Exemplo:
?- insereObjectoEntrePosicoes([[a,_,_],[_,_,_],[_,_,_]], t, (1,1), (1,3)).
[[a,t,t,t],[_,_,_],[_,_,_]].
*/
insereObjectoEntrePosicoes(Tabuleiro, TendaOuRelva, (L, C1), (L, C2)) :-
    insereObjectoEntrePosicoes(Tabuleiro, TendaOuRelva,0, L, C1, C2, NovoTabuleiro),
    Tabuleiro = NovoTabuleiro.
insereObjectoEntrePosicoes([], _, _, _, _, _,[]).
insereObjectoEntrePosicoes([Linha|RestoTabuleiro], TendaOuRelva, ContadorL, L, C1, C2, [NovaLinha|RestoTabuleiro]) :-
    ContadorL1 is ContadorL+1,
    ContadorL1 == L,
    insereObjectoEntrePosicoesAux(Linha, TendaOuRelva,ContadorL1, C1, C2, NovaLinha).
insereObjectoEntrePosicoes([Linha|RestoTabuleiro], TendaOuRelva, ContadorL, L, C1, C2, [Linha|RestoTabuleiro]) :-
    % Se estivermos na linha errada
    ContadorL1 is ContadorL+1,
    insereObjectoEntrePosicoes(RestoTabuleiro, TendaOuRelva, ContadorL1,L, C1, C2, RestoTabuleiro).
% inserirObjectoEntrePosicoesAux(Linha, NovoElemento, C_actual, C1, C2, NovaLinha) auxiliar de insereObjectoEntrePosicoes.
insereObjectoEntrePosicoesAux([], _, _, _, _,[]).
insereObjectoEntrePosicoesAux([E|Resto], TendaOuRelva, ContadorC, C1,C2, [TendaOuRelva|RestoAtualizado]) :-
    % Caso esteja na celula a substituir
    ContadorC1 is ContadorC+1,
    ContadorC >=C1,
    ContadorC =<C2,
    var(E), % Adicionado para o predicado relva 
    insereObjectoEntrePosicoesAux(Resto, TendaOuRelva, ContadorC1, C1,C2, RestoAtualizado).
insereObjectoEntrePosicoesAux([Elemento|Resto], TendaOuRelva, ContadorC,C1, C2, [Elemento|RestoAtualizado]) :-
    ContadorC1 is ContadorC +1,
    insereObjectoEntrePosicoesAux(Resto, TendaOuRelva, ContadorC1,C1, C2, RestoAtualizado).

/*
relva((Tabuleiro, L_num, C_num)) e verdade se Tabuleiro e o resultado de colocar relva em todas as celulas que nas linhas e colunas que ja nao pode ser colocada tenda.
Exemplo:
?- relva(([[a,_,_],[_,_,_],[_,_,_]], [1,0,0], [1,0,0])).
Tabuleiro = [[a,r,r],[r,_,_],[r,_,_]].
*/

relva((T,L_num,C_num)):-
    relvaLinhas(T,L_num, NovoTabuleiro),
    relvaTranspose(NovoTabuleiro,C_num, NovoTabuleiro1),
    T = NovoTabuleiro1,!.
% relvaTranspose(T,C_num,NovoTabuleiro):- auxiliar de relva, transpoe o tabuleiro e chama relvaLinhas
relvaTranspose(T,C_num,NovoTabuleiro):-
    transpose(T, T1),
    relvaLinhas(T1,C_num, NovoTabuleiro1),
    transpose(NovoTabuleiro1, NovoTabuleiro).

% relvaLinhas(T,L_num,NovoTabuleiro):- auxiliar de relva, insere a relva por linha para cada linha do tabuleiro
relvaLinhas([],[],[]).
relvaLinhas([H|RestoLinhas],[L|Resto], [NovaLinha|NovoTabuleiro1]):-
    contador(H, t, Quantidade),
    Quantidade >= L,!,
    length(H, Num_colunas),
    insereObjectoEntrePosicoesAux(H,r,0,0,Num_colunas,NovaLinha),
    relvaLinhas(RestoLinhas,Resto,NovoTabuleiro1),!.
relvaLinhas([H|RestoLinhas],[_|Resto], [H|NovoTabuleiro1]):-
    relvaLinhas(RestoLinhas,Resto,NovoTabuleiro1),!.


/*
inacessiveis(Tabuleiro) e verdade se Tabuleiro e o resultado de colocar relva em todas as celulas que nao sao acessiveis, ou seja, que nao estejam na vizinhanca de uma arvore.
Exemplo:
?- inacessiveis([[a,_,_],[_,_,_],[_,_,_]]).
Tabuleiro = [[a,_,r],[_,r,r],[r,r,r]].
*/
inacessiveis(Tabuleiro):-
    todasCelulas(Tabuleiro, TodasCelulas),
    todasCelulas(Tabuleiro, TodasCelulasComAvore, a),
    calculoInacessiveis(TodasCelulasComAvore, Possiveis),!,
    remove_common(TodasCelulas, Possiveis, Preencher),
    remove_common(Preencher, TodasCelulasComAvore, Preencher2),
    insereRelvas(Tabuleiro,r,Preencher2).

% insereRelvas(Tabuleiro, TendaOuRelva, Lista) auxiliar de inacessiveis, insere relva em todas as celulas da lista
insereRelvas(_,_,[]).
insereRelvas(Tabuleiro, TendaOuRelva, [H|T]):-
    H = (L,C),
    insereObjectoCelula(Tabuleiro, TendaOuRelva, (L,C)),
    insereRelvas(Tabuleiro, TendaOuRelva, T).


% calculoInacessiveis(TodasCelulasComAvore, Possiveis) auxiliar de inacessiveis, calcula todas as celulas que nao sao acessiveis
calculoInacessiveis([],[]).
calculoInacessiveis([H|T], Inacessiveis):-
    vizinhanca(H, Vizinhanca),
    calculoInacessiveis(T, Inacessiveis1),
    append(Vizinhanca, Inacessiveis1, Inacessiveis).


% remove_common(L1, L2, L3) auxiliar de inacessiveis, remove todos os elementos de L1 que estao em L2
remove_common([], _, []).
remove_common([H|T], L2, L1):- 
    member(H, L2), !, 
    remove_common(T, L2, L1).
remove_common([H|T], L2, [H|L1]):-  
    remove_common(T, L2, L1).


/*
aproveita((Tabuleiro, L_num, C_num)) e verdade se Tabuleiro e o resultado de colocar tendas em todas as celulas que nas linhas e colunas faltam exctamente X tendas.
Exemplo:
?- aproveita(([[a,_,_],[_,_,_],[r,_,_]], [1,0,0], [1,0,0])).
[[a,_,_],[t,_,_],[r,_,_]].
*/
aproveita((T,Num_l,Num_c)):-
    % write("Aproveita"),nl,
    aproveita(T, Num_l),
    % write("LINHAS "), write(T),nl,
    transpose(T, T1),
    % write("---------------< TRANSPOSING"),nl,
    aproveita(T1, Num_c),
    % write("COLUNAS "), write(T1),nl,
    transpose(T1, T2),
    T=T2.

aproveita([],[]).
aproveita([H|T], [Hl|Tl]):-
    % write("A VERIFICAR "), write(H),nl,
    contador(H, t, QuantidadeTenda),
    % write("QUANTIDADE TENDA "), write(QuantidadeTenda),nl,
    contador(H, _, QuantidadeVar),
    % write("QUANTIDADE VAR "), write(QuantidadeVar),nl,
    QuantidadeTenda < Hl,
    Diff is Hl - QuantidadeTenda,
    % write("DIFF "), write(Diff),nl,
    QuantidadeVar==Diff,!,
    length(H, Num_colunas),
    insereObjectoEntrePosicoes([H|T], t, (1, 1), (1, Num_colunas)),
    aproveita(T, Tl),!.
aproveita([_|T], [_|Tl]):-
    aproveita(T, Tl).

/*
limpaVizinhancas((Tabuleiro, L_num, C_num)) e verdade se Tabuleiro e o resultado de colocar relva em todas as celulas que estao na vivinhanca alargada das tendas.
Exemplo:
?- limpaVizinhancas(([[a,t,_],[_,_,_],[_,_,_]], [1,0,0], [1,0,0])).
[[a,t,r],[r,r,r],[_,_,_]].
*/
limpaVizinhancas((Tabuleiro,_,_)):-
    todasCelulas(Tabuleiro, TodasCelulas),
    todasCelulas(Tabuleiro, TodasCelulasComTenda, t),
    calculoALimpar(TodasCelulasComTenda, Possiveis),
    get_common(TodasCelulas, Possiveis, Preencher),
    insereRelvas(Tabuleiro,r,Preencher),!.

% calculoALimpar(TodasCelulasComTenda, Possiveis) auxiliar de limpaVizinhancas, calcula todas as celulas que estao na vizinhanca alargada das tendas
calculoALimpar([],[]).
calculoALimpar([H|T], Inacessiveis):-
    H = (L,C),
    vizinhancaAlargada((L,C), Vizinhanca),  
    calculoALimpar(T, Inacessiveis1),
    append(Vizinhanca, Inacessiveis1, Inacessiveis).    

% get_common(L1, L2, L3) auxiliar de limpaVizinhancas, retorna todos os elementos de L1 que estao em L2
get_common([], _, []).
get_common([H|T], L2,[H|L1]):- 
    member(H, L2), !, 
    get_common(T, L2, L1).

get_common([_|T], L2, L1):-  
    get_common(T, L2, L1).

/*
unicaHipotese((Tabuleiro)) e verdade se Tabuleiro e o resultado de colocar a tenda caso haja apenas 1 celula vazia na vizinhanca da arvore.
Exemplo:
?- unicaHipotese(([[a,r,r],[_,r,r],[r,r,r]])).
[[a,r,r],[t,r,r],[r,r,r]].
*/
unicaHipotese((T,_,_)):-
    todasCelulas(T, TodasCelulas),
    todasCelulas(T, TodasCelulasComAvore,a),
    buscaVizinhancas(T, TodasCelulasComAvore, Possiveis),
    blockCheck(T, TodasCelulas, Possiveis),!.
% buscaVizinhancas(Tabuleiro, TodasCelulasComAvore, Possiveis) auxiliar de unicaHipotese, calcula todas as celulas que estao na vizinhanca das arvores
% TODO - ALTERAR O FOR LOOP, em vez de recursivo, com o member
buscaVizinhancas(_, [], []).
buscaVizinhancas(Tabuleiro, [H|T], [Vizinhanca|Possiveis1]):-
    H= (L,C),
    vizinhanca((L,C), Vizinhanca),
    buscaVizinhancas(Tabuleiro, T, Possiveis1).
% blockCheck(Tabuleiro, TodasCelulas, Possiveis) auxiliar de unicaHipotese, verifica se existe apenas uma celula vazia na vizinhanca da arvore
blockCheck(_, _, []).
blockCheck(Tabuleiro, TodasCelulas, [H|T]):-
    get_common(TodasCelulas, H, CelulasPossiveis),
    checkForTenda(Tabuleiro, CelulasPossiveis),
    countVariables(Tabuleiro, CelulasPossiveis, Contador),
    Contador == 1,
    preencherTenda(Tabuleiro, CelulasPossiveis),
    blockCheck(Tabuleiro, TodasCelulas, T).
blockCheck(Tabuleiro, TodasCelulas, [_|T]):-
    blockCheck(Tabuleiro, TodasCelulas, T).

% checkForTenda(Tabuleiro, CelulasPossiveis) auxiliar de blockCheck, verifica se existe uma tenda numa lista de celulas
checkForTenda(_,[]).
checkForTenda(Tabuleiro,[H|T]):-
    H = (L,C),
    nth1(L, Tabuleiro, Lista),
    nth1(C, Lista, Elemento),
    Elemento == t,!,fail,
    checkForTenda(Tabuleiro,T).
checkForTenda(Tabuleiro,[_|T]):-
    checkForTenda(Tabuleiro,T).

% countVariables(Tabuleiro, CelulasPossiveis, Contador) auxiliar de blockCheck, conta o numero de variaveis numa lista de celulas
countVariables(_,[],0).
countVariables(Tabuleiro,[H|T],Contador):-
    H = (L,C),
    nth1(L, Tabuleiro, Lista),
    nth1(C, Lista, Elemento),
    var(Elemento),!,
    countVariables(Tabuleiro, T, Contador1),
    Contador is Contador1+1.
countVariables(Tabuleiro,[_|T],Contador):-
    countVariables(Tabuleiro, T, Contador).

% prencherTenda(Tabuleiro, CelulasPossiveis) auxiliar de blockCheck, preenche a tenda numa lista de celulas
preencherTenda(_,[]).
preencherTenda(Tabuleiro,[H|T]):-
    % write("A preencher "), write(H),nl,
    H = (L,C),
    insereObjectoCelula(Tabuleiro, t, (L,C)),
    preencherTenda(Tabuleiro,T).

/*
valida(ListaArvores, ListaTendas) e verdade se a cada arvore que esta na ListaArvores corresponde uma tenda que consta na ListaTendas.
Exemplo:
?- valida([(1,1),(2,2)], [(1,2),(2,1)]).
true.
*/
valida([], _).
valida([H|T], LTen) :-
    H = (L,C),
    vizinhanca((L,C), Vizinhanca),
    get_common(LTen, Vizinhanca, Possiveis),
    member((L1,C1), Possiveis), % iterate over all tents in Possiveis
    select((L1,C1), LTen, LTen1), % remove tenda da lista de tendas
    no_tents_in_vizinhancaAlargada(LTen1, (L1,C1)), % Verifica se existem tendas na vizinhanca alargada
    valida(T, LTen1).
valida([_|T], LTen) :-
    !,fail,
    valida(T, LTen).

% no_tents_in_vizinhancaAlargada(LTen, (L1,C1)) auxiliar de valida, verifica se nao existem tendas na vizinhanca alargada de uma celula
no_tents_in_vizinhancaAlargada([], _).
no_tents_in_vizinhancaAlargada([(L2,C2)|T], (L1,C1)) :-
    vizinhancaAlargada((L1,C1), VizinhancaAlargada),
    \+ member((L2,C2), VizinhancaAlargada),
    no_tents_in_vizinhancaAlargada(T, (L1,C1)).

/*
resolve(Puzzle) e verdade se Puzzle e o resultado de resolver o puzzle, ou seja, de colocar tendas em todas as celulas vazias de modo a que o numero de tendas em cada linha e coluna seja igual ao numero de tendas que falta colocar nessa linha ou coluna.
Exemplo:
?- resolve(([[a,_,_],[_,_,_],[_,_,_]], [1,0,0], [1,0,0])).
[[a,t,t],[r,r,r],[r,r,r]].
*/
resolve((T,LinhasNum,ColunasNum)) :-
    % Estrategia de resolucao
    inacessiveis(T),
    relva((T,LinhasNum,ColunasNum)),
    aproveita((T,LinhasNum,ColunasNum)),
    relva((T,LinhasNum,ColunasNum)),
    unicaHipotese((T,LinhasNum,ColunasNum)),
    limpaVizinhancas((T,LinhasNum,ColunasNum)),
    aproveita((T,LinhasNum,ColunasNum)),

    todasCelulas(T, TodasCelulas, _),
    todasCelulas(T, CelulasArvore, a),
    todasCelulas(T, CelulasTenda, t),
    length(CelulasArvore, NumArvores),
    length(CelulasTenda, NumTendas),
    Dif is NumArvores - NumTendas,
    maplist(vizinhanca, CelulasArvore, Vizinhanca1),
    flatten(Vizinhanca1, Vizinhanca),
    get_common(TodasCelulas, Vizinhanca, Possiveis),!,
    combination(Dif, Possiveis, Locations),
    append(CelulasTenda, Locations,CheckList),
    valida(CelulasArvore,CheckList),
    insereRelvas(T,t,Locations),
    calculaObjectosTabuleiro(T, ContagemLinhas, ContagemColunas, t),
    ContagemLinhas == LinhasNum,
    ContagemColunas == ColunasNum,
    todasCelulas(T, NovaTodasCelulas, _),
    insereRelvas(T,r,NovaTodasCelulas),!.

/*  
combination(N, L, R) e verdade se R e uma lista de N elementos de L, ou seja, permite encontrar as combinacoes possiveis de N elementos de uma lista L.
Exemplo:
?- combination(2, [1,2,3], R).
R = [1, 2] ;
R = [1, 3] ;
R = [2, 3] ;
*/
combination(0, _, []).
combination(N, [X|T], [X|Comb]) :-
    N > 0,
    N1 is N - 1,
    combination(N1, T, Comb).
combination(N, [_|T], Comb) :-
    N > 0,
    combination(N, T, Comb).