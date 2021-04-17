% Trabalho 2 - Suguru Solver
% Alunos: José Luiz de Souza - 19100533
%         Samuel Cardoso - 19100544
%         Thiago Zimmermann Loureiro Chaves - 19100547

% Define as subregões de cada regiao, onde para cada índices (x,y)
% existe um value definindo qual a subregião que aquela coordenada
% pertence, esta função será um função auxiliar para definir as
% subregiões de cada região definida na função 'entrada'



% Armazena os valores inciais da matriz, servido
% de entrada. Este seria o puzzle base que deverá
% ser resolvido

matriz5x5(
    [[0,1,2,2,2],
    [0,1,1,2,2],
    [3,1,4,4,4],
    [3,3,3,4,4],
    [3,5,5,5,5]],
    [[0,0,0,0,0],
    [0,0,1,0,4],
    [0,0,0,0,0],
    [4,0,0,0,0],
    [0,0,0,0,0]])

matriz6x6(
    [[0,0,0,2,2,2],
    [1,1,0,0,2,2],
    [3,3,3,4,4,4],
    [7,3,6,4,4,5],
    [7,7,6,6,5,5],
    [7,7,6,6,5,5]],
    [[0,5,0,0,0,0],
    [0,0,0,0,4,5],
    [0,0,0,0,0,0],
    [5,1,0,3,0,0],
    [0,0,0,0,0,2],
    [4,0,2,0,0,0]])

matriz7x7(
    [[0,0,1,1,2,2,2],
    [0,0,3,3,3,2,2],
    [0,4,4,3,5,6,6],
    [7,4,4,5,5,5,6],
    [7,7,4,8,5,9,6],
    [7,7,8,8,8,9,6],
    [10,10,10,8,9,9,9]],
    [[1,0,0,0,0,1,0],
    [0,0,3,0,0,0,0],
    [0,0,0,0,5,0,0],
    [3,0,2,0,2,0,2],
    [2,0,0,0,0,0,0],
    [0,0,5,0,0,5,0],
    [1,0,0,0,1,0,0]])

matriz8x8(
    [[0,0,0,3,4,4,5,5],
    [0,2,2,3,4,4,5,5],
    [2,2,3,3,4,6,6,5],
    [2,7,7,3,8,8,6,6],
    [10,10,7,7,8,9,9,6],
    [11,10,10,7,8,13,9,9],
    [11,11,10,12,8,13,13,9],
    [11,11,12,12,12,12,13,13]],
    [[0,0,0,3,0,0,2,0],
    [4,0,0,0,0,0,0,0],
    [0,2,0,0,0,0,0,0],
    [0,1,5,0,0,1,5,0],
    [0,2,0,0,0,0,0,0],
    [0,0,0,0,4,0,0,4],
    [0,0,0,0,0,3,0,0],
    [0,5,0,0,0,5,0,0]])

retornaElementoMatriz(Mat,X,Y,Z) :- nth0(X,Mat,Lista), nth0(Y,Lista,El), Z is El.

retornaelementolista(Lista,X,Z) :- nth0(X,Lista,El).