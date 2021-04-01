
%------------------------------ Definición de la base de datos ----------------------------------------------------------------


/*conecta(origen, destino, distancia, tiempoNormal, tiempoPresa) indica que origen conecta con destino con una distancia
de x cantidad de kilómetros y posee una lista de 2 tiempos:  el primero es el tiempo promedio en condiciones normales y el segundo es el tiempo con presa*/

conecta(a, b, 2, [10, 20]).
conecta(a, f, 3, [15, 30]).
conecta(a, d, 1, [5, 10]).
conecta(b, a, 2, [14, 28]).
conecta(b, e, 4, [20, 40]).
conecta(c, e, 5, [15, 30]).
conecta(c, g, 7, [35, 70]).
conecta(d, a, 1, [3, 6]).
conecta(d, g, 8, [40, 80]).
conecta(e, b, 4, [12, 24]).
conecta(e, c, 5, [25, 50]).
conecta(e, f, 6, [30, 60]).
conecta(f, a, 3, [21, 42]).
conecta(f, e, 6, [18, 36]).
conecta(g, c, 7, [21, 42]).
conecta(g, d, 8, [24, 48]).


ruta(Origen, Destino, Ruta, Distancia) :- camino(Origen, Destino, [Origen], Referencia, Distancia), reverse(Referencia, Ruta).

camino(Origen, Destino, Ruta, [Destino|Ruta], Distancia) :- conecta(Origen, Destino, Distancia, _).

camino(Origen, Destino, Visitados, Ruta, Distancia) :- conecta(Origen, Intermedio, DistanciaAdyacente, _), Intermedio \== Destino, \+member(Intermedio, Visitados), camino(Intermedio, Destino,[Intermedio|Visitados], Ruta, Distancia1), Distancia is DistanciaAdyacente + Distancia1. 

ruta_corta(Origen, Destino, Ruta, Distancia) :- setof([R,D], ruta(Origen, Destino, R, D), Set), Set = [_|_], minima(Set,[Ruta, Distancia]).
 
minima([F|R],M) :- min(R,F,M).

min([],M,M).
min([[P,L]|R],[_,M],Min) :- L < M, !, min(R,[P,L],Min). 
min([_|R],M,Min) :- min(R,M,Min).

%------------------------------ Sintagmas nominales y verbales ----------------------------------------------------------------


oracion(S0,S):- sintagma_nominal(S0,S1), sintagma_verbal(S1,S).

sintagma_nominal(S0,S):- determinante(S0,S1), nombre(S1,S).

sintagma_verbal(S0,S):- verbo(S0,S).
sintagma_verbal(S0,S):- verbo(S0,S1), sintagma_nominal(S1,S).

determinante([el|S],S).
determinante([la|S],S).

nombre([hombre|S],S).
nombre([manzana|S],S).

verbo([come|S],S).

concatenar([],L,L).
concatenar([X|L1],L2,[X|L3]):- concatenar(L1,L2,L3).

%Recibe el string Oracion y separa todas las palabras en una lista. Si hay dos palabras seguidas que inician con mayúscula, 
%las une en un solo elemento de la lista. 
%Por ejemplo: ?- separar_oracion('Voy a ir a San José', L).     L = ["Voy", "a", "ir", "a", "San José"] 
separar_oracion(Oracion , Lista) :- split_string(Oracion, " ", " ", Palabras), 
                                    unir_nombres_propios(Palabras, ListaInvertida), 
                                    inversa(ListaInvertida, Lista), !.

unir_nombres_propios(P, L) :- unir_nombres_propios(P, [], L).
unir_nombres_propios([X|Tail], Aux, L) :- Aux == [], unir_nombres_propios(Tail, [X|Aux], L). %añade primera palabra siempre
unir_nombres_propios([], L, L). %condicion de parada

unir_nombres_propios([X|Tail1], [Y|Tail2], L) :- %une nombres propios en un solo elemento de la lista
                                        inicial_es_mayus(Y), %si la última palabra añadida empieza con mayúscula
                                        inicial_es_mayus(X), %si la próxima palabra a añadir empieza con mayúscula
                                        string_concat(Y, " ", X1), %concatenar separadas por espacio en blanco
                                        string_concat(X1, X, NombrePropio),
                                        unir_nombres_propios(Tail1, [NombrePropio|Tail2], L). %añadir nuevo nombre a la lista                                       
                                  
unir_nombres_propios([X|Tail], L1, L2) :- unir_nombres_propios(Tail, [X|L1], L2). %añade la próxima palabra a L1


%Verifica si la primera letra del string Palabra, es mayuscula. Para eso compara el código ASCII de dicha letra 
%y valida si se encuentra en el rango de las letras mayúsculas: [65, 90]
inicial_es_mayus(Palabra) :- string_code(1, Palabra, Code),
                             Code > 64, Code < 91.

%Valida si X es el primer elemento en Lista:
%primer_elemento(Lista, X)
primer_elemento([X|_], X). 

%Valida si L1 es la lista inversa de L2
inversa(L1,L2) :- inversa(L1,[],L2).
inversa([],L,L).
inversa([X|L1],L2,L3) :- inversa(L1,[X|L2],L3).
                             

