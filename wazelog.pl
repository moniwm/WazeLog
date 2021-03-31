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
                             

