:-use_module(dcg).

%------------------------------ Definición de la base de datos para el grafo ----------------------------------------------------------------

/*
conecta(origen, destino, distancia, tiempoNormal, tiempoPresa) indica que origen conecta con destino con una distancia
de x cantidad de kilómetros y posee una lista de 2 tiempos:  el primero es el tiempo promedio en condiciones normales y 
el segundo es el tiempo con presa
*/

conecta('San Jose', 'Corralillo', 22, 22, 44).
conecta('San Jose', 'Cartago', 20, 20, 40).
conecta('Tres Rios', 'San Jose', 8, 8, 16).
conecta('Tres Rios', 'Pacayas', 15, 15, 30).
conecta('Cartago', 'San Jose', 20, 20, 40).
conecta('Cartago', 'Tres Rios', 8, 8, 16).
conecta('Cartago', 'Musgo Verde', 10, 10, 20).
conecta('Cartago', 'Pacayas', 13, 13, 26).
conecta('Cartago', 'Paraiso', 10, 10, 20).
conecta('Corralillo', 'San Jose', 22, 22, 44).
conecta('Corralillo', 'Musgo Verde', 6, 6, 12).
conecta('Musgo Verde', 'Corralillo', 6, 6, 12).
conecta('Musgo Verde', 'Cartago', 10, 10, 20).
conecta('Pacayas', 'Cartago', 13, 13, 26).
conecta('Pacayas', 'Tres Rios', 15, 15, 30).
conecta('Pacayas', 'Cervantes', 8, 8, 16).
conecta('Cervantes', 'Pacayas', 8, 8, 16).
conecta('Cervantes', 'Juan Vinas', 5, 5, 10).
conecta('Cervantes', 'Cachi', 7, 7, 14).
conecta('Paraiso', 'Cervantes', 4, 4, 8).
conecta('Paraiso', 'Cachi', 10, 10, 20).
conecta('Paraiso', 'Orosi', 8, 8, 16).
conecta('Orosi', 'Paraiso', 8, 8, 16).
conecta('Orosi', 'Cachi', 12, 12, 24).
conecta('Cachi', 'Paraiso', 10, 10, 20).
conecta('Cachi', 'Orosi', 12, 12, 24).
conecta('Cachi', 'Cervantes', 7, 7, 14).
conecta('Cachi', 'Turrialba', 40, 40, 80).
conecta('Juan Vinas', 'Turrialba', 4, 4, 8).
conecta('Turrialba', 'Cachi', 40, 40, 80).
conecta('Turrialba', 'Pacayas', 18, 18, 36).

%Se dice que existe una Ruta entre Origen y Destino si existe un camino entre ambos lugares
ruta(Origen, Destino, Ruta, Distancia, Tiempo_Normal, Tiempo_Presa) :- 
    camino(Origen, Destino, [Origen], Referencia, Distancia, Tiempo_Normal, Tiempo_Presa), %Verifica que exista un camino
    inversa(Referencia, Ruta). %Verifica que Referencia sea la inversa de Ruta

%Existe un camino entre Origen y Destino si Origen y Destino están conectados
camino(Origen, Destino, Ruta, [Destino|Ruta], Distancia, Tiempo_Normal, Tiempo_Presa) :- 
    conecta(Origen, Destino, Distancia, Tiempo_Normal, Tiempo_Presa).

%Existe un camino entre Oriegen y Destino si...
camino(Origen, Destino, Visitados, Ruta, Distancia, Tiempo_Normal, Tiempo_Presa) :- 
    conecta(Origen, Intermedio, Distancia_Adyacente, Tiempo_Normal_Adyacente, Tiempo_Presa_Adyacente), %Existe un camino entre el Origen y un punto Intermedio
    Intermedio \== Destino, %Intermedio es diferente a Destino
    \+member(Intermedio, Visitados), %Verifica si ese punto intermedio pertenece a los visitados y devuelve true si no pertenece
    camino(Intermedio, Destino,[Intermedio|Visitados], Ruta, Distancia1, Tiempo_Normal1, Tiempo_Presa1), %Si no pertenece a los visitados, se vuelve a buscar camino entre intermedio y destino
    Distancia is Distancia_Adyacente + Distancia1, % Va sumando la distancia entre puntos
    Tiempo_Normal is Tiempo_Normal_Adyacente + Tiempo_Normal1, %Suma el tiempo normal que se tarda yendo entre los puntos
    Tiempo_Presa is Tiempo_Presa_Adyacente + Tiempo_Presa1. %Suma el tiempo en presa que se tarda yendo entre dos puntos


%Busca la ruta mas corta entre dos puntos basándose en la distancia entre ellos
ruta_corta(Origen, Destino, Ruta, Distancia, Tiempo_Normal, Tiempo_Presa) :- 
    setof([R,D], ruta(Origen, Destino, R, D, Tiempo_Normal, Tiempo_Presa), Set), %Produce un set de resultados sin duplicados y de manera ordenada
    Set = [_|_], %Crea la variable set
    minima(Set,[Ruta, Distancia]). %Busca la ruta minima

%Busca la ruta mas corta del set
minima([Primera|Resto], Menor) :- min(Resto, Primera, Menor).

%Condicion de parada que indica que se encuentra el menor cuando el set esta vacio y lo ultimo que se comparó es igual a la minima
min([], Menor, Menor). 

%Hecho recursivo que devuelve la ruta menor al comparar distancias
min([[Ruta, Distancia]|Resto], [_, Distancia_Menor], Menor) :- 
    Distancia < Distancia_Menor, !, 
    min(Resto, [Ruta, Distancia], Menor). 

%Pasa al siguiente elemento del set para compararlo con la referencia
min([_|Resto], Menor, Referencia) :- min(Resto, Menor, Referencia).

%Obtiene la ruta con todos los lugares que el usuario quiere visitar
obtener_ruta(Lista_Lugares, Ruta_Parcial, Ruta_Final, Visitados, Distancia, Tiempo_Normal, Tiempo_Presa):-
    Visitados >= 2, %Es una condición de parada que indica que puede seguir solo si la lista con lugares tiene 2 elementos o mas
    nth0(0, Lista_Lugares, Origen, Restante1), % Obtiene el elemento que se quiere tener como origen además de la lista sin el primer elemento
    nth0(1, Lista_Lugares, Destino, Restante2), % Obtiene el elemento de destino
    obtener_ruta_parcial(Restante1, Origen, Destino, Ruta_Parcial, Ruta_Final, Distancia, Tiempo_Normal, Tiempo_Presa), !. %Obtiene la ruta parcial entre los dos primeros lugares de la lista

%Obtiene rutas parciales entre lugares
obtener_ruta_parcial(Otros_Lugares, Origen, Destino, Ruta_Parcial, Ruta_Final, Distancia1, Tiempo_Normal1, Tiempo_Presa1):-
    ruta_corta(Origen, Destino, Ruta, Distancia2, Tiempo_Normal2, Tiempo_Presa2), %Obtiene la ruta mas corta
    nth0(0, Ruta, Lugar1, R),
    append(Ruta_Parcial, R, Ruta_Final), %Se crea la ruta parcial de esta forma para evitar que cuando se reitere se repitan los lugares
    length(Otros_Lugares, Tamano), %Sirve para obtener el tamaño de la lista para la condicion de parada
    Distancia is Distancia1 + Distancia2,
    Tiempo_Normal is Tiempo_Normal1 + Tiempo_Normal2,
    Tiempo_Presa is Tiempo_Presa1 + Tiempo_Presa2,
    obtener_ruta(Otros_Lugares, Ruta_Final, Ruta2, Tamano, Distancia, Tiempo_Normal, Tiempo_Presa), !.

%Da el mensaje final con la ruta optima obtenida    
obtener_ruta(Otros_Lugares, Ruta, Ruta, Visitados, Distancia, Tiempo_Normal, Tiempo_Presa):-
    write('La ruta mas optima es: '),
    length(Ruta, Tamano),
    recorrer_lista(Ruta, Elemento, Tamano),
    write('\nLa distancia recorrida es de: '),
    write(Distancia),
    write('km.'),
    write('\nDuracion en condiciones normales: '),
    write(Tiempo_Normal),
    write('min.'),
    write('\nDuracion con presa: '),
    write(Tiempo_Presa),
    write('min.'), 
    write('\nGracias por utilizar Wazelog. ¡Buen viaje!\n'), !.

%Recorre una lista e imprime cada uno de sus elementos seguidos de una coma para separarlos siempre y cuando el tamaño de la lista sea mayor a 1
recorrer_lista(Lista, Elemento, Tamano):-
    Tamano > 1,
    nth0(0, Lista, Elemento1, Restante),
    write(Elemento1),
    write(', '),
    length(Restante, Tamano2),
    recorrer_lista(Restante, Elemento2, Tamano2).

%Si la lista es mayor a uno se imprime el elemento sin la coma
recorrer_lista(Lista, Lista, Tamano):-
    nth0(0, Lista, Elemento1, Restante),
    write(Elemento1).


%------------------------------ Sistema Experto ----------------------------------------------------------------

%Recibe el string Oracion y separa todas las palabras en una lista. Si hay dos palabras seguidas que inician con mayúscula, 
%las une en un solo elemento de la lista. 
%Por ejemplo: ?- separar_oracion('Voy a ir a San José', L).     L = ["Voy", "a", "ir", "a", "San José"] 
separar_oracion(Oracion , Lista) :- split_string(Oracion, " ", " ", Strings),
                                    unir_nombres_propios(Strings, Palabras),
                                    convertir_string_char(Palabras, Lista), !.%corte para obtener el primer resultado únicamente

%Convierte lista de strings a una lista de atoms
convertir_string_char(Strings, Chars) :- convertir_string_char(Strings, [], Chars).
convertir_string_char([], L0, L0).
convertir_string_char([X|Resto], L0, L1) :- atom_string(Atom, X), convertir_string_char(Resto, [Atom|L0], L1).

unir_nombres_propios(P, L) :- unir_nombres_propios(P, [], L). %Regla para unir los nombres propios (palabras que empiezan con mayúscula) en un solo elemento
unir_nombres_propios([X|Tail], Aux, L) :- Aux == [], unir_nombres_propios(Tail, [X|Aux], L). %añade primera palabra siempre
unir_nombres_propios([], L, L). %condicion de parada

unir_nombres_propios([X|Tail1], [Y|Tail2], L) :- %une nombres propios en un solo elemento de la lista
                                        inicial_es_mayus(Y), %si la última palabra añadida empieza con mayúscula
                                        inicial_es_mayus(X), %si la próxima palabra a añadir empieza con mayúscula
                                        string_concat(Y, " ", X1), %concatenar palabras separadas por espacio en blanco
                                        string_concat(X1, X, NombrePropio),
                                        unir_nombres_propios(Tail1, [NombrePropio|Tail2], L). %añadir nuevo nombre a la lista    

unir_nombres_propios([X|Tail1], [Y|Tail2], L) :- %une nombres de lugares de dos palabras en un solo elemento de la lista
                                        es_nombre_lugar_minuscula(Y), %si la última palabra añadida es uno de los lugares en minuscula
                                        string_concat(Y, " ", X1), %concatenar palabras separadas por espacio en blanco
                                        string_concat(X1, X, NombrePropio),
                                        unir_nombres_propios(Tail1, [NombrePropio|Tail2], L). %añadir nuevo nombre a la lista                                     
                                  
unir_nombres_propios([X|Tail], L1, L2) :- unir_nombres_propios(Tail, [X|L1], L2). %añade la próxima palabra a L1


%Verifica si la primera letra del string Palabra, es mayuscula. Para eso compara el código ASCII de dicha letra 
%y valida si se encuentra en el rango de las letras mayúsculas: [65, 90]
inicial_es_mayus(Palabra) :- string_code(1, Palabra, Code),
                             Code > 64, Code < 91.

%Verifica si cierta palabra corresponge a la primera palabra de los nombres de lugares del grafo pero en minuscula
es_nombre_lugar_minuscula(Palabra) :- Palabra == "san";
                                    Palabra == "juan";
                                    Palabra == "musgo";
                                    Palabra == "tres".


%Valida si X es el primer elemento en Lista:
%primer_elemento(Lista, X)
primer_elemento([X|_], X). 

%Valida si L1 es la lista inversa de L2
inversa(L1,L2) :- inversa(L1,[],L2).
inversa([],L,L).
inversa([X|L1],L2,L3) :- inversa(L1,[X|L2],L3).
                             
%X es la longitud de la lista L
longitud(L,X) :- longitud(L,X,0).
longitud([],X,Y) :- X is Y.
longitud([_|Tail], X, Y) :- longitud(Tail, X, Y + 1).

wazelog :-
    write('\n¡Hola! Bienvenido a Wazelog, la mejor lógica computacional para llegar a su destino.\n Por favor, indíqueme dónde se encuentra: \n'),
    obtener_lugar([], L0),
    write('\nEntendido. ¿Cuál es su destino?\n'),
    obtener_lugar(L0,L1),
    punto_intermedio(L1, L2), !.

%Obtiene el lugar ingresado por el usuario
obtener_lugar(L0, L1) :- 
    read_line_to_string(user_input, Oracion),
    separar_oracion(Oracion, Lista),
    oracion(Lista,L), nodo(L, Lugar), append(L0, [Lugar], L1), !.

%En caso de que no capte el lugar notifica al usuario y se vuelve a llamar recursivamente
obtener_lugar(L0, L1) :- 
    write('\nNo logré captar el lugar que mencionó. Por favor descríbalo nuevamente.\n'),
    obtener_lugar(L0, L1).

%Obtiene los puntos intermedios que el usuario quiera visitar
punto_intermedio(L0,L1) :- 
    write('\nExcelente, ¿tiene algún otro destino intermedio? [si/no]\n'),
    read_line_to_string(user_input, Resp),
    atom_string(Atom, Resp),
    afirmacion(Atom),
    obtener_punto_intermedio(L0, L1), !.

%Se cumple cuando el usuario ya no quiere ingresar puntos intermedios
punto_intermedio(L0, L0):- 
    nth0(0, L0, Origen, R),
    length(L0, Tamano),
    obtener_ruta(L0, [Origen], Ruta_Final, Tamano, 0, 0, 0), !.

%Obtiene el nombre del lugar intermedio y lo añade en la posicion 1 de la lista de lugares
obtener_punto_intermedio(L0, L1):-
    write('\nPor favor indíqueme cuál es la ubicación de este punto intermedio: \n'),
    read_line_to_string(user_input, Oracion),
    separar_oracion(Oracion, Lista),
    oracion(Lista,L), nodo(L, Lugar), nth0(1, L1, Lugar, L0),
    punto_intermedio(L1, L2), !.

%En caso de no captar el lugar, envia un mensaje notificando al usuario y hace una llamada recursiva 
obtener_punto_intermedio(L0,L1) :-
    write('\nNo logré captar el lugar que mencionó. Por favor descríbalo nuevamente.\n'),
    obtener_punto_intermedio(L0,L1).

afirmacion(si).

nodo('San Jose', 'San Jose').
nodo('Cartago', 'Cartago').
nodo('Corralillo', 'Corralillo').
nodo('Musgo Verde', 'Musgo Verde').
nodo('Tres Rios', 'Tres Rios').
nodo('Pacayas', 'Pacayas').
nodo('Paraiso', 'Paraiso').
nodo('Cervantes', 'Cervantes').
nodo('Juan Vinas', 'Juan Vinas').
nodo('Turrialba', 'Turrialba').
nodo('Cachi', 'Cachi').
nodo('Orosi', 'Orosi').

nodo(Lugar, L) :- format('\n¿Donde se ubica este lugar: ~a?\n',[Lugar]),
    read_line_to_string(user_input, Oracion),
    separar_oracion(Oracion, Lista),
    oracion(Lista,L),
    nodo(L, L), !.