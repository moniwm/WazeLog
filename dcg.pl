
:- module(dcg, [oracion/2]).

oracion(X, Lugar):-oracion(Lugar, X,[]),lugar(Lugar),!.
oracion([X|Resto], Lugar):-oracion(Resto, Lugar). %en caso de que la oracion empiece con sintagmas no reconocidos, se va descomponiendo la lista
														%hasta que encuentra una oración reconocible

lugar('Alajuela').
lugar('San Jose').
lugar('Cartago').
lugar('Heredia').
lugar('Limon').
lugar('Guanacaste').
lugar('Puntarenas').

lugar(casa).
lugar(universidad).
lugar(soda).
lugar(cafeteria).
lugar(iglesia).
lugar(oficina).
lugar(estacion).
lugar(comisaria).
lugar(catedral).
lugar(gasolinera).
lugar(piscina).
lugar(supermercado).
lugar(mall).
lugar(apartamento).
lugar(aeropuerto).
lugar(colegio).
lugar(cine).
lugar(teatro).
lugar(parque).
lugar(estadio).
lugar(gimnasio).
lugar(hotel).
lugar(motel).
lugar(banco).
lugar(templo).
lugar(museo).

%Definición de estructura de la oración 
oracion(Lugar) -->
	coordinacion_sintagma_nominal(Genero, Numero, Persona, nominativo), %evalua la posibilidad de un sintagma nominal compuesto e.g. Monica y yo queremos ir a San Jose
	sintagma_verbal(Modo, Genero, Numero, Persona, Lugar).

%Sujero tácito
oracion(Lugar) -->
	sintagma_verbal(Modo, Genero, Numero, Persona, Lugar).

%Respuesta solo de predicado que denota lugar
oracion(Lugar) -->
	coordinacion_sintagma_nominal(Genero, Numero, Persona, nominativo, Lugar).

%Predicado compuesto separado por 'y'
coordinacion_sintagma_nominal(Genero, plural, Persona, Caso) --> 
	sintagma_nominal(Gen1,_, Pers1, Caso),
	y,
	sintagma_nominal(Gen2,_, Pers2, Caso),
	{genero(Gen1,Gen2,Genero), persona(Pers1,Pers2,Persona)}. %Uso de las llaves de la DCG (Definite Clause Grammar) de Prolog para definir la informacion
															  %de Genero y Persona, que permite mantener intactas las reglas recursivas que generan la coordinacion del sintagma

%Predicado compuesto separado por ','
coordinacion_sintagma_nominal(Genero, plural, Persona, Caso) --> 
	sintagma_nominal(Gen1,_, Pers1, Caso),
	coma, %sintagmas nominales compuestos separados por coma
	sintagma_nominal(Gen2,_, Pers2, Caso),
	{genero(Gen1,Gen2,Genero), persona(Pers1,Pers2,Persona)}.

%Predicado simple
coordinacion_sintagma_nominal(Genero, Numero, Persona, Caso) --> sintagma_nominal(Genero, Numero, Persona, Caso).

%Predicado simple que denota lugar
coordinacion_sintagma_nominal(Genero, Numero, Persona, Caso, Lugar) --> sintagma_nominal(Genero, Numero, Persona, Caso, Lugar).

%En un predicado compuesto:
genero(masculino,_,masculino). %si el primer objeto es genero masculino, se utiliza el genero masculino  
genero(femenino,X,X). %si el primero objeto es femenino, se usa el genero del segundo objeto

%sintagmas nominales
sintagma_nominal(Genero,Numero,tercera,_) --> determinante(Genero,Numero),sustantivo(Genero,Numero), sintagma_preposicional.
sintagma_nominal(Genero,Numero,Persona,Caso) --> pronombre(Genero,Numero,Persona,Caso).
sintagma_nominal(Genero,singular,tercera,_) --> nombre_propio(Genero).

%sintagma nominal para averiguar lugar de origen/destino
sintagma_nominal(Genero,Numero,tercera,_,Lugar) --> determinante(Genero,Numero),sustantivo(Genero,Numero,Lugar),sintagma_preposicional(Lugar).


%Forma de tratamiento en predicados compuestos
persona(primera,_,primera).
persona(segunda, Persona2, PersonaFinal) :- (
			Persona2 == primera, !, PersonaFinal = primera;
			PersonaFinal = segunda
		).
persona(tercera,X,X).

%sintagma preposicional
sintagma_preposicional--> sp(Preposicion), sintagma_preposicional. %se admiten varios sintagmas preposicionales e.g. la casa de Monica en Cartago
sintagma_preposicional-->[].

sp(Preposicion)-->preposicion(Preposicion),coordinacion_sintagma_nominal(_,_,_,oblicuo). %caso gramatical oblicuo

%sintagma preposicional que precede a un lugar
sintagma_preposicional(Lugar)--> sp(Preposicion, Lugar), sintagma_preposicional(Lugar). %se admiten varios sintagmas preposicionales e.g. la casa de Monica en Cartago
sintagma_preposicional(Lugar)-->[].

sp(Preposicion, Lugar)-->preposicion(Preposicion),coordinacion_sintagma_nominal(_,_,_,oblicuo, Lugar).
sp(Preposicion, _) --> preposicion(Preposicion), verbo(Pide,Genero,Numero,Persona,Lexema). %verbo seguido de preposicion e infinitivo e.g. voy a ir 


%sintagma verbal
sintagma_verbal(Modo,Genero,Numero,Persona, Lugar) --> pronombre(Genero,Numero,Persona,oblicuo), % pronombre oblicuo e.g. me voy a ir
							auxiliares(Modo,Genero,Numero,Persona,_,Pide),
							verbo(Pide,Genero,Numero,Persona,Lexema),
							sintagma_preposicional(Lugar).

%Caso en que el verbo sea auxiliar o modal e.g. quiero ir, ha ido, está yendo
auxiliares(Modo,Genero,Numero,Persona,Categoria,Pide)-->verbo(Tipo,Modo,Genero,Numero,Persona,Lexema),{categoria(Tipo,Lexema,Categoria),pide(Categoria,Pide)}.
auxiliares(_,_,_,_,_,_)-->[]. %permite la ausencia de auxiliares

pide(modal,infinitivo).
pide(haber,participio).
pide(estar,gerundio).

categoria(aux,estar,estar).
categoria(aux,haber,haber).
categoria(modal,_,modal).

%----------------------------
% Léxico:

%separadores
coma --> [','].
y --> ['y'].
y --> ['o'].

%determinantes
determinante(femenino,singular) --> ['la'];['La'].
determinante(femenino,plural) --> ['las'];['Las'].
determinante(masculino,singular) --> ['el'];['El'].
determinante(masculino,plural) --> ['los'];['Los'].
determinante(_, singular) --> ['mi'];['Mi'].
determinante(_, plural) --> ['mis'];['Mis'].
determinante(_, singular) --> ['tu'];['Tu'].
determinante(_, plural) --> ['tus'];['Tus'].
determinante(_, singular) --> ['su'];['Su'].
determinante(_, plural) --> ['sus'];['Sus'].
determinante(masculino,singular) --> ['nuestro'];['Nuestro'].
determinante(masculino,plural) --> ['nuestros'];['Nuestros'].
determinante(femenino,singular) --> ['nuestra'];['Nuestra'].
determinante(femenino,plural) --> ['nuestras'];['Nuestras'].
determinante(masculino,singular) --> ['un'];['Un'].
determinante(masculino,plural) --> ['unos'];['Unos'].
determinante(femenino,singular) --> ['una'];['Una'].
determinante(femenino,plural) --> ['unas'];['Unas'].
determinante(masculino,singular) --> ['algun'];['Algun'].
determinante(masculino,plural) --> ['algunos'];['Algunos'].
determinante(femenino,singular) --> ['alguna'];['Alguna'].
determinante(femenino,plural) --> ['algunas'];['Algunas'].


determinante(_,_) --> []. %en caso de que no haya: cuando el sintagma nominal empieza con 
						  %una preposición e.g. "en Alajuela" (sintagma preposicional hace 
						  %llamado recursivo a sintagma nominal) 

%sustantivos
sustantivo(femenino,singular,casa) --> ['casa'];['Casa'].
sustantivo(femenino,singular,universidad) --> ['universidad'];['Universidad'].
sustantivo(femenino,singular,soda) --> ['soda'];['Soda'].
sustantivo(femenino,singular,cafeteria) --> ['cafeteria'];['Cafeteria'].
sustantivo(femenino,singular,iglesia) --> ['iglesia'];['Iglesia'].
sustantivo(femenino,singular,oficina) --> ['oficina'];['Oficina'].
sustantivo(femenino,singular,estacion) --> ['estacion'];['Estacion'].
sustantivo(femenino,singular,comisaria) --> ['comisaria'];['Comisaria'].
sustantivo(femenino,singular,catedral) --> ['catedral'];['Catedral'].
sustantivo(femenino,singular,gasolinera) --> ['gasolinera'];['Gasolinera'].
sustantivo(femenino,singular,piscina) --> ['piscina'];['Piscina'].
sustantivo(femenino,singular) --> ['hermana'];['Hermana'].
sustantivo(femenino,singular) --> ['madre'];['Madre'].
sustantivo(femenino,singular) --> ['amiga'];['Amiga'].
sustantivo(femenino,singular) --> ['prima'];['Prima'].
sustantivo(femenino,singular) --> ['esposa'];['Esposa'].
sustantivo(femenino,singular) --> ['novia'];['Novia'].
sustantivo(femenino,singular) --> ['sobrina'];['Sobrina'].
sustantivo(femenino,singular) --> ['tia'];['Tia'].
sustantivo(femenino,singular) --> ['vecina'];['Vecina'].

sustantivo(femenino,plural) --> ['hermanas'];['Hermanas'].
sustantivo(femenino,plural) --> ['madres'];['Madres'].
sustantivo(femenino,plural) --> ['amigas'];['Amigas'].
sustantivo(femenino,plural) --> ['primas'];['Primas'].
sustantivo(femenino,plural) --> ['esposas'];['Esposas'].
sustantivo(femenino,plural) --> ['novias'];['Novias'].
sustantivo(femenino,plural) --> ['sobrinas'];['Sobrinas'].
sustantivo(femenino,plural) --> ['tias'];['Tias'].
sustantivo(femenino,plural) --> ['vecinas'];['Vecinas'].

sustantivo(masculino, singular, supermercado) --> ['supermercado'];['Supermercado'].
sustantivo(masculino, singular, mall) --> ['mall'];['Mall'].
sustantivo(masculino, singular, apartamento) --> ['apartamento'];['Apartamento'].
sustantivo(masculino, singular, aeropuerto) --> ['aeropuerto'];['Aeropuerto'].
sustantivo(masculino, singular, colegio) --> ['colegio'];['Colegio'].
sustantivo(masculino, singular, cine) --> ['cine'];['Cine'].
sustantivo(masculino, singular, teatro) --> ['teatro'];['Teatro'].
sustantivo(masculino, singular, parque) --> ['parque'];['Parque'].
sustantivo(masculino, singular, estadio) --> ['estadio'];['Estadio'].
sustantivo(masculino, singular, gimnasio) --> ['gimnasio'];['Gimnasio'].
sustantivo(masculino, singular, hotel) --> ['hotel'];['Hotel'].
sustantivo(masculino, singular, motel) --> ['motel'];['Motel'].
sustantivo(masculino, singular, banco) --> ['banco'];['Banco'].
sustantivo(masculino, singular, templo) --> ['templo'];['Templo'].
sustantivo(masculino, singular, museo) --> ['museo'];['Museo'].

sustantivo(masculino, singular) --> ['hermano'];['Hermano'].
sustantivo(masculino, singular) --> ['padre'];['Padre'].
sustantivo(masculino, singular) --> ['amigo'];['Amigo'].
sustantivo(masculino, singular) --> ['primo'];['Primo'].
sustantivo(masculino, singular) --> ['esposo'];['Esposo'].
sustantivo(masculino, singular) --> ['novio'];['Novio'].
sustantivo(masculino, singular) --> ['sobrino'];['Sobrino'].
sustantivo(masculino, singular) --> ['tio'];['Tio'].
sustantivo(masculino, singular) --> ['vecino'];['Vecino'].

sustantivo(masculino, plural) --> ['hermanos'];['Hermanos'].
sustantivo(masculino, plural) --> ['padres'];['Padres'].
sustantivo(masculino, plural) --> ['amigos'];['Amigos'].
sustantivo(masculino, plural) --> ['primos'];['Primos'].
sustantivo(masculino, plural) --> ['esposos'];['Esposos'].
sustantivo(masculino, plural) --> ['novios'];['Novios'].
sustantivo(masculino, plural) --> ['sobrinos'];['Sobrinos'].
sustantivo(masculino, plural) --> ['tios'];['Tios'].
sustantivo(masculino, plural) --> ['vecinos'];['Vecinos'].

sustantivo(masculino, singular) --> ['punto'];['Punto'].
sustantivo(masculino, singular) --> ['origen'];['Origen'].
sustantivo(masculino, singular) --> ['salida'];['Salida'].
sustantivo(masculino, singular) --> ['destino'];['Destino'].
sustantivo(masculino, singular) --> ['llegada'];['Llegada'].

%Nodos del Grafo
sustantivo(masculino,singular,'Cartago') --> ['Cartago'];['cartago'].
sustantivo(masculino,singular,'San Jose') --> ['San Jose'];['san jose'].
sustantivo(masculino,singular,'Limon') --> ['Limon'];['limon'].
sustantivo(masculino,singular,'Guanacaste') --> ['Guanacaste'];['guanacaste'].
sustantivo(femenino,singular,'Alajuela') --> ['Alajuela'];['alajuela'].
sustantivo(femenino,singular,'Heredia') --> ['Heredia'];['heredia'].
sustantivo(femenino,singular,'Puntarenas') --> ['Puntarenas'];['puntarenas'].

sustantivo(_,_) --> []. %en caso de que no haya
sustantivo(_,_,_) --> []. %en caso de que no haya

%Pronombres personales
pronombre(_,singular,primera,nominativo) --> ['yo'];['Yo'].
pronombre(_,singular,primera,oblicuo) --> ['mi'];['Mi'].
pronombre(_,singular,segunda,_) --> ['vos'];['Vis'].
pronombre(_,singular,segunda,nominativo) --> ['tu'];['Tu'].
pronombre(_,singular,segunda,oblicuo) --> ['ti'];['Ti'].
pronombre(masculino,plural,primera,_) --> ['nosotros'];['Nosotros'].
pronombre(femenino,plural,primera,_) --> ['nosotras'];['Nosotras'].
pronombre(masculino,singular,tercera,_) --> ['el'];['El'].
pronombre(femenino,singular,tercera,_) --> ['ella'];['Ella'].
pronombre(masculino,plural,tercera,_) --> ['ellos'];['Ellos'].
pronombre(femenino,plural,tercera,_) --> ['ellas'];['Ellas'].
pronombre(_,singular,primera,oblicuo) --> ['me'];['Me'].
pronombre(_,singular,segunda,nominativo) --> ['te'];['Te'].
pronombre(_,plural,primera,oblicuo) --> ['nos'];['Nos'].
pronombre(_,_,tercera,oblicuo) --> ['se'];['Se'].

pronombre(_,_,_,_) --> [].

%Nombres propios
nombre_propio(femenino) --> ['Monica'].
nombre_propio(masculino) --> ['Luis Pedro'].
nombre_propio(masculino) --> ['Marco'].
nombre_propio(_) --> [].

%Preposiciones
preposicion(a) --> ['a'];['A'].
preposicion(al) --> ['al'];['Al'].
preposicion(ante) --> ['ante'];['Ante'].
preposicion(bajo) --> ['bajo'];['Bajo'].
preposicion(cabe) --> ['cabe'];['Cabe'].
preposicion(con) --> ['con'];['Con'].
preposicion(contra) --> ['contra'];['Contra'].
preposicion(de)-->['de'];['De'].
preposicion(desde) --> ['desde'];['Desde'].
preposicion(durante) --> ['durante'];['Durante'].
preposicion(en) --> ['en'];['En'].
preposicion(entre) --> ['entre'];['Entre'].
preposicion(hacia) --> ['hacia'];['Hacia'].
preposicion(hasta) --> ['hasta'];['Hasta'].
preposicion(mediante) --> ['mediante'];['Mediante'].
preposicion(para) --> ['para'];['Para'].
preposicion(por) --> ['por'];['Por'].
preposicion(segun) --> ['segun'];['Segun'].
preposicion(sin) --> ['sin'];['Sin'].
preposicion(sobre)-->['sobre'];['Sobre'].
preposicion(tras) --> ['tras'];['Tras'].
preposicion(que) --> ['que'];['Que'].

%-----------------------------------------
%verbos
verbo(_,_,_,_,_) --> [].
%ser
verbo(indicativo,_,singular,primera,ser) --> ['soy'];['sere'];['Soy'];['Sere'].
verbo(indicativo,_,singular,segunda,ser) --> ['sos'];['seras'];['Sos'];['Seras'].
verbo(indicativo,_,singular,segunda,ser) --> ['eres'];['seras'];['Eres'];['Seras'].
verbo(indicativo,_,singular,tercera,ser) --> ['es'];['seran'];['Es'];['Seran'].
verbo(indicativo,_,plural,primera,ser) --> ['somos'];['seremos'];['Somos'];['Seremos'].
verbo(indicativo,_,plural,segunda,ser) --> ['son'];['seran'];['Son'];['Seran']. 
verbo(indicativo,_,plural,tercera,ser) --> ['son'];['seran'];['Son'];['Seran'].

verbo(participio,_,_,_,ser)-->['sido'];['Sido'].
verbo(gerundio,_,_,_,ser)-->['siendo'];['Siendo'].
verbo(infinitivo,_,_,_,ser)-->['ser'];['Ser'].

%estar
verbo(indicativo,_,singular,primera,estar) --> ['estoy'];['estare'];['Estoy'];['Estare'].
verbo(indicativo,_,singular,segunda,estar) --> ['estas'];['estaras'];['Estas'];['Estaras'].
verbo(indicativo,_,singular,tercera,estar) --> ['esta'];['estara'];['Esta'];['Estara'].
verbo(indicativo,_,plural,primera,estar) --> ['estamos'];['estaremos'];['Estamos'];['Estaremos'].
verbo(indicativo,_,plural,segunda,estar) --> ['estan'];['estaran'];['Estan'];['Estaran']. 
verbo(indicativo,_,plural,tercera,estar) --> ['estan'];['estaran'];['Estan'];['Estaran'].

verbo(participio,_,_,_,estar)-->['estado'];['Estado'].
verbo(gerundio,_,_,_,estar)-->['estando'];['Estando'].
verbo(infinitivo,_,_,_,estar)-->['estar'];['Estar'].

%ir
verbo(indicativo,_,singular,primera,ir) --> ['voy'];['ire'];['Voy'];['Ire'].
verbo(indicativo,_,singular,segunda,ir) --> ['vas'];['iras'];['Vas'];['Iras'].
verbo(indicativo,_,singular,tercera,ir) --> ['va'];['ira'];['Va'];['Ira'].
verbo(indicativo,_,plural,primera,ir) --> ['vamos'];['iremos'];['Vamos'];['Iremos'].
verbo(indicativo,_,plural,segunda,ir) --> ['van'];['iran'];['Van'];['Iran']. 
verbo(indicativo,_,plural,tercera,ir) --> ['van'];['iran'];['Van'];['Iran'].

verbo(participio,_,_,_,ir)-->['ido'];['Ido'].
verbo(gerundio,_,_,_,ir)-->['yendo'];['Yendo'].
verbo(infinitivo,_,_,_,ir)-->['ir'];['Ir'].

%pasar
verbo(indicativo,_,singular,primera,pasar) --> ['paso'];['pasare'];['Paso'];['Pasare'].
verbo(indicativo,_,singular,segunda,pasar) --> ['pasas'];['pasaras'];['Pasas'];['Pasaras'].
verbo(indicativo,_,singular,tercera,pasar) --> ['pasa'];['pasara'];['Pasa'];['Pasara'].
verbo(indicativo,_,plural,primera,pasar) --> ['pasamos'];['pasaremos'];['Pasamos'];['Pasaremos'].
verbo(indicativo,_,plural,segunda,pasar) --> ['pasan'];['pasaran'];['Pasan'];['Pasaran']. 
verbo(indicativo,_,plural,tercera,pasar) --> ['pasan'];['pasaran'];['Pasan'];['Pasaran'].

verbo(participio,_,_,_,pasar)-->['pasado'];['Pasado'].
verbo(gerundio,_,_,_,pasar)-->['pasando'];['Pasando'].
verbo(infinitivo,_,_,_,pasar)-->['pasar'];['Pasar'].

%encontrar
verbo(indicativo,_,singular,primera,encontrar) --> ['encuentro'];['encontrare'];['Encuentro'];['Encontrare'].
verbo(indicativo,_,singular,segunda,encontrar) --> ['encontras'];['encontraras'];['Encontras'];['Encontraras'].
verbo(indicativo,_,singular,tercera,encontrar) --> ['encuentra'];['encontrara'];['Encuentra'];['Encontrara'].
verbo(indicativo,_,plural,primera,encontrar) --> ['encontramos'];['encontraremos'];['Encontramos'];['Encontraremos'].
verbo(indicativo,_,plural,segunda,encontrar) --> ['encuentran'];['encontraran'];['Encuentran'];['Encontraran']. 
verbo(indicativo,_,plural,tercera,encontrar) --> ['encuentran'];['encontraran'];['Encuentran'];['Encontraran'].

verbo(participio,_,_,_,encontrar)-->['encontrado'];['Encontrado'].
verbo(gerundio,_,_,_,encontrar)-->['encontrando'];['Encontrando'].
verbo(infinitivo,_,_,_,encontrar)-->['encontrar'];['Encontrar'].

%ubicar
verbo(indicativo,_,singular,primera,ubicar) --> ['ubico'];['ubicare'];['Ubico'];['Ubicare'].
verbo(indicativo,_,singular,segunda,ubicar) --> ['ubicas'];['ubicaras'];['Ubicas'];['Ubicaras'].
verbo(indicativo,_,singular,tercera,ubicar) --> ['ubica'];['ubicara'];['Ubica'];['Ubicara'].
verbo(indicativo,_,plural,primera,ubicar) --> ['ubicamos'];['ubicaremos'];['Ubicamos'];['Ubicaremos'].
verbo(indicativo,_,plural,segunda,ubicar) --> ['ubican'];['ubicaran'];['Ubican'];['Ubicaran']. 
verbo(indicativo,_,plural,tercera,ubicar) --> ['ubican'];['ubicaran'];['Ubican'];['Ubicaran'].

verbo(participio,_,_,_,ubicar)-->['ubicado'];['Ubicado'].
verbo(gerundio,_,_,_,ubicar)-->['ubicando'];['Ubicando'].
verbo(infinitivo,_,_,_,ubicar)-->['ubicar'];['Ubicar'].

%dirigir
verbo(indicativo,_,singular,primera,dirigir) --> ['dirijo'];['dirigire'];['Dirijo'];['Dirigire'].
verbo(indicativo,_,singular,segunda,dirigir) --> ['dirigis'];['dirigiras'];['Dirigis'];['Dirigiras'].
verbo(indicativo,_,singular,tercera,dirigir) --> ['dirige'];['dirigira'];['Dirige'];['Dirigira'].
verbo(indicativo,_,plural,primera,dirigir) --> ['dirigimos'];['dirigiremos'];['Dirigimos'];['Dirigiremos'].
verbo(indicativo,_,plural,segunda,dirigir) --> ['dirigen'];['dirigiran'];['Dirigen'];['Dirigiran']. 
verbo(indicativo,_,plural,tercera,dirigir) --> ['dirigen'];['dirigiran'];['Dirigen'];['Dirigiran'].

verbo(participio,_,_,_,dirigir)-->['dirigido'];['Dirigido'].
verbo(gerundio,_,_,_,dirigir)-->['dirigiendo'];['Dirigiendo'].
verbo(infinitivo,_,_,_,dirigir)-->['dirigir'];['Dirigir'].

%viajar
verbo(indicativo,_,singular,primera,viajar) --> ['viajo'];['viajare'];['Viajo'];['Viajare'].
verbo(indicativo,_,singular,segunda,viajar) --> ['viajas'];['viajaras'];['Viajas'];['Viajaras'].
verbo(indicativo,_,singular,tercera,viajar) --> ['viaja'];['viajara'];['Viaja'];['Viajara'].
verbo(indicativo,_,plural,primera,viajar) --> ['viajamos'];['viajaremos'];['Viajamos'];['Viajaremos'].
verbo(indicativo,_,plural,segunda,viajar) --> ['viajan'];['viajaran'];['Viajan'];['Viajaran']. 
verbo(indicativo,_,plural,tercera,viajar) --> ['viajan'];['viajaran'];['Viajan'];['Viajaran'].

verbo(participio,_,_,_,viajar)-->['viajado'];['Viajado'].
verbo(gerundio,_,_,_,viajar)-->['viajando'];['Viajando'].
verbo(infinitivo,_,_,_,viajar)-->['viajar'];['Viajar'].

%localizar
verbo(indicativo,_,singular,primera,localizar) --> ['localizo'];['localizare'];['Localizo'];['Localizare'].
verbo(indicativo,_,singular,segunda,localizar) --> ['localizas'];['localizaras'];['Localizas'];['Localizara'].
verbo(indicativo,_,singular,tercera,localizar) --> ['localiza'];['localizara'];['Localiza'];['Localizara'].
verbo(indicativo,_,plural,primera,localizar) --> ['localizamos'];['localizaremos'];['Localizamos'];['Localizaremos'].
verbo(indicativo,_,plural,segunda,localizar) --> ['localizan'];['localizaran'];['Localizan'];['Localizaran']. 
verbo(indicativo,_,plural,tercera,localizar) --> ['localizan'];['localizaran'];['Localizan'];['Localizaran'].

verbo(participio,_,_,_,localizar)-->['localizado'];['Localizado'].
verbo(gerundio,_,_,_,localizar)-->['localizando'];['Localizando'].
verbo(infinitivo,_,_,_,localizar)-->['localizar'];['Localizar'].

%verbos auxiliares
%haber
verbo(aux,participio,_,singular,primera,haber) --> ['he'];['He'].
verbo(aux,participio,_,singular,segunda,haber) --> ['has'];['Has'].
verbo(aux,participio,_,singular,tercera,haber) --> ['ha'];['Ha'].
verbo(aux,participio,_,plural,primera,haber) --> ['hemos'];['Hemos'].
verbo(aux,participio,_,plural,segunda,haber) --> ['han'];['Han']. 
verbo(aux,participio,_,plural,tercera,haber) --> ['han'];['Han'].

verbo(aux,participio,_,_,_,haber)-->['habido'];['Habido'].
verbo(aux,gerundio,_,_,_,haber)-->['habiendo'];['Habiendo'].

%estar
verbo(aux,gerundio,_,singular,primera,estar) --> ['estoy'];['Estoy'];['estaria'];['Estaria'].
verbo(aux,gerundio,_,singular,segunda,estar) --> ['estas'];['Estas'].
verbo(aux,gerundio,_,singular,tercera,estar) --> ['estan'];['Estan'].
verbo(aux,gerundio,_,plural,primera,estar) --> ['estamos'];['Estamos'].
verbo(aux,gerundio,_,plural,segunda,estar) --> ['estan'];['Estan']. 
verbo(aux,gerundio,_,plural,tercera,estar) --> ['estan'];['Estan'].

verbo(aux,participio,_,_,_,estar)-->['estado'];['Estado'].
verbo(aux,gerundio,_,_,_,estar)-->['estando'];['Estando'].

%querer
verbo(indicativo,_,singular,primera,querer) --> ['quiero'];['Quiero'];['quisiera'];['Quisiera'].
verbo(indicativo,_,singular,segunda,querer) --> ['queres'];['Queres'].
verbo(indicativo,_,singular,segunda,querer) --> ['quieres'];['Quieres'].
verbo(indicativo,_,singular,tercera,querer) --> ['quiere'];['Quiere'].
verbo(indicativo,_,plural,primera,querer) --> ['queremos'];['Queremos'].
verbo(indicativo,_,plural,segunda,querer) --> ['quieren'];['Quieren']. 
verbo(indicativo,_,plural,tercera,querer) --> ['quieren'];['Quieren'].

verbo(participio,_,_,_,querer)-->['querido'];['Querido'].
verbo(gerundio,_,_,_,querer)-->['queriendo'];['Queriendo'].
verbo(infinitivo,_,_,_,querer)-->['querer'];['Querer'].

%desear
verbo(indicativo,_,singular,primera,desear) --> ['deseo'];['Deseo'];['desearia'];['Desearia'].
verbo(indicativo,_,singular,segunda,desear) --> ['deseas'];['Deseas'].
verbo(indicativo,_,singular,tercera,desear) --> ['desea'];['Desea'].
verbo(indicativo,_,plural,primera,desear) --> ['deseamos'];['Deseamos'].
verbo(indicativo,_,plural,segunda,desear) --> ['desean'];['Desean']. 
verbo(indicativo,_,plural,tercera,desear) --> ['desean'];['Desean'].

verbo(participio,_,_,_,desear)-->['deseado'];['Deseado'].
verbo(gerundio,_,_,_,desear)-->['deseando'];['Deseando'].
verbo(infinitivo,_,_,_,desear)-->['desear'];['Desear'].

%tener
verbo(indicativo,_,singular,primera,tener) --> ['tengo'];['Tengo'];['tendria'];['Tendria'].
verbo(indicativo,_,singular,segunda,tener) --> ['tenes'];['Tenes'].
verbo(indicativo,_,singular,tercera,tener) --> ['tiene'];['Tiene'].
verbo(indicativo,_,plural,primera,tener) --> ['tenemos'];['Tenemos'].
verbo(indicativo,_,plural,segunda,tener) --> ['tienen'];['Tienen']. 
verbo(indicativo,_,plural,tercera,tener) --> ['tienen'];['Tienen'].

verbo(participio,_,_,_,tener)-->['tenido'];['Tenido'].
verbo(gerundio,_,_,_,tener)-->['teniendo'];['Teniendo'].
verbo(infinitivo,_,_,_,tener)-->['tener'];['Tener'].

%necesitar
verbo(indicativo,_,singular,primera,necesitar) --> ['necesito'];['Necesito'];['necesitaria'];['Necesitaria'].
verbo(indicativo,_,singular,segunda,necesitar) --> ['necesitas'];['Necesitas'].
verbo(indicativo,_,singular,tercera,necesitar) --> ['necesita'];['Necesita'].
verbo(indicativo,_,plural,primera,necesitar) --> ['necesitamos'];['Necesitamos'].
verbo(indicativo,_,plural,segunda,necesitar) --> ['necesitan'];['Necesitan']. 
verbo(indicativo,_,plural,tercera,necesitar) --> ['necesitan'];['Necesitan'].

verbo(participio,_,_,_,necesitar)-->['necesitado'];['Necesitado'].
verbo(gerundio,_,_,_,necesitar)-->['necesitando'];['Necesitando'].
verbo(infinitivo,_,_,_,necesitar)-->['necesitar'];['Necesitar'].

%gustar
verbo(indicativo,_,_,_,gustar) --> ['gusta'];['Gusta'];['gustara'];['Gustara'];['gustaria'];['Gustaria'].

verbo(participio,_,_,_,gustar)-->['gustado'];['Gustado'].
verbo(gerundio,_,_,_,gustar)-->['gustando'];['Gustando'].
verbo(infinitivo,_,_,_,gustar)-->['gustar'];['Gustar'].

verbo(modal,Modo,Genero,Numero,Persona,querer)-->verbo(Modo,Genero,Numero,Persona,querer). %convertir verbo querer en modal tambien
verbo(modal,Modo,Genero,Numero,Persona,desear)-->verbo(Modo,Genero,Numero,Persona,desear). %convertir verbo desear en modal tambien
verbo(modal,Modo,Genero,Numero,Persona,tener)-->verbo(Modo,Genero,Numero,Persona,tener). %convertir verbo tener en modal tambien
verbo(modal,Modo,Genero,Numero,Persona,necesitar)-->verbo(Modo,Genero,Numero,Persona,necesitar). %convertir verbo tener en modal tambien
verbo(modal,Modo,Genero,Numero,Persona,gustar)-->verbo(Modo,Genero,Numero,Persona,gustar). %convertir verbo gustar en modal tambien