
oracion(X, Lugar):-oracion(Lugar, X,[]),lugar(Lugar),!.
oracion([X|Resto], Lugar):-oracion(Resto, Lugar). %en caso de que la oracion empiece con sintagmas no reconocidos, se va descomponiendo la lista
														%hasta que encuentra una oración reconocible

lugar(alajuela).
lugar(san_jose).
lugar(cartago).
lugar(heredia).
lugar(limon).
lugar(alajuela).
lugar(guanacaste).

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


%sintagma verbal
sintagma_verbal(Modo,Genero,Numero,Persona, Lugar) --> auxiliares(Modo,Genero,Numero,Persona,_,Pide),
							verbo(Pide,Genero,Numero,Persona,Lexema),
							%sintagma_nominal(Genero,Numero,Persona,Caso, Lugar).
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
determinante(femenino,singular) --> ['la'].
determinante(femenino,plural) --> ['las'].
determinante(masculino,singular) --> ['el'].
determinante(masculino,plural) --> ['los'].
determinante(_, singular) --> ['mi'].
determinante(_, plural) --> ['mis'].
determinante(_, singular) --> ['tu'].
determinante(_, plural) --> ['tus'].
determinante(_, singular) --> ['su'].
determinante(_, plural) --> ['sus'].
determinante(masculino,singular) --> ['nuestro'].
determinante(masculino,plural) --> ['nuestros'].
determinante(femenino,singular) --> ['nuestra'].
determinante(femenino,plural) --> ['nuestras'].
determinante(masculino,singular) --> ['un'].
determinante(masculino,plural) --> ['unos'].
determinante(femenino,singular) --> ['una'].
determinante(femenino,plural) --> ['unas'].
determinante(masculino,singular) --> ['algun'].
determinante(masculino,plural) --> ['algunos'].
determinante(femenino,singular) --> ['alguna'].
determinante(femenino,plural) --> ['algunas'].

determinante(femenino,singular) --> ['La'].
determinante(femenino,plural) --> ['Las'].
determinante(masculino,singular) --> ['El'].
determinante(masculino,plural) --> ['Los'].
determinante(_, singular) --> ['Mi'].
determinante(_, plural) --> ['Mis'].
determinante(_, singular) --> ['Tu'].
determinante(_, plural) --> ['Tus'].
determinante(_, singular) --> ['Su'].
determinante(_, plural) --> ['Sus'].
determinante(masculino,singular) --> ['Nuestro'].
determinante(masculino,plural) --> ['Nuestros'].
determinante(femenino,singular) --> ['Nuestra'].
determinante(femenino,plural) --> ['Nuestras'].
determinante(masculino,singular) --> ['Un'].
determinante(masculino,plural) --> ['Unos'].
determinante(femenino,singular) --> ['Una'].
determinante(femenino,plural) --> ['Unas'].
determinante(masculino,singular) --> ['Algun'].
determinante(masculino,plural) --> ['Algunos'].
determinante(femenino,singular) --> ['Alguna'].
determinante(femenino,plural) --> ['Algunas'].

determinante(_,_) --> []. %en caso de que no haya: cuando el sintagma nominal empieza con 
						  %una preposición e.g. "en Alajuela" (sintagma preposicional hace 
						  %llamado recursivo a sintagma nominal) 

%sustantivos
sustantivo(femenino,singular,casa) --> ['casa'].
sustantivo(femenino,singular,universidad) --> ['universidad'].
sustantivo(femenino,singular,soda) --> ['soda'].
sustantivo(femenino,singular,cafeteria) --> ['cafeteria'].
sustantivo(femenino,singular,iglesia) --> ['iglesia'].
sustantivo(femenino,singular,oficina) --> ['oficina'].
sustantivo(femenino,singular,estacion) --> ['estacion'].
sustantivo(femenino,singular,comisaria) --> ['comisaria'].
sustantivo(femenino,singular,catedral) --> ['catedral'].
sustantivo(femenino,singular,gasolinera) --> ['gasolinera'].
sustantivo(femenino,singular,piscina) --> ['piscina'].
sustantivo(femenino,singular) --> ['hermana'].
sustantivo(femenino,singular) --> ['madre'].
sustantivo(femenino,singular) --> ['amiga'].
sustantivo(femenino,singular) --> ['prima'].
sustantivo(femenino,singular) --> ['esposa'].
sustantivo(femenino,singular) --> ['novia'].
sustantivo(femenino,singular) --> ['sobrina'].
sustantivo(femenino,singular) --> ['tia'].
sustantivo(femenino,singular) --> ['vecina'].

sustantivo(femenino,plural) --> ['hermanas'].
sustantivo(femenino,plural) --> ['madres'].
sustantivo(femenino,plural) --> ['amigas'].
sustantivo(femenino,plural) --> ['primas'].
sustantivo(femenino,plural) --> ['esposas'].
sustantivo(femenino,plural) --> ['novias'].
sustantivo(femenino,plural) --> ['sobrinas'].
sustantivo(femenino,plural) --> ['tias'].
sustantivo(femenino,plural) --> ['vecinas'].

sustantivo(masculino, singular, supermercado) --> ['supermercado'].
sustantivo(masculino, singular, mall) --> ['mall'].
sustantivo(masculino, singular, apartamento) --> ['apartamento'].
sustantivo(masculino, singular, aeropuerto) --> ['aeropuerto'].
sustantivo(masculino, singular, colegio) --> ['colegio'].
sustantivo(masculino, singular, cine) --> ['cine'].
sustantivo(masculino, singular, teatro) --> ['teatro'].
sustantivo(masculino, singular, parque) --> ['parque'].
sustantivo(masculino, singular, estadio) --> ['estadio'].
sustantivo(masculino, singular, gimnasio) --> ['gimnasio'].
sustantivo(masculino, singular, hotel) --> ['hotel'].
sustantivo(masculino, singular, motel) --> ['motel'].
sustantivo(masculino, singular, banco) --> ['banco'].
sustantivo(masculino, singular, templo) --> ['templo'].
sustantivo(masculino, singular, museo) --> ['museo'].

sustantivo(masculino, singular) --> ['hermano'].
sustantivo(masculino, singular) --> ['padre'].
sustantivo(masculino, singular) --> ['amigo'].
sustantivo(masculino, singular) --> ['primo'].
sustantivo(masculino, singular) --> ['esposo'].
sustantivo(masculino, singular) --> ['novio'].
sustantivo(masculino, singular) --> ['sobrino'].
sustantivo(masculino, singular) --> ['tio'].
sustantivo(masculino, singular) --> ['vecino'].

sustantivo(masculino, plural) --> ['hermanos'].
sustantivo(masculino, plural) --> ['padres'].
sustantivo(masculino, plural) --> ['amigos'].
sustantivo(masculino, plural) --> ['primos'].
sustantivo(masculino, plural) --> ['esposos'].
sustantivo(masculino, plural) --> ['novios'].
sustantivo(masculino, plural) --> ['sobrinos'].
sustantivo(masculino, plural) --> ['tios'].
sustantivo(masculino, plural) --> ['vecinos'].

sustantivo(masculino, singular) --> ['punto'].
sustantivo(masculino, singular) --> ['origen'].
sustantivo(masculino, singular) --> ['salida'].
sustantivo(masculino, singular) --> ['destino'].
sustantivo(masculino, singular) --> ['llegada'].

%Nodos del Grafo
sustantivo(masculino,singular,cartago) --> ['Cartago'];['cartago'].
sustantivo(masculino,singular,san_jose) --> ['San Jose'];['san jose'].
sustantivo(masculino,singular,limon) --> ['Limon'];['limon'].
sustantivo(masculino,singular,guanacaste) --> ['Guanacaste'];['guanacaste'].
sustantivo(femenino,singular,alajuela) --> ['Alajuela'];['alajuela'].
sustantivo(femenino,singular,heredia) --> ['Heredia'];['heredia'].
sustantivo(femenino,singular,puntarenas) --> ['Puntarenas'];['puntarenas'].

sustantivo(_,_) --> []. %en caso de que no haya
sustantivo(_,_,_) --> []. %en caso de que no haya

%Pronombres personales
pronombre(_,singular,primera,nominativo) --> ['yo'].
pronombre(_,singular,primera,oblicuo) --> ['mi'].
pronombre(_,singular,segunda,_) --> ['vos'].
pronombre(_,singular,segunda,nominativo) --> ['tu'].
pronombre(_,singular,segunda,oblicuo) --> ['ti'].
pronombre(masculino,plural,primera,_) --> ['nosotros'].
pronombre(femenino,plural,primera,_) --> ['nosotras'].
pronombre(masculino,singular,tercera,_) --> ['el'].
pronombre(femenino,singular,tercera,_) --> ['ella'].
pronombre(masculino,plural,tercera,_) --> ['ellos'].
pronombre(femenino,plural,tercera,_) --> ['ellas'].
pronombre(_,singular,primera,oblicuo) --> ['me'].
pronombre(_,singular,segunda,nominativo) --> ['te'].
pronombre(_,plural,primera,oblicuo) --> ['nos'].
pronombre(_,_,tercera,oblicuo) --> ['se'].

pronombre(_,singular,primera,nominativo) --> ['Yo'].
pronombre(_,singular,primera,oblicuo) --> ['Mi'].
pronombre(_,singular,segunda,_) --> ['Vos'].
pronombre(_,singular,segunda,nominativo) --> ['Tu'].
pronombre(_,singular,segunda,oblicuo) --> ['Ti'].
pronombre(masculino,plural,primera,_) --> ['Nosotros'].
pronombre(femenino,plural,primera,_) --> ['Nosotras'].
pronombre(masculino,singular,tercera,_) --> ['El'].
pronombre(femenino,singular,tercera,_) --> ['Ella'].
pronombre(masculino,plural,tercera,_) --> ['Ellos'].
pronombre(femenino,plural,tercera,_) --> ['Ellas'].
pronombre(_,singular,primera,oblicuo) --> ['Me'].
pronombre(_,singular,segunda,nominativo) --> ['Te'].
pronombre(_,plural,primera,oblicuo) --> ['Nos'].
pronombre(_,_,tercera,oblicuo) --> ['Se'].

pronombre(_,_,_,_) --> [].

%Nombres propios
nombre_propio(femenino) --> ['Monica'].
nombre_propio(masculino) --> ['Luis Pedro'].
nombre_propio(masculino) --> ['Marco'].
nombre_propio(_) --> [].

%Preposiciones
preposicion(a) --> ['a'].
preposicion(al) --> ['al'].
preposicion(ante) --> ['ante'].
preposicion(bajo) --> ['bajo'].
preposicion(cabe) --> ['cabe'].
preposicion(con) --> ['con'].
preposicion(contra) --> ['contra'].
preposicion(de)-->['de'].
preposicion(desde) --> ['desde'].
preposicion(durante) --> ['durante'].
preposicion(en) --> ['en'].
preposicion(entre) --> ['entre'].
preposicion(hacia) --> ['hacia'].
preposicion(hasta) --> ['hasta'].
preposicion(mediante) --> ['mediante'].
preposicion(para) --> ['para'].
preposicion(por) --> ['por'].
preposicion(segun) --> ['segun'].
preposicion(segun) --> ['segun'].
preposicion(sin) --> ['sin'].
preposicion(sobre)-->['sobre'].
preposicion(tras) --> ['tras'].

preposicion(a) --> ['A'].
preposicion(al) --> ['Al'].
preposicion(ante) --> ['Ante'].
preposicion(bajo) --> ['Bajo'].
preposicion(cabe) --> ['Cabe'].
preposicion(con) --> ['Con'].
preposicion(contra) --> ['Contra'].
preposicion(de)-->['De'].
preposicion(desde) --> ['Desde'].
preposicion(durante) --> ['Durante'].
preposicion(en) --> ['En'].
preposicion(entre) --> ['Entre'].
preposicion(hacia) --> ['Hacia'].
preposicion(hasta) --> ['Hasta'].
preposicion(mediante) --> ['Mediante'].
preposicion(para) --> ['Para'].
preposicion(por) --> ['Por'].
preposicion(segun) --> ['Segun'].
preposicion(segun) --> ['Segun'].
preposicion(sin) --> ['Sin'].
preposicion(sobre)-->['Sobre'].
preposicion(tras) --> ['Tras'].

%-----------------------------------------
%verbos
verbo(_,_,_,_,_) --> [].
%ser
verbo(indicativo,_,singular,primera,ser) --> ['soy'].
verbo(indicativo,_,singular,segunda,ser) --> ['sos'].
verbo(indicativo,_,singular,segunda,ser) --> ['eres'].
verbo(indicativo,_,singular,tercera,ser) --> ['es'].
verbo(indicativo,_,plural,primera,ser) --> ['somos'].
verbo(indicativo,_,plural,segunda,ser) --> ['son']. 
verbo(indicativo,_,plural,tercera,ser) --> ['son'].

verbo(participio,_,_,_,ser)-->['sido'].
verbo(gerundio,_,_,_,ser)-->['siendo'].
verbo(infinitivo,_,_,_,ser)-->['ser'].

%estar
verbo(indicativo,_,singular,primera,estar) --> ['estoy'].
verbo(indicativo,_,singular,segunda,estar) --> ['estas'].
verbo(indicativo,_,singular,tercera,estar) --> ['esta'].
verbo(indicativo,_,plural,primera,estar) --> ['estamos'].
verbo(indicativo,_,plural,segunda,estar) --> ['estan']. 
verbo(indicativo,_,plural,tercera,estar) --> ['estan'].

verbo(participio,_,_,_,estar)-->['estado'].
verbo(gerundio,_,_,_,estar)-->['estando'].
verbo(infinitivo,_,_,_,estar)-->['estar'].

%ir
verbo(indicativo,_,singular,primera,ir) --> ['voy'].
verbo(indicativo,_,singular,segunda,ir) --> ['vas'].
verbo(indicativo,_,singular,tercera,ir) --> ['va'].
verbo(indicativo,_,plural,primera,ir) --> ['vamos'].
verbo(indicativo,_,plural,segunda,ir) --> ['van']. 
verbo(indicativo,_,plural,tercera,ir) --> ['van'].

verbo(participio,_,_,_,ir)-->['ido'].
verbo(gerundio,_,_,_,ir)-->['yendo'].
verbo(infinitivo,_,_,_,ir)-->['ir'].

%encontrar
verbo(indicativo,_,singular,primera,encontrar) --> ['encuentro'].
verbo(indicativo,_,singular,segunda,encontrar) --> ['encontras'].
verbo(indicativo,_,singular,tercera,encontrar) --> ['encuentra'].
verbo(indicativo,_,plural,primera,encontrar) --> ['encontramos'].
verbo(indicativo,_,plural,segunda,encontrar) --> ['encuentran']. 
verbo(indicativo,_,plural,tercera,encontrar) --> ['encuentran'].

verbo(participio,_,_,_,encontrar)-->['encontrado'].
verbo(gerundio,_,_,_,encontrar)-->['encontrando'].
verbo(infinitivo,_,_,_,encontrar)-->['encontrar'].

%ubicar
verbo(indicativo,_,singular,primera,ubicar) --> ['ubico'].
verbo(indicativo,_,singular,segunda,ubicar) --> ['ubicas'].
verbo(indicativo,_,singular,tercera,ubicar) --> ['ubica'].
verbo(indicativo,_,plural,primera,ubicar) --> ['ubicamos'].
verbo(indicativo,_,plural,segunda,ubicar) --> ['ubican']. 
verbo(indicativo,_,plural,tercera,ubicar) --> ['ubican'].

verbo(participio,_,_,_,ubicar)-->['ubicado'].
verbo(gerundio,_,_,_,ubicar)-->['ubicando'].
verbo(infinitivo,_,_,_,ubicar)-->['ubicar'].

%dirigir
verbo(indicativo,_,singular,primera,dirigir) --> ['dirijo'].
verbo(indicativo,_,singular,segunda,dirigir) --> ['dirigis'].
verbo(indicativo,_,singular,tercera,dirigir) --> ['dirige'].
verbo(indicativo,_,plural,primera,dirigir) --> ['dirigimos'].
verbo(indicativo,_,plural,segunda,dirigir) --> ['dirigen']. 
verbo(indicativo,_,plural,tercera,dirigir) --> ['dirigen'].

verbo(participio,_,_,_,dirigir)-->['dirigido'].
verbo(gerundio,_,_,_,dirigir)-->['dirigiendo'].
verbo(infinitivo,_,_,_,dirigir)-->['dirigir'].

%viajar
verbo(indicativo,_,singular,primera,viajar) --> ['viajo'].
verbo(indicativo,_,singular,segunda,viajar) --> ['viajas'].
verbo(indicativo,_,singular,tercera,viajar) --> ['viaja'].
verbo(indicativo,_,plural,primera,viajar) --> ['viajamos'].
verbo(indicativo,_,plural,segunda,viajar) --> ['viajan']. 
verbo(indicativo,_,plural,tercera,viajar) --> ['viajan'].

verbo(participio,_,_,_,viajar)-->['viajado'].
verbo(gerundio,_,_,_,viajar)-->['viajando'].
verbo(infinitivo,_,_,_,viajar)-->['viajar'].

%localizar
verbo(indicativo,_,singular,primera,localizar) --> ['localizo'].
verbo(indicativo,_,singular,segunda,localizar) --> ['localizas'].
verbo(indicativo,_,singular,tercera,localizar) --> ['localiza'].
verbo(indicativo,_,plural,primera,localizar) --> ['localizamos'].
verbo(indicativo,_,plural,segunda,localizar) --> ['localizan']. 
verbo(indicativo,_,plural,tercera,localizar) --> ['localizan'].

verbo(participio,_,_,_,localizar)-->['localizado'].
verbo(gerundio,_,_,_,localizar)-->['localizando'].
verbo(infinitivo,_,_,_,localizar)-->['localizar'].

%verbos auxiliares

verbo(aux,participio,_,singular,primera,haber) --> ['he'].
verbo(aux,participio,_,singular,segunda,haber) --> ['has'].
verbo(aux,participio,_,singular,tercera,haber) --> ['ha'].
verbo(aux,participio,_,plural,primera,haber) --> ['hemos'].
verbo(aux,participio,_,plural,segunda,haber) --> ['han']. 
verbo(aux,participio,_,plural,segunda,haber) --> ['habeis']. 
verbo(aux,participio,_,plural,tercera,haber) --> ['han'].

verbo(aux,participio,_,_,_,haber)-->[habido].
verbo(aux,gerundio,_,_,_,haber)-->[habiendo].

verbo(aux,gerundio,_,singular,primera,estar) --> ['estoy'].
verbo(aux,gerundio,_,singular,segunda,estar) --> ['estas'].
verbo(aux,gerundio,_,singular,tercera,estar) --> ['estan'].
verbo(aux,gerundio,_,plural,primera,estar) --> ['estamos'].
verbo(aux,gerundio,_,plural,segunda,estar) --> ['estan']. 
verbo(aux,gerundio,_,plural,segunda,estar) --> ['estais']. 
verbo(aux,gerundio,_,plural,tercera,estar) --> ['estan'].

verbo(aux,participio,_,_,_,estar)-->['estado'].
verbo(aux,gerundio,_,_,_,estar)-->['estando'].

verbo(indicativo,_,singular,primera,querer) --> ['quiero'].
verbo(indicativo,_,singular,segunda,querer) --> ['queres'].
verbo(indicativo,_,singular,segunda,querer) --> ['quieres'].
verbo(indicativo,_,singular,tercera,querer) --> ['quiere'].
verbo(indicativo,_,plural,primera,querer) --> ['queremos'].
verbo(indicativo,_,plural,segunda,querer) --> ['quieren']. 
verbo(indicativo,_,plural,tercera,querer) --> ['quieren'].

verbo(participio,_,_,_,querer)-->[querido].
verbo(gerundio,_,_,_,querer)-->[queriendo].

verbo(indicativo,_,singular,primera,desear) --> ['deseo'];['Deseo'].
verbo(indicativo,_,singular,segunda,desear) --> ['deseas'].
verbo(indicativo,_,singular,tercera,desear) --> ['desea'].
verbo(indicativo,_,plural,primera,desear) --> ['deseamos'].
verbo(indicativo,_,plural,segunda,desear) --> ['desean']. 
verbo(indicativo,_,plural,tercera,desear) --> ['desean'].

verbo(participio,_,_,_,desear)-->['deseado'].
verbo(gerundio,_,_,_,desear)-->['deseando'].

verbo(modal,Modo,Genero,Numero,Persona,querer)-->verbo(Modo,Genero,Numero,Persona,querer). %convertir verbo querer en modal tambien
verbo(modal,Modo,Genero,Numero,Persona,desear)-->verbo(Modo,Genero,Numero,Persona,desear). %convertir verbo desear en modal tambien

