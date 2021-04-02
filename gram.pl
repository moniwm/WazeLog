

oracion(X):-oracion(X,[]).

%Definición de estructura de la oración 
oracion -->
	coordinacion_sintagma_nominal(Genero, Numero, Persona, nominativo). %evalua la posibilidad de un sintagma nominal compuesto i.e. Maria y yo queremos ir a San Jose
	%coordinacion_sintagma_verbal(indicativo, Genero, Numero, Persona).

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

%En un predicado compuesto:
genero(masculino,_,masculino). %si el primer objeto es genero masculino, se utiliza el genero masculino  
genero(femenino,X,X). %si el primero objeto es femenino, se usa el genero del segundo objeto

%sintagmas nominales
sintagma_nominal(GENERO,NUMERO,tercera,_) --> determinante(GENERO,NUMERO),sustantivo(GENERO,NUMERO).%,op_sa(GENERO,NUMERO),op_sp,op_rel(GENERO,NUMERO),op_apos.
sintagma_nominal(GENERO,NUMERO,PERSONA,CASO) --> pro_personal(GENERO,NUMERO,PERSONA,CASO).
sintagma_nominal(GENERO,singular,tercera,_) --> nombre_propio(GENERO).

%Forma de tratamiento en predicados compuestos
persona(primera,_,primera).
persona(segunda, Persona2, PersonaFinal) :- (
			Persona2 == primera, !, PersonaFinal = primera;
			PersonaFinal = segunda
		).
persona(tercera,X,X).

%----------------------------
% Léxico:

coma --> [','].
y --> ['y'].
y --> ['o'].

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
determinante(masculino,singular) --> ['algún'].
determinante(masculino,plural) --> ['algunos'].
determinante(femenino,singular) --> ['alguna'].
determinante(femenino,plural) --> ['algunas'].

sustantivo(femenino,singular) --> ['casa'].
sustantivo(femenino,plural) --> ['casas'].
sustantivo(masculino, singular) --> ['supermercado'].
sustantivo(masculino, singular) --> ['mall'].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%----------------------------------------------------------------------------------------
oracion0(X):-oracion0(presencia,X,[]).

%oracion
oracion0(AUSENCIA) --> sn_coord(GENERO,NUMERO,PERSONA,nominativo),sv_coord(indicativo,GENERO,NUMERO,PERSONA,AUSENCIA).
oracion0(AUSENCIA) --> sv_coord(indicativo,_,_,_,AUSENCIA). %sujeto tácito

%coordinación de sintagmas nominales

sn_coord(GENERO,plural,PERSONA,CASO) --> sn(GEN1,_,PER1,CASO),y,sn(GEN2,_,PER2,CASO),
										 {gen(GEN1,GEN2,GENERO),pers(PER1,PER2,PERSONA)}.
sn_coord(GENERO,plural,PERSONA,CASO) --> sn(GEN1,_,PER1,CASO),coma,sn_coord(GEN2,_,PER2,CASO),
									     {gen(GEN1,GEN2,GENERO),pers(PER1,PER2,PERSONA)}.
									     
sn_coord(GENERO,NUMERO,PERSONA,CASO)-->sn(GENERO,NUMERO,PERSONA,CASO). %si no hay

gen(masculino,_,masculino).
gen(femenino,X,X).

pers(primera,_,primera).
pers(segunda,primera,primera).
pers(segunda,segunda,segunda). %se puede contraer con cut
pers(segunda,tercera,segunda).
pers(tercera,X,X).



%coordinación de sintagmas verbales
sv_coord(MODO,GENERO,NUMERO,PERSONA,AUSENCIA) --> sv(MODO,GENERO,NUMERO,PERSONA,AUSENCIA),y,sv(MODO,GENERO,NUMERO,PERSONA,AUSENCIA).
sv_coord(MODO,GENERO,NUMERO,PERSONA,AUSENCIA) --> sv(MODO,GENERO,NUMERO,PERSONA,AUSENCIA),coma,sv_coord(MODO,GENERO,NUMERO,PERSONA,AUSENCIA).

%si no hay coordinación
sv_coord(MODO,GENERO,NUMERO,PERSONA,AUSENCIA) --> sv(MODO,GENERO,NUMERO,PERSONA,AUSENCIA).

%sintagma nominal (faltan modificadores)
sn(GENERO,NUMERO,tercera,_) --> determinante(GENERO,NUMERO),sustantivo(GENERO,NUMERO,_),op_sa(GENERO,NUMERO),op_sp,op_rel(GENERO,NUMERO),op_apos.
sn(GENERO,NUMERO,PERSONA,CASO) --> pro_personal(GENERO,NUMERO,PERSONA,CASO).
sn(GENERO,singular,tercera,_) --> nombre_propio(GENERO).

op_sa(GENERO,NUMERO)-->sa_coord(GENERO,NUMERO).
op_sa(_,_)-->[].
op_sp-->sp_coord(_),op_sp. %se admiten varios sp (la casa de María en Buenos Aires)
op_sp-->[].
op_apos-->[].
op_apos-->apos.

op_rel(_,_)-->[].
op_rel(GENERO,NUMERO)-->rel(GENERO,NUMERO).


%aposición
apos-->coma,sn_coord(_,_,_,nominativo),coma.

%oracion relativa
rel(GENERO,NUMERO)-->pron_rel(GENERO,NUMERO),oracion0(ausencia).

%coordinación sintagma adjetival
sa_coord(GENERO,NUMERO) --> sa(GENERO,NUMERO),y,sa(GENERO,NUMERO).
sa_coord(GENERO,NUMERO) --> sa(GENERO,NUMERO),coma,sa_coord(GENERO,NUMERO).

sa_coord(GENERO,NUMERO) --> sa(GENERO,NUMERO).

%sintagma adjetival

sa(GENERO,NUMERO) --> adverbio(cantidad),adjetivo(GENERO,NUMERO).
sa(GENERO,NUMERO) --> adjetivo(GENERO,NUMERO).


%coordinación de sp (se permite usando la misma preposición únicamente)
sp_coord(PREP)-->sp(PREP),y,sp(PREP).
sp_coord(PREP)-->sp(PREP),coma,sp_coord(PREP).

sp_coord(PREP)-->sp(PREP).

%sintagma proposicional

sp(PREP)-->preposicion(PREP),sn_coord(_,_,_,oblicuo).

%sintagma verbal

%predicativo subjetivo obligatorio
sv(MODO,GENERO,NUMERO,PERSONA,AUSENCIA) --> pre_adv,auxiliares(MODO,GENERO,NUMERO,PERSONA,_,PIDE),verbo(PIDE,GENERO,NUMERO,PERSONA,LEXEMA),
											sa_coord(GENERO,NUMERO),{copulativo(LEXEMA)}.

%María mira:			
sv(MODO,GENERO,NUMERO,PERSONA,AUSENCIA) --> pre_adv,auxiliares(MODO,GENERO,NUMERO,PERSONA,_,PIDE),verbo(PIDE,GENERO,NUMERO,PERSONA,LEXEMA),op_sa,
											{diatesis(LEXEMA,transitivo);diatesis(LEXEMA,intransitivo)}.
%María mira la casa:										
sv(MODO,GENERO,NUMERO,PERSONA,AUSENCIA) --> pre_adv,auxiliares(MODO,GENERO,NUMERO,PERSONA,_,PIDE),verbo(PIDE,GENERO,NUMERO,PERSONA,LEXEMA),
											od(AUSENCIA),op_sa,{diatesis(LEXEMA,tr_estricto);diatesis(LEXEMA,transitivo)}.
%María la mira:		
sv(MODO,GENERO,NUMERO,PERSONA,AUSENCIA) --> pre_adv,pron_od(AUSENCIA),auxiliares(MODO,GENERO,NUMERO,PERSONA,_,PIDE),verbo(PIDE,GENERO,NUMERO,PERSONA,LEXEMA),
											op_sa,{diatesis(LEXEMA,tr_estricto);diatesis(LEXEMA,transitivo)}.
%María la mira a la casa (español rioplatense):
sv(MODO,GENERO,NUMERO,PERSONA,AUSENCIA) --> pre_adv,pron_od(AUSENCIA,GENOBJ,NUMOBJ,PERSOBJ),auxiliares(MODO,GENERO,NUMERO,PERSONA,_,PIDE),verbo(PIDE,GENERO,NUMERO,PERSONA,LEXEMA),
											od(AUSENCIA,GENOBJ,NUMOBJ,PERSOBJ),op_sa,{diatesis(LEXEMA,tr_estricto);diatesis(LEXEMA,transitivo)}.

%con dos objetos
%María lee el libro a Juan:
sv(MODO,GENERO,NUMERO,PERSONA,AUSENCIA) --> pre_adv,auxiliares(MODO,GENERO,NUMERO,PERSONA,_,PIDE),verbo(PIDE,GENERO,NUMERO,PERSONA,LEXEMA),
											od(AUSENCIA),oi(_,_,_),op_sa,{diatesis(LEXEMA,ditransitivo)}.
%María le lee el libro a Juan:
sv(MODO,GENERO,NUMERO,PERSONA,AUSENCIA) --> pre_adv,pro_pers_atono(GENOBJ,NUMOBJ,PERSOBJ,dativo),auxiliares(MODO,GENERO,NUMERO,PERSONA,_,PIDE),verbo(PIDE,GENERO,NUMERO,PERSONA,LEXEMA),
											od(AUSENCIA),oi(GENOBJ,NUMOBJ,PERSOBJ),op_sa,{diatesis(LEXEMA,ditransitivo)}.											
%María le lee el libro
sv(MODO,GENERO,NUMERO,PERSONA,AUSENCIA) --> pre_adv,pro_pers_atono(_,_,_,dativo),auxiliares(MODO,GENERO,NUMERO,PERSONA,_,PIDE),verbo(PIDE,GENERO,NUMERO,PERSONA,LEXEMA),
											od(AUSENCIA),op_sa,{diatesis(LEXEMA,ditransitivo)}.	

%María se lo lee									
sv(MODO,GENERO,NUMERO,PERSONA,presencia) --> pre_adv,pron_oi(_,_,_),pron_od(presencia),auxiliares(MODO,GENERO,NUMERO,PERSONA,_,PIDE),verbo(PIDE,GENERO,NUMERO,PERSONA,LEXEMA),
											op_sa,{diatesis(LEXEMA,ditransitivo)}.
%María se lo lee a Juan											
sv(MODO,GENERO,NUMERO,PERSONA,presencia) --> pre_adv,pron_oi(GENOBJ,NUMOBJ,PERSOBJ),pron_od(presencia),auxiliares(MODO,GENERO,NUMERO,PERSONA,_,PIDE),verbo(PIDE,GENERO,NUMERO,PERSONA,LEXEMA),
											oi(GENOBJ,NUMOBJ,PERSOBJ),op_sa,{diatesis(LEXEMA,ditransitivo)}.														
 
			
%auxiliares
auxiliares(MODO,_,_,_,_,MODO)-->[]. %permito la ausencia de auxiliares

auxiliares(MODO,GENERO,NUMERO,PERSONA,CAT,PIDE)-->verbo(TIPO,MODO,GENERO,NUMERO,PERSONA,LEXEMA),{categoria(TIPO,LEXEMA,CAT),pide(CAT,PIDE)}.
auxiliares(MODO,GENERO,NUMERO,PERSONA,CAT,PIDE)-->verbo(TIPO,MODO,GENERO,NUMERO,PERSONA,LEXEMA),auxiliares(MODO2,_,_,_,CAT2,PIDE),
							{categoria(TIPO,LEXEMA,CAT), CAT \= CAT2, pide(CAT,MODO2) }.

pide(modal,infinitivo).
pide(haber,participio).
pide(estar,gerundio).

categoria(aux,estar,estar).
categoria(aux,haber,haber).
categoria(modal,_,modal).

%pre-adverbios
pre_adv-->[].
pre_adv-->adverbio(afirmacion).
pre_adv-->adverbio(negacion).

%objeto directo

od(ausencia)-->[].
od(presencia)-->sn_coord(GENERO,NUMERO,PERSONA,oblicuo).
od(presencia)-->[a],sn_coord(GENERO,NUMERO,PERSONA,oblicuo). %persona/personificación
od(presencia,GENERO,NUMERO,PERSONA)-->[a],sn_coord(GENERO,NUMERO,PERSONA,oblicuo). %rioplatense
od(ausencia,_,_,_)-->[].

pron_od(ausencia)-->[].
pron_od(presencia)-->pro_pers_atono(_,_,_,acusativo).
pron_od(presencia,GENERO,NUMERO,PERSONA)-->pro_pers_atono(GENERO,NUMERO,PERSONA,acusativo). %rioplatense
pron_od(ausencia,_,_,_)-->[].


%objeto indirecto 

oi(GENERO,NUMERO,PERSONA)-->[a],sn_coord(GENERO,NUMERO,PERSONA,oblicuo).

pron_oi(GENERO,NUMERO,tercera)-->[se]. %pronombre modificado por pronunciación

pron_oi(GENERO,NUMERO,PERSONA)-->pro_pers_atono(GENERO,NUMERO,PERSONA,dativo),{not(PERSONA = tercera)}.

%sintagma adverbial

op_sa-->[].
op_sa-->sa.
sa-->op_cantidad,adverbio(modo).
sa-->adverbio(lugar).
sa-->adverbio(tiempo).
op_cantidad-->[].
op_cantidad-->adverbio(cantidad).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Léxico:



determinante(femenino,plural) --> [las].
determinante(femenino,singular) --> [la].
determinante(masculino,plural) --> [los].
determinante(masculino,singular) --> [el].

pro_personal(_,singular,primera,nominativo) --> [yo].
pro_personal(_,singular,primera,oblicuo) --> [mi].
pro_personal(_,singular,segunda,nominativo) --> [vos].
pro_personal(_,singular,segunda,oblicuo) --> [vos].
pro_personal(_,singular,segunda,nominativo) --> [tu].
pro_personal(_,singular,segunda,oblicuo) --> [ti].
pro_personal(masculino,plural,primera,_) --> [nosotros].
pro_personal(femenino,plural,primera,_) --> [nosotras].
pro_personal(masculino,singular,tercera,_) --> [el].
pro_personal(femenino,singular,tercera,_) --> [ella].
pro_personal(masculino,plural,tercera,_) --> [ellos].
pro_personal(femenino,plural,tercera,_) --> [ellas].

pro_pers_atono(_,singular,primera,_)-->[me].
pro_pers_atono(_,singular,segunda,_)-->[te].
pro_pers_atono(masculino,singular,tercera,acusativo)-->[lo].
pro_pers_atono(femenino,singular,tercera,acusativo)-->[la].
pro_pers_atono(_,singular,tercera,dativo)-->[le].
pro_pers_atono(_,plural,primera,_)-->[nos].
pro_pers_atono(masculino,plural,segunda,_)-->[los]. %uds
pro_pers_atono(femenino,plural,segunda,_)-->[las]. %uds
pro_pers_atono(_,plural,segunda,_)-->[os]. %vosotros
pro_pers_atono(masculino,plural,tercera,acusativo)-->[los].
pro_pers_atono(femenino,plural,tercera,acusativo)-->[las].
pro_pers_atono(_,plural,tercera,dativo)-->[les].

sustantivo(femenino,plural,casa) --> [casas].
sustantivo(femenino,singular,casa) --> [casa].
sustantivo(femenino,plural,ciudad) --> [ciudades].
sustantivo(femenino,singular,ciudad) --> [ciudad].
sustantivo(masculino,plural,perro) --> [perros].
sustantivo(masculino,singular,perro) --> [perro].
sustantivo(masculino,plural,mundo) --> [mundos].
sustantivo(masculino,singular,mundo) --> [mundo].

nombre_propio(femenino) --> [maria].
nombre_propio(femenino) --> [juana].
nombre_propio(masculino) --> [juan].
nombre_propio(masculino) --> [pedro].
nombre_propio(_) --> [buenosaires].
nombre_propio(_) --> [nuevayork].
 
%verbos
 
verbo(infinitivo,_,_,_,X)-->[X].
verbo(_,infinitivo,_,_,_,X)-->[X].

%comer

verbo(indicativo,_,singular,primera,comer) --> [como].
verbo(indicativo,_,singular,segunda,comer) --> [comes].
verbo(indicativo,_,singular,tercera,comer) --> [come].
verbo(indicativo,_,plural,primera,comer) --> [comemos].
verbo(indicativo,_,plural,segunda,comer) --> [comen]. %ustedes
verbo(indicativo,_,plural,segunda,comer) --> [comeis]. %vosotros
verbo(indicativo,_,plural,tercera,comer) --> [comen].

verbo(participio,_,_,_,comer)-->[comido].
verbo(gerundio,_,_,_,comer)-->[comiendo].

diatesis(comer,transitivo).

%conocer

verbo(indicativo,_,singular,primera,conocer) --> [conozco].
verbo(indicativo,_,singular,segunda,conocer) --> [conoces].
verbo(indicativo,_,singular,tercera,conocer) --> [conoce].
verbo(indicativo,_,plural,primera,conocer) --> [conocemos].
verbo(indicativo,_,plural,segunda,conocer) --> [conocen]. %ustedes
verbo(indicativo,_,plural,segunda,conocer) --> [conoceis]. %vosotros
verbo(indicativo,_,plural,tercera,conocer) --> [conocen].

verbo(participio,_,_,_,conocer)-->[conocido].
verbo(gerundio,_,_,_,conocer)-->[conociendo].

diatesis(conocer,tr_estricto).

%dar

verbo(indicativo,_,singular,primera,dar) --> [doy].
verbo(indicativo,_,singular,segunda,dar) --> [das].
verbo(indicativo,_,singular,tercera,dar) --> [da].
verbo(indicativo,_,plural,primera,dar) --> [damos].
verbo(indicativo,_,plural,segunda,dar) --> [dan]. %ustedes
verbo(indicativo,_,plural,segunda,dar) --> [dais]. %vosotros
verbo(indicativo,_,plural,tercera,dar) --> [dan].

verbo(participio,_,_,_,dar)-->[dado].
verbo(gerundio,_,_,_,dar)-->[dando].

diatesis(dar,ditransitivo).

%ser

verbo(indicativo,_,singular,primera,ser) --> [soy].
verbo(indicativo,_,singular,segunda,ser) --> [sos].
verbo(indicativo,_,singular,segunda,ser) --> [eres].
verbo(indicativo,_,singular,tercera,ser) --> [es].
verbo(indicativo,_,plural,primera,ser) --> [somos].
verbo(indicativo,_,plural,segunda,ser) --> [son]. %ustedes
verbo(indicativo,_,plural,segunda,ser) --> [sois]. %vosotros
verbo(indicativo,_,plural,tercera,ser) --> [son].

verbo(participio,_,_,_,ser)-->[sido].
verbo(gerundio,_,_,_,ser)-->[siendo].

copulativo(ser).

%auxiliares

verbo(aux,indicativo,_,singular,primera,haber) --> [he].
verbo(aux,indicativo,_,singular,segunda,haber) --> [has].
verbo(aux,indicativo,_,singular,tercera,haber) --> [ha].
verbo(aux,indicativo,_,plural,primera,haber) --> [hemos].
verbo(aux,indicativo,_,plural,segunda,haber) --> [han]. %ustedes
verbo(aux,indicativo,_,plural,segunda,haber) --> [habeis]. %vosotros
verbo(aux,indicativo,_,plural,tercera,haber) --> [han].

verbo(aux,participio,_,_,_,haber)-->[habido].
verbo(aux,gerundio,_,_,_,haber)-->[habiendo].

verbo(aux,indicativo,_,singular,primera,estar) --> [estoy].
verbo(aux,indicativo,_,singular,segunda,estar) --> [estas].
verbo(aux,indicativo,_,singular,tercera,estar) --> [estan].
verbo(aux,indicativo,_,plural,primera,estar) --> [estamos].
verbo(aux,indicativo,_,plural,segunda,estar) --> [estan]. %ustedes
verbo(aux,indicativo,_,plural,segunda,estar) --> [estais]. %vosotros
verbo(aux,indicativo,_,plural,tercera,estar) --> [estan].

verbo(aux,participio,_,_,_,estar)-->[estado].
verbo(aux,gerundio,_,_,_,estar)-->[estando].

verbo(indicativo,_,singular,primera,querer) --> [quiero].
verbo(indicativo,_,singular,segunda,querer) --> [queres].
verbo(indicativo,_,singular,segunda,querer) --> [quieres].
verbo(indicativo,_,singular,tercera,querer) --> [quiere].
verbo(indicativo,_,plural,primera,querer) --> [queremos].
verbo(indicativo,_,plural,segunda,querer) --> [quieren]. %ustedes
verbo(indicativo,_,plural,segunda,querer) --> [quereis]. %vosotros
verbo(indicativo,_,plural,tercera,querer) --> [quieren].

verbo(participio,_,_,_,querer)-->[querido].
verbo(gerundio,_,_,_,querer)-->[queriendo].

diatesis(querer,tr_estricto).

verbo(modal,MODO,GENERO,NUMERO,PERSONA,querer)-->verbo(MODO,GENERO,NUMERO,PERSONA,querer). %también modal

%adjetivos

adjetivo(masculino,singular) --> [lindo].
adjetivo(femenino,singular) --> [linda].
adjetivo(masculino,plural) --> [lindos].
adjetivo(femenino,plural) --> [lindas].
adjetivo(_,singular) --> [grande].
adjetivo(_,plural) --> [grandes].

adverbio(cantidad) --> [muy].
adverbio(cantidad) --> [apenas].
adverbio(cantidad) --> [mas].
adverbio(afirmacion) --> [si].
adverbio(afirmacion) --> [tambien].
adverbio(negacion) --> [no].
adverbio(negacion) --> [tampoco].
adverbio(modo) --> [rapidamente].
adverbio(tiempo) --> [ayer].
adverbio(lugar) --> [ahi].

preposicion(de)-->[de].
preposicion(sobre)-->[sobre].
preposicion(en)-->[en].

pron_rel(_,_)-->[que].
pron_rel(_,singular)-->[quien].
pron_rel(_,plural)-->[quienes].
coma --> [coma].
y --> [y].
y --> [o].
