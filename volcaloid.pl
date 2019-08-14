% Nombre y Apellido: Santiago Martin Aspres

sabeCantar(megurineLuka,cancion(nightFever,4)).
sabeCantar(megurineLuka,cancion(foreverYoung,5)).
sabeCantar(hatsuneMiku,cancion(tellYourWorld,4)).
sabeCantar(gumi,cancion(foreverYoung,4)).
sabeCantar(gumi,cancion(tellYourWorld,5)).
sabeCantar(seeU,cancion(novermberRain,6)).
sabeCantar(seeU,cancion(nightFever,5)).

% Punto 1
cantanteNovedoso(Vocaloid):-
    esVocaloid(Vocaloid),
    cantCancionesQueSabe(Vocaloid,Cant),
    tiempoTotalCanciones(Vocaloid,TiempoTotal),
    TiempoTotal < 15,
    Cant >= 2.

esVocaloid(Vocaloid):-
    sabeCantar(Vocaloid,_).

cantCancionesQueSabe(Vocaloid,Cant):-
    findall(Cancion,sabeCantar(Vocaloid,Cancion),Canciones),
    length(Canciones, Cant).

tiempoDeCanciones(Vocaloid,ListaDeTiempos):-
    findall(Tiempo,sabeCantar(Vocaloid,cancion(_,Tiempo)),ListaDeTiempos).

tiempoTotalCanciones(Vocaloid,TiempoTotal):-
    tiempoDeCanciones(Vocaloid,ListaDeTiempos),
    sumlist(ListaDeTiempos,TiempoTotal).

% Punto 2
esAcelerado(Vocaloid):-
    esVocaloid(Vocaloid),
    tiempoDeCanciones(Vocaloid,ListaDeTiempos),
    max_member(Maximo,ListaDeTiempos),
    Maximo =< 4.
    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Punto 1

%De cada concierto se sabe su nombre, el país donde se realizará, una cantidad de fama y el tipo de concierto.
%concierto(Nombre,Pais,CantFama,gigante(CantMin,TiempoMinimo))
concierto(mikuExpo,estadosUnidos,2000,gigante(2,6)).
concierto(magicalMirai,japon,3000,gigante(3,10)).
concierto(vocalektVisions,estadosUnidos,1000,mediano(9)).
concierto(mikuFest,argentina,100,pequenio(4)).

% Punto 2

puedeParticipar(hatsuneMiku,mikuExpo).
puedeParticipar(hatsuneMiku,magicalMirai).
puedeParticipar(hatsuneMiku,vocalektVisions).
puedeParticipar(hatsuneMiku,mikuFest).

puedeParticipar(Vocaloid,Concierto):-
    esVocaloid(Vocaloid),
    tipoDeConcierto(Concierto,TipoDeConcierto),
    cumpleCondiciones(Vocaloid,TipoDeConcierto).

tipoDeConcierto(Concierto,TipoDeConcierto):-
    concierto(Concierto,_,_,TipoDeConcierto).

cumpleCondiciones(Vocaloid,grande(CantMin,TiempoTotalMin)):-
    cantCancionesQueSabe(Vocaloid,CantV),
    tiempoTotalCanciones(Vocaloid,TiempoTotalV),
    CantV > CantMin,
    TiempoTotalV >= TiempoTotalMin.
cumpleCondiciones(Vocaloid, mediano(Tiempo)):-
    tiempoTotalCanciones(Vocaloid,TiempoV),
    TiempoV < Tiempo.
cumpleCondiciones(Vocaloid, mediano(Tiempo)):-
    sabeCantar(Vocaloid, cancion(_,TiempoV)),
    TiempoV > Tiempo.

% Punto 3

puntosDeFama(Concierto,PuntosDeFama):-
    concierto(Concierto,_,PuntosDeFama,_).

famaTotal(Vocaloid,Fama):-
    findall(PuntosDeFama,(puedeParticipar(Vocaloid,Concierto),puntosDeFama(Concierto,PuntosDeFama)),Puntos),
    sumlist(Puntos,Fama).

nivelFama(Vocaloid, NivelDeFama):-
    famaTotal(Vocaloid, Fama),
    cantCancionesQueSabe(Vocaloid,Cant),
    NivelDeFama is Fama * Cant.

masFamoso(Vocaloid):-
    esVocaloid(Vocaloid),
forall((esVocaloid(Vocaloid2),Vocaloid \= Vocaloid2),(nivelFama(Vocaloid,NivelFama1),nivelFama(Vocaloid2,NivelFama2),NivelFama1>NivelFama2)).

% Punto 4

conoceA(megurineLuka,hatsuneMiku).
conoceA(megurineLuka,gumi).
conoceA(gumi,seeU).
conoceA(seeU,kaito).

unicoQueParticipaEnConcierto(Vocaloid,Concierto):-
    puedeParticipar(Vocaloid,Concierto),
    not((conocido(Vocaloid,OtroCantante),
    puedeParticipar(OtroCantante,Concierto))).   

conocido(VocaloidA,VocaloidB):-
    conoceA(VocaloidA,VocaloidB).
conocido(VocaloidA,VocaloidC):-
    conoceA(VocaloidA,VocaloidB),
    conocido(VocaloidB,VocaloidC).

/* Punto 5

    No habría que realizar cambios, solo podriamos modelar otra regla de cumple condiciones ya que
    las condiciones a cumplir dependen del nuevo tipo de concierto, pero solo si el ejercicio lo requiere.
    Esto fue gracias al polimorfismo ya que se puede trabajar asi el nuevo tipo de Concierto particular sin tocar tanto el codigo.
*/