module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Hechicero = Hechicero {
    antiguedad :: Number,
    clan :: String,
    grado :: Number
} deriving (Eq, Show)

nobara = Hechicero 1 "Kugisaki" 3
tito = Hechicero 3 "Hokague" 1
raul = Hechicero 200 "Ninja" 0

esEspecial :: Hechicero->Bool
esEspecial hechicero = grado hechicero == 0

tieneExperiencia :: Hechicero -> Bool
tieneExperiencia hechicero = antiguedad hechicero > 1

type Equipo = [Hechicero]

equipoA = [tito, nobara, raul]

estaPreparado :: Equipo->Bool
estaPreparado equipo = length equipo > 3
 
subirDeGrado :: Hechicero -> Hechicero
subirDeGrado hechicero | esEspecial hechicero = hechicero
                       | otherwise = hechicero{grado = grado hechicero - 1}
                       
clanesPrestigiosos = ["Gojo", "Kamo", "Zenine"]

esPrestigioso:: Hechicero->Bool
esPrestigioso hechicero = elem (clan hechicero) clanesPrestigiosos

esInvencible :: Equipo->Bool
esInvencible equipo = any esEspecial equipo

favoritoDelLider :: Equipo->Bool
favoritoDelLider equipo = all esPrestigioso equipo

losExpertos :: Equipo -> Equipo
losExpertos equipo = filter tieneExperiencia equipo

puedeConTodo :: Equipo -> Bool
puedeConTodo equipo = esInvencible equipo || estaPreparado equipo

powerUp :: Equipo -> Equipo
powerUp equipo = map subirDeGrado equipo

cuantosEspeciales :: Equipo -> Number
cuantosEspeciales = length.losEspeciales

losEspeciales :: Equipo-> Equipo
losEspeciales equipo = filter esEspecial equipo

promedioDeGrados :: Equipo -> Number
promedioDeGrados equipo = sum (losGrados equipo) / length equipo

losGrados :: Equipo->[Number]
losGrados equipo = map grado equipo

calcular :: (Number, Number)->(Number, Number)
calcular (x, y) | even x && even y = (x*2, y)
                | not (even x) && not (even y)= (x, y+1)
                | not (even x) && even y = (x,y)
                | even x && not (even y) = (x*2, x+1)

------------------------
filtrar :: (a->Bool)->[a]->[a]
filtrar function lista = foldr (aplicacion function) [] lista

aplicacion :: (a->Bool)->a->[a]->[a]
aplicacion function x y| function x = (:) x y
                       | otherwise = y 

maximoSegun :: (a->Number)->[a]->a
maximoSegun function lista = foldl1 (maxSegun function) lista

maxSegun :: (a->Number)->a->a->a
maxSegun function x y | max (function y) (function x)== function x = x
                      | otherwise = y

aparearCon :: (a->a->b)->[a]->[a]->[b]
aparearCon function lista1 lista2 = foldr aparea [] (zip lista1 lista2)
                                    where aparea= (:).(operarSegun function)
operarSegun :: (a->a->b)->(a,a)->b
operarSegun function x = function (fst x) (snd x)

----------------------------

--El parcial q no fue

data Mago = Mago {
    nombre::String,
    horrocruxes:: [Horrocrux]
}deriving(Eq,Show)

data Horrocrux = Horrocrux {
    denominacion::String,
    mago::Mago
}
instance Show Horrocrux where
    show (horrocrux)= show (denominacion horrocrux)
instance Eq Horrocrux where
    h1==h2 = denominacion h1 == denominacion h2

diadema = Horrocrux {
    denominacion = "Ravenclow",
    mago = srTenebroso
}

diario = Horrocrux {
    denominacion = "Diario de Tom Riddle",
    mago = srTenebroso
}

harry = Horrocrux {
    denominacion = "Harry Postre",
    mago = srTenebroso
}

otroMas = Horrocrux{
    denominacion = "asd",
    mago = asd
}
asd = Mago{
    nombre = "asdd",
    horrocruxes = [otroMas]
}

srTenebroso = Mago {
    nombre = "Voldemort",
    horrocruxes = [diadema, diario, harry]
}

destruir :: Horrocrux->Mago
destruir horrocrux = (mago horrocrux){horrocruxes=borrarUno horrocrux (horrocruxes (mago horrocrux))}

borrarUno :: Eq a => a->[a]->[a]
borrarUno elemento lista = filter (elemento/=) lista

destruirHorrocruxes :: [Horrocrux]->Mago
destruirHorrocruxes horrocruxs = (mago (head horrocruxs)){horrocruxes = ((intersectarVarios.(map horrocruxes).(map destruir)) horrocruxs)}

destruirHorrocruxesVariosMago :: [Mago]->[Horrocrux]->[Mago]
destruirHorrocruxesVariosMago [] _ = []
destruirHorrocruxesVariosMago (m:ms) horrocruxs = [destruirHorrocruxes (horrocruxesSegunMago m horrocruxs)] ++ destruirHorrocruxesVariosMago ms horrocruxs 

horrocruxesSegunMago :: Mago->[Horrocrux]->[Horrocrux]
horrocruxesSegunMago magoTenebroso horrocruxs = filter ((magoTenebroso==).mago) horrocruxs

magoSegunHorrocruxes :: [Horrocrux]->[Mago]
magoSegunHorrocruxes horrocruxs = sinRepetidos (map mago horrocruxs)

sinRepetidos :: Eq a => [a]->[a]
sinRepetidos [z] = [z]
sinRepetidos (x:xs) | elem x xs = sinRepetidos xs 
                    | otherwise = sinRepetidos xs ++ [x]

intersectarVarios :: Eq a =>[[a]]->[a]
intersectarVarios listas = foldr1 intersectar listas

intersectar :: Eq a=> [a]->[a]->[a]
intersectar [] _ = []
intersectar _ [] = []
intersectar (x:xs) lista | elem x lista= [x] ++ intersectar xs lista
                         | otherwise = intersectar xs lista

--Primero hice finalFeliz1 pero como no usua la funcion destruir agregue la funcion finalFeliz2 y deja las dos pq me gusto como quedo finalFeliz1
vencerMagoTenebroso :: [Horrocrux]->[Horrocrux]->Bool
vencerMagoTenebroso [] _ = True
vencerMagoTenebroso (primero:restoHorrocrux) horrocruxDestruidos = elem primero horrocruxDestruidos && vencerMagoTenebroso restoHorrocrux horrocruxDestruidos

finalFeliz1 :: [Horrocrux]->Bool
finalFeliz1 = vencerMagoTenebroso (horrocruxes srTenebroso)

finalFeliz2 :: [Horrocrux]->Bool
finalFeliz2 horrocruxs = (([]==).horrocruxes.separarSrTenebroso.(destruirHorrocruxesVariosMago (magoSegunHorrocruxes horrocruxs))) horrocruxs

separarSrTenebroso :: [Mago]->Mago
separarSrTenebroso = head.(filter (("Voldemort"==).nombre))