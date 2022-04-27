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