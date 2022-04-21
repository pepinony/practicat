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
esInvencible = any esEspecial 

favoritoDelLider :: Equipo->Bool
favoritoDelLider = all esPrestigioso

losExpertos :: Equipo -> Equipo
losExpertos = map tieneExperiencia

puedeConTodo :: Equipo -> Bool
puedeConTodo equipo = esInvencible equipo || estaPreparado equipo

