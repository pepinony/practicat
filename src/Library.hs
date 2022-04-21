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

esEspecial :: Hechicero->Bool
esEspecial hehcicero = grado hechicero == 0

tieneExperiencia :: Hechicero -> Bool
tieneExperiencia hechicero = antiguedad hechicero > 1

type Equipo = [Hechicero]

equipoEstaPreparado :: Equipo->Bool
equipoEstaPreparado equipo = length equipo > 3
 
subirDeGrado :: Hechicero -> Hechicero
subirDeGrado hechicero | esEspecial hechicero = hechicero
                       | otherwise = hechicero {grado hechicero = (grado hechicero - 1)}
                       
clanesPrestigioso = ["Gojo", "Kamo", "Zenine"]

esPrestigioso:: Hechicero->Bool
esPrestigioso hechicero = elem clan hechicero clanesPrestigiosos
