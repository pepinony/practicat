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

tieneExperiencia :: Hechicero -> Bool
tieneExperiencia hechicero = antiguedad hechicero > 1

equipoEstaPreparado :: [Hechicero]->Bool
equipoEstaPreparado equipo = length equipo > 3
 
subirDeGrado :: Hechicero -> Hechicero
subirDeGrado hechicero | grado hechicero == 0 = hechicero
                       | otherwise = Hechicero (antiguedad hechicero) (clan hechicero)  (grado hechicero - 1)

esPrestigioso:: Hechicero->Bool
esPrestigioso hechicero = clan hechicero == "Gojo" || clan hechicero == "Kamo" ||clan hechicero == "Zenine"
