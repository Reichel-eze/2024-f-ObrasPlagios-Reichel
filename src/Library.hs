module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Autor = UnAutor{
    nombre :: String,
    obras :: [Obra]
}deriving (Show,Eq)

data Obra = UnaObra{
    texto :: String,
    anio :: Number
}deriving(Show,Eq)

-- 1) Modelar las siguientes obras y que existan autores que las hayan publicado:

obraA :: Obra
obraA = UnaObra "Había una vez un pato." 1997

obraB :: Obra
obraB = UnaObra "¡Habia una vez un pato!" 1996

obraC :: Obra
obraC = UnaObra "Mirtha, Susana y Moria." 2010

obraD :: Obra 
obraD = UnaObra "La semántica funcional del amoblamiento vertebral es riboficiente" 2020

obraE :: Obra
obraE = UnaObra "La semántica funcional de Mirtha, Susana y Moria." 2022

ezequiel :: Autor
ezequiel = UnAutor "Ezequiel" [obraA, obraC]

reichel :: Autor
reichel = UnAutor "Reichel" [obraB, obraE]

-- 2) Conocer la versión cruda de un texto, que consiste en eliminar los acentos de las letras existentes y 
-- quitar signos de puntuación y todo carácter que no sea una letra o un número. 
-- Por ejemplo, la versión cruda de "Había una vez un pato..." es "Habia una vez un pato"

versionCruda :: String -> String
versionCruda = quitarSignos . tranformarPalabra

tranformarPalabra :: String -> String
tranformarPalabra texto
    | tieneAcento texto = map transformarAcento texto
    | otherwise = texto 

quitarSignos :: String -> String
quitarSignos texto = filter (not . tieneSignos) texto

tieneSignos :: Char -> Bool  
tieneSignos letra = letra `elem` ['!','.','¡']

tieneAcento :: String -> Bool
tieneAcento texto = any esAcento texto

esAcento :: Char -> Bool
esAcento letra = letra `elem` ['á','é','í','ó','ú'] 

transformarAcento :: Char -> Char
transformarAcento 'á' = 'a' 
transformarAcento 'é' = 'e' 
transformarAcento 'í' = 'i' 
transformarAcento 'ó' = 'o' 
transformarAcento 'ú' = 'u' 
transformarAcento letra = letra   

-- 3) Plagios
-- Se desea detectar si una obra es plagio de la otra. Hay distintas formas de reconocer un plagio, de los cuales se conocen las siguientes, 
-- pero podrían haber más. En cualquier caso, una obra debe haber sido publicada en un año posterior a la obra original para ser considerada un plagio. 

type Plagio = Obra -> Obra -> Bool

esPlagio :: Obra -> Obra -> Plagio -> Bool
esPlagio obra1 obra2 plagio
    | anio obra1 > anio obra2 = plagio obra1 obra2    
    | otherwise = False    

-- Copia literal: ocurre cuando la versión cruda de una es igual a la de la otra. Por ejemplo, A es plagio de B. 

copiaLiteral :: Plagio
copiaLiteral obra1 obra2 = versionCruda (texto obra1) == versionCruda (texto obra2)

-- Empieza igual: Los primeros caracteres de una obra son los mismos que otra, y su longitud es menor. La cantidad de caracteres a analizar puede ser variable. 
-- Por ejemplo, E es plagio de D para una cantidad 10, pero no para una cantidad 30.

empiezaIgual :: Number -> Plagio
empiezaIgual cantidad obra1 obra2 = take cantidad (texto obra1) == take cantidad (texto obra2) && longitudMenorObras obra1 obra2

longitudMenorObras :: Obra -> Obra -> Bool
longitudMenorObras obra1 obra2 = length (texto obra1) < length (texto obra2)

--Le agregaron intro: La obra plagiada empieza a su manera, pero al final incluye totalmente el texto de la original. Por ejemplo, E es plagio de C.

leAgregaronIntro :: Plagio 
leAgregaronIntro obra1 obra2 = reverse (take (length (texto obra2)) (reverse (texto obra1)) ) == texto obra2 

--leAgregaronIntro' :: Plagio 
--leAgregaronIntro' obra1 obra2 = reverse . take (length (texto obra2)) . reverse . (texto obra1) == texto obra2 

--Inventar otra forma de detectar plagio, utilizando una expresión lambda.



-- FALTAAAAAAAAAAAAAA





-- Bots
-- Existen diferentes bots, y cada uno detecta diversas formas de plagio. Además se conoce su fabricante.
-- 4) Modelar dos bots de ejemplo, incluyendo todas las formas de detección existentes hasta ahora.

data Bot = UnBot {
    detecciones :: [Plagio],
    fabricante :: String
}deriving (Show,Eq)

botote :: Bot
botote = UnBot [copiaLiteral, empiezaIgual 10] "Microsoft"

botito :: Bot
botito = UnBot [leAgregaronIntro, copiaLiteral] "Apple"

-- 5) Un bot detecta si una obra es plagio de otra si verifica alguna (ANY) de las formas de detección que maneja.

deteccionPlagio :: Bot -> Plagio 
deteccionPlagio bot obra1 obra2 = any (esPlagio obra1 obra2) (detecciones bot)

-- 6) Dado un conjunto de autores y un bot, detectar si es una cadena de plagiadores. 
-- Es decir, el segundo plagió al primero, el tercero al segundo, y así. 
-- Se considera que un autor plagió a otro cuando alguna de sus obras es plagio de alguna de las del otro según el bot.

esCadenaDePlagiadores :: Bot -> [Autor] -> Bool
esCadenaDePlagiadores bot [_] = False
esCadenaDePlagiadores bot [autor1,autor2] = esAutorPlagiador bot autor1 autor2
esCadenaDePlagiadores bot (autor1:autor2:autores) = esAutorPlagiador bot autor1 autor2 && esCadenaDePlagiadores bot (autor2:autores)

esAutorPlagiador :: Bot -> Autor -> Autor -> Bool
esAutorPlagiador bot autor autorOriginal = any (esPlagioDeEsteAutor bot autorOriginal) (obras autor)

esPlagioDeEsteAutor :: Bot -> Autor -> Obra -> Bool
esPlagioDeEsteAutor bot autorOriginal obra = any (deteccionPlagio bot obra) (obras autorOriginal) 