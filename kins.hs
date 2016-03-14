module Kins
  ( Selo(..)
  , Tom(..)
  , Cor(..)
  , Kin
  , findKin
  , findCor
  , findTom
  , findSelo
  , findGuia
  , findAnalogo
  , findAntipoda
  , findOculto
  , findOndaEncantada
  , tzolkin
  , seloIndex
  , tomIndex
  , kinIndex
  , ondaEncantada
  ) where


import Data.List
import Data.Maybe


data Selo
  = Dragao
  | Vento
  | Noite
  | Semente
  | Serpente
  | EnlacadorDeMundos
  | Mao
  | Estrela
  | Lua
  | Cachorro
  | Macaco
  | Humano
  | CaminhanteDoCeu
  | Mago
  | Aguia
  | Guerreiro
  | Terra
  | Espelho
  | Tormenta
  | Sol
  deriving (Eq, Ord, Show, Read, Bounded, Enum)


data Tom
  = Magnetico
  | Lunar
  | Eletrico
  | AutoExistente
  | Harmonico
  | Ritmico
  | Ressonante
  | Galatico
  | Solar
  | Planetario
  | Espectral
  | Cristal
  | Cosmico
  deriving (Eq, Ord, Show, Read, Bounded, Enum)


data Cor
  = Vermelho
  | Branco
  | Azul
  | Amarelo
  deriving (Eq, Ord, Show, Read, Bounded, Enum)


data Kin = Kin Selo Tom
  deriving (Eq, Show, Read)


selos = [Dragao .. Sol]
tons = [Magnetico .. Cosmico]
cores = [Vermelho .. Amarelo]


(!!<) :: [a] -> Int -> a
list !!< index =
  let
    size = length list
    i = index `mod` size
    posInd = if i < 0 then size - i else i
  in
    list !! posInd


(!!?) :: (Eq a) => [a] -> a -> Int
list !!? el =
  fromMaybe 0 $ el `elemIndex` list


tzolkin :: [Kin]
tzolkin =
  let
    tzolkinLength = length tons * length selos
  in
    map findKin [1..tzolkinLength]


ondasEncantadas :: [Kin]
ondasEncantadas =
  filter (\(Kin _ tom) -> tom == Magnetico) tzolkin


seloIndex :: Selo -> Int
seloIndex selo =
  succ $ selos !!? selo


tomIndex :: Tom -> Int
tomIndex tom =
  succ $ tons !!? tom


kinIndex :: Kin -> Int
kinIndex kin =
  succ $ tzolkin !!? kin


findSelo :: Int -> Selo
findSelo n =
  selos !!< (n - 1)


findTom :: Int -> Tom
findTom n =
  tons !!< (n - 1)


findCor :: Int -> Cor
findCor n =
  cores !!< (n - 1)


findKin :: Int -> Kin
findKin n =
  Kin (findSelo n) (findTom n)


findGuia :: Kin -> Kin
findGuia (Kin selo tom) =
  let
    tomDots = (flip mod) 5 $ tomIndex tom
    kinI = kinIndex (Kin selo tom)
    factorList = [1, 2, 3, 4, 0]
  in
    findKin . (+kinI) . (*52) $ factorList !!? tomDots


findAnalogo :: Kin -> Kin
findAnalogo (Kin selo tom) =
  let
    aSelo = findSeloDiff (19-) selo
  in
    (Kin aSelo tom)


findSeloDiff :: (Int -> Int) -> Selo -> Selo
findSeloDiff f selo =
  findSelo . f $ seloIndex selo


findAntipoda :: Kin -> Kin
findAntipoda (Kin selo tom) =
  let
    aSelo = findSeloDiff (10+) selo
  in
    (Kin aSelo tom)


findOculto :: Kin -> Kin
findOculto (Kin selo tom) =
  let
    oSelo = findSeloDiff (21-) selo
    oTom = findTom . (14-) $ tomIndex tom
  in
    (Kin oSelo oTom)


findOndaEncantada :: Kin -> Kin
findOndaEncantada (Kin selo tom) =
  let
    index = kinIndex (Kin selo tom)
    tomI = tomIndex $ tom
  in
    findKin $ index - (tomI - 1)


findOndaPosition :: Selo -> Int
findOndaPosition selo =
  succ $ ondasEncantadas !!? Kin selo Magnetico


findFamilia :: Kin -> [Selo]
findFamilia (Kin selo _) =
  let
    rest = (flip mod) 5 $ seloIndex selo
  in
    map (\x -> findSelo x) [rest,(rest + 5)..(length selos - 1)]


seloColor :: Selo -> Cor
seloColor selo =
  findCor . (flip mod) 4 $ seloIndex selo


readKin :: Kin -> String
readKin (Kin selo tom) =
  show selo ++ " " ++ show tom ++ " " ++ show (seloColor selo)


ondaEncantada :: Kin -> [Kin]
ondaEncantada kin =
  let
    magnetico = findOndaEncantada kin
    index = kinIndex magnetico
  in
    map findKin [index .. (index + 12)]