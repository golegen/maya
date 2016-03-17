module Kins
  ( Selo(..)
  , Tom(..)
  , Cor(..)
  , Kin
  , tzolkin
  , seloIndex
  , tomIndex
  , kinIndex
  , ondaIndex
  , findKin
  , findCor
  , findTom
  , findSelo
  , findGuia
  , findAnalogo
  , findAntipoda
  , findOculto
  , findOndaEncantada
  , findFamilia
  , findHarmonic
  , readKin
  , ondaEncantada
  ) where


import Helpers


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

castle = length tons * length cores
tzolkinLength = length selos * length tons


tzolkin :: [Kin]
tzolkin =
  map findKin [1..tzolkinLength]


ondasEncantadas :: [Kin]
ondasEncantadas =
  filter isMagnetico tzolkin
  where
    isMagnetico (Kin _ tom) = tom == Magnetico


seloIndex :: Selo -> Int
seloIndex selo =
  selos !!? selo + 1


tomIndex :: Tom -> Int
tomIndex tom =
  tons !!? tom + 1


kinIndex :: Kin -> Int
kinIndex kin =
  tzolkin !!? kin + 1


ondaIndex :: Selo -> Int
ondaIndex selo =
  ondasEncantadas !!? (Kin selo Magnetico) + 1


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
  findKin . (+) start . (*) castle $ factorList !!? tomDots
  where
    factorList = [1, 2, 3, 4, 0]
    start = kinIndex (Kin selo tom)
    tomDots = tomIndex tom `rem` 5


findAnalogo :: Kin -> Kin
findAnalogo (Kin selo tom) =
  (Kin selo' tom)
  where
    selo' = findSelo $ 19 - seloIndex selo


findAntipoda :: Kin -> Kin
findAntipoda (Kin selo tom) =
  (Kin selo' tom)
  where
    selo' = findSelo $ 10 + seloIndex selo


findOculto :: Kin -> Kin
findOculto (Kin selo tom) =
  (Kin selo' tom')
  where
    selo' = findSelo $ 21 - seloIndex selo
    tom' = findTom $ 14 - tomIndex tom


findOndaEncantada :: Kin -> Kin
findOndaEncantada (Kin selo tom) =
  findKin $ kinIndex (Kin selo tom) - (tomIndex tom) + 1


findFamilia :: Kin -> [Selo]
findFamilia (Kin selo _) =
  map findSelo [start,step..end]
  where
    start = seloIndex selo `rem` 5
    step  = start + 5
    end   = length selos - 1


findHarmonic :: [Int] -> Kin
findHarmonic list =
  findKin (sum list)


seloColor :: Selo -> Cor
seloColor selo =
  findCor $ seloIndex selo `rem` length cores


readKin :: Kin -> String
readKin (Kin selo tom) =
  show selo ++ " " ++ show tom ++ " " ++ show (seloColor selo)


ondaEncantada :: Kin -> [Kin]
ondaEncantada kin =
  map findKin [start .. end]
  where
    start = kinIndex $ findOndaEncantada kin
    end   = start + 12
