module Kins
  ( Selo(..)
  , Tom(..)
  , Cor(..)
  , Kin
  , findKin
  , findCor
  , findTom
  , findSelo
  , findOndaEncantada
  , tzolkin
  , seloIndex
  , tomIndex
  , corIndex
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


data Kin = Kin Selo Tom Cor
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
  Kin (findSelo n) (findTom n) (findCor n)


tzolkin :: [Kin]
tzolkin =
  let
    tzolkinLength = length tons * length selos
  in
    map findKin [1..tzolkinLength]


seloIndex :: Selo -> Int
seloIndex selo =
  (+1) $ selos !!? selo


tomIndex :: Tom -> Int
tomIndex tom =
  (+1) $ tons !!? tom


corIndex :: Cor -> Int
corIndex cor =
  (+1) $ cores !!? cor


kinIndex :: Kin -> Int
kinIndex kin =
  (+1) $ tzolkin !!? kin


findOndaEncantada :: Kin -> Kin
findOndaEncantada kin =
  let
    index = kinIndex kin
    tom = tomIndex $ findTom index
  in
    findKin $ index - (tom - 1)


ondaEncantada :: Kin -> [Kin]
ondaEncantada kin =
  let
    magnetico = findOndaEncantada kin
    index = kinIndex magnetico
  in
    map findKin [index .. (index + 12)]