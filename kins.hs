module Kins
  ( Selo(..)
  , Tom(..)
  , Cor(..)
  , Kin
  , findKin
  , findCor
  , findTom
  , findSelo
  , tzolkin
  ) where

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
  deriving (Show, Read)

selos = [Dragao .. Sol]
tons = [Magnetico .. Cosmico]
cores = [Vermelho .. Amarelo]

(!!<) :: [a] -> Int -> a
list !!< index =
  let
    size = length list
  in
    (list!!) $ index `mod` size


findSelo :: Int -> Selo
findSelo n = selos !!< (n - 1)

findTom :: Int -> Tom
findTom n = tons !!< (n - 1)

findCor :: Int -> Cor
findCor n = cores !!< (n - 1)

findKin :: Int -> Kin
findKin n =
  Kin (findSelo n) (findTom n) (findCor n)

tzolkin :: [Kin]
tzolkin =
  let
    tzolkinLength = length tons * length selos
  in
    map findKin [1..tzolkinLength]