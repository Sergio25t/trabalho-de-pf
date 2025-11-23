module Pairing (runAVEparing, runElimParing) where

import Data.List (sortBy, findIndex, sortOn)
import Data.Ord  (comparing, Down(..))
import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- ------------------- AVE -------------------

runAVEparing
  :: [(String, Char, Int, Int, Double)]
  -> [(String, String, String, (Int, Int), String)]
  -> [(String, String, String, (Int, Int), String)]
runAVEparing jogadores resultadosAntigos =
  let
    nomes = [ n | (n,_,_,_,_) <- jogadores ]
    aveMap = M.fromList [ (n, ave) | (n,_,_,_,ave) <- jogadores ]
    getAve j = M.findWithDefault 0.0 j aveMap

    wins = M.fromListWith (+)
             [ (v,1) | (_, j1, j2, _, v) <- resultadosAntigos
                     , not (null v)
                     , v == j1 || v == j2 ]
    getWins j = M.findWithDefault 0 j wins

    opps = let step m (_,a,b,_,_) =
                  M.insertWith S.union a (S.singleton b)
                $ M.insertWith S.union b (S.singleton a) m
           in foldl step M.empty resultadosAntigos
    jogou a b = S.member b (M.findWithDefault S.empty a opps)

    ordenados = sortBy (comparing (\j -> (Down (getWins j), Down (getAve j), j))) nomes

    pairUp []       = []
    pairUp [x]      = [(x,"BYE")]  
    pairUp (x:y:xs)
      | not (jogou x y) = (x,y) : pairUp xs
      | otherwise =
          case findIndex (\z -> not (jogou x z)) xs of
            Nothing  -> (x,y) : pairUp xs
            Just k   -> let z   = xs !! k
                            xs' = take k xs ++ y : drop (k+1) xs
                        in (x,z) : pairUp xs'

    pares = pairUp ordenados

    proxRondaStr =
      let rs = [ read r :: Int | (r,_,_,_,_) <- resultadosAntigos, not (null r) ]
      in show (if null rs then 1 else maximum rs + 1)

    novaRonda =
      [ if b == "BYE"
          then (proxRondaStr, a, b, (1,0), a)
          else (proxRondaStr, a, b, (0,0), "")
      | (a,b) <- pares
      ]
  in resultadosAntigos ++ novaRonda



runElimParing
  :: [(Int, String)]
  -> [ ( String, String, String, String, String, String
       , String, String, String, String, String, String, String ) ]
runElimParing participantes =
  let
    seedsOrd = sortOn fst participantes
    equipas  = map snd seedsOrd
    n        = length equipas
    pot2s    = [2,4,8,16,32,64]
    _check   = if n `elem` pot2s
                 then ()
                 else error "Número de equipas deve ser potência de 2 (2,4,8,16,32,64) quando BYE está desativado."

    emparelharPrimeira xs =
      let k    = length xs `div` 2
          left = take k xs
          right= reverse (drop k xs)
      in zip left right

    nomeRonda m = case m of
      2  -> "Final"
      4  -> "Meias-Finais"
      8  -> "Quartos"
      16 -> "Oitavos"
      32 -> "R32"
      64 -> "R64"
      _  -> "R" ++ show m

    construirRondas xs
      | length xs <= 1 = []
      | otherwise =
          let m      = length xs
              rLabel = nomeRonda m
              pairs  = emparelharPrimeira xs
              jogos1 = zipWith (\idx (a,b) -> ("J" ++ show idx, a, b, Nothing)) [1..] pairs
              winners = [ "Vencedor " ++ rLabel ++ "-" ++ jId | (jId,_,_,_) <- jogos1 ]
          in (rLabel, jogos1) : construirRondas winners

    rondas = construirRondas equipas

    toResultado rLabel (jId, a, b, vOpt) =
      let pontosA = ""
          pontosB = ""
          data_   = ""
          hora    = ""
          local   = ""
          arbitro = ""
          estado  = maybe "Por jogar" (const "Por jogar") vOpt  
          vencedor= maybe "" id vOpt
          obs     = ""  
      in ( rLabel, jId, a, b, pontosA, pontosB
         , data_, hora, local, arbitro, estado, vencedor, obs )
  in
    [ toResultado r j
    | (r, js) <- rondas
    , j <- js
    ]
