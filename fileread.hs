{-# OPTIONS_GHC -Wall #-}

import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Data.List (groupBy, sortBy, findIndex, sortOn)
import Data.Function (on)
import Data.Ord  (comparing, Down(..))
import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- ====================== Utils (nomes em PT) ======================

dividirPor :: Char -> String -> [String]
dividirPor _ [] = [""]
dividirPor d (c:cs)
  | c == d    = "" : resto
  | otherwise = (c : head resto) : tail resto
  where
    resto = dividirPor d cs

limparEspacos :: String -> String
limparEspacos = unwords . words

separarPorVirgula :: String -> [String]
separarPorVirgula = dividirPor ','

separarCampos :: String -> [String]
separarCampos = separarPorVirgula . limparEspacos

lerInteiroSeguro :: String -> Int
lerInteiroSeguro s = fromMaybe 0 (readMaybe s :: Maybe Int)

lerDoubleSeguro :: String -> Double
lerDoubleSeguro s = fromMaybe 0.0 (readMaybe s :: Maybe Double)

pausa :: IO ()
pausa = do
  putStrLn "\n[Enter] para voltar ao menu..."
  _ <- getLine
  pure ()

-- ====================== Tipos (nomes em PT) ======================

type JogadorAVE           = (String, Char, Int, Int, Double)
type TorneioAVE           = [JogadorAVE]
type ResultadoAVE         = (String, String, String, (Int, Int), String)
type TorneioElim          = (Int, String)  -- (seed, equipa)
type ResultadoTorneioElim =
  ( String, String, String, String, String, String
  , String, String, String, String, String, String, String )

-- ====================== AVE: IO leitura/printing ======================

lerTorneioAVE :: String -> IO TorneioAVE
lerTorneioAVE nomeFicheiro = do
  conteudo <- readFile nomeFicheiro
  let ls = lines conteudo
  case ls of
    (cab1:cab2:resto) -> do
      putStrLn ("\n" ++ cab1)
      putStrLn (cab2 ++ "\n")
      let linhasValidas = filter (not . null) resto
      pure (parseJogadoresAVE linhasValidas)
    _ -> error "Ficheiro AVE com formato inválido."

lerResultadosTorneioAVE :: String -> IO [ResultadoAVE]
lerResultadosTorneioAVE nomeFicheiro = do
  conteudo <- readFile nomeFicheiro
  let linhasValidas = filter (not . null) (lines conteudo)
  pure (parseResultadosAVE linhasValidas)

parseJogadoresAVE :: [String] -> TorneioAVE
parseJogadoresAVE = map parseJogadorAVE . drop 1

parseJogadorAVE :: String -> JogadorAVE
parseJogadorAVE linha =
  case separarPorVirgula linha of
    [nome, genero, vitoria, derrota, ave] ->
      ( nome
      , case genero of { (c:_)-> c; [] -> '?' }
      , lerInteiroSeguro vitoria
      , lerInteiroSeguro derrota
      , lerDoubleSeguro ave
      )
    _ -> error ("Formato inválido na linha: " ++ linha)

parseResultadosAVE :: [String] -> [ResultadoAVE]
parseResultadosAVE = map parseResultadoAVELinha . drop 2

parseResultado :: String -> (Int, Int)
parseResultado s =
  let sCleaned = map (\c -> if c == 'Ô' then '-' else c) s
      sValid   = filter (\c -> c == '-' || (c >= '0' && c <= '9')) sCleaned
  in case dividirPor '-' sValid of
       [a, b] -> (lerInteiroSeguro a, lerInteiroSeguro b)
       _      -> error ("Formato inválido de resultado: " ++ s)

parseResultadoAVELinha :: String -> ResultadoAVE
parseResultadoAVELinha linha =
  case separarPorVirgula linha of
    [ronda, jogador1, jogador2, resultado, vencedor] ->
      (ronda, jogador1, jogador2, parseResultado resultado, vencedor)
    _ -> error ("Formato inválido na linha: " ++ linha)

printTorneioAVE :: TorneioAVE -> IO ()
printTorneioAVE torneio = do
  putStrLn "Jogadores (Nome, Género, Vitórias, Derrotas, AVE):\n"
  mapM_ (\(nome, genero, vitoria, derrota, ave) ->
           putStrLn $ "Nome: " ++ nome
                   ++ " (" ++ [genero] ++ ") - Vitórias: " ++ show vitoria
                   ++ "; Derrotas: " ++ show derrota
                   ++ "; AVE: " ++ show ave
        ) torneio

printResultadosTorneioAVE :: [ResultadoAVE] -> IO ()
printResultadosTorneioAVE resultados = do
  putStrLn "Torneio: Torneio AVE - Open de Vila Real"
  putStrLn "Resultados Individuais\n"
  let ordenados = sortOn (\(r,_,_,_,_) -> lerInteiroSeguro r) resultados
      grupos    = groupBy ((==) `on` (\(r,_,_,_,_) -> r)) ordenados
  mapM_ imprimirRondaAVE grupos

imprimirRondaAVE :: [(String, String, String, (Int, Int), String)] -> IO ()
imprimirRondaAVE [] = pure ()
imprimirRondaAVE jogos@((ronda, _, _, _, _):_) = do
  putStrLn $ "--- Ronda " ++ ronda ++ " ---"
  mapM_ (\(_, j1, j2, (g1, g2), v) ->
           putStrLn $ j1 ++ " vs " ++ j2 ++ " -> "
                    ++ show g1 ++ "-" ++ show g2 ++ " (Vencedor: " ++ v ++ ")"
        ) jogos
  putStrLn "--------------------------------------------------------------"

-- ====================== AVE: emparelhamento (T2.1) ======================

-- Mantém nome pedido no enunciado:
-- runAVEparing :: estrutura_de_dados_torneio_ave -> estrutura_de_dados_resultados_ave -> estrutura_de_dados_resultados_ave
runAVEparing :: TorneioAVE -> [ResultadoAVE] -> [ResultadoAVE]
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

    pairUp :: [String] -> [(String,String)]
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
      let rs = [ lerInteiroSeguro r | (r,_,_,_,_) <- resultadosAntigos ]
      in show (if null rs then 1 else maximum rs + 1)

    novaRonda =
      [ if b == "BYE"
          then (proxRondaStr, a, b, (1,0), a)
          else (proxRondaStr, a, b, (0,0), "")
      | (a,b) <- pares
      ]
  in resultadosAntigos ++ novaRonda

-- ====================== Eliminação direta: IO e printing ======================

lerTorneioElim :: String -> IO [TorneioElim]
lerTorneioElim nomeFicheiro = do
  conteudo <- readFile nomeFicheiro
  let ls = lines conteudo
  case ls of
    (cab1:cab2:resto) -> do
      putStrLn ("\n" ++ cab1)
      putStrLn (cab2 ++ "\n")
      let linhasValidas = filter (not . null) resto
      pure (parseTorneioElim linhasValidas)
    _ -> error "Ficheiro Torneio Elim com formato inválido."

lerResultadosTorneioElim :: String -> IO [ResultadoTorneioElim]
lerResultadosTorneioElim nomeFicheiro = do
  conteudo <- readFile nomeFicheiro
  let ls = lines conteudo
      linhasValidas = filter (not . null) ls
  pure (parseResultadosTorneioElim linhasValidas)

parseTorneioElim :: [String] -> [TorneioElim]
parseTorneioElim = map parseTorneioElimLinha

parseTorneioElimLinha :: String -> TorneioElim
parseTorneioElimLinha linha =
  case separarPorVirgula linha of
    [numero, equipa] -> (lerInteiroSeguro numero, equipa)
    _ -> error ("Formato inválido na linha: " ++ linha)

parseResultadosTorneioElim :: [String] -> [ResultadoTorneioElim]
parseResultadosTorneioElim = map parseResultadoTorneioElimLinha . drop 2

parseResultadoTorneioElimLinha :: String -> ResultadoTorneioElim
parseResultadoTorneioElimLinha linha =
  case separarPorVirgula linha of
    [ronda, jogo, equipaA, equipaB, pontosA, pontosB, data_, hora, local, arbitro, estado, vencedor, observacoes] ->
      (ronda, jogo, equipaA, equipaB, pontosA, pontosB, data_, hora, local, arbitro, estado, vencedor, observacoes)
    _ -> error ("Formato inválido na linha: " ++ linha)

printTorneioElim :: String -> [TorneioElim] -> IO ()
printTorneioElim nome torneios = do
  putStrLn nome
  putStrLn "Equipas:\n"
  mapM_ (\(numero, equipa) -> putStrLn $ "Seed " ++ show numero ++ " - " ++ equipa) torneios

printResultadosTorneioElim :: [ResultadoTorneioElim] -> IO ()
printResultadosTorneioElim resultados = do
  putStrLn "Resultados Torneio Elim\n"
  mapM_ (\(ronda, jogo, equipaA, equipaB, pontosA, pontosB, data_, hora, local, arbitro, estado, vencedor, observacoes) -> do
           let linha =  ronda ++ " - " ++ jogo ++ "\n"
                     ++ equipaA ++ " vs " ++ equipaB ++ "\n"
                     ++ "Pontuação A: " ++ pontosA ++ "   Pontuação B: " ++ pontosB ++ "\n"
                     ++ "Data: " ++ data_ ++ "   Hora: " ++ hora ++ "   Local: " ++ local ++ "\n"
                     ++ "Árbitro/Oficial: " ++ arbitro ++ "\n"
                     ++ "Estado: " ++ estado ++ "\n"
                     ++ "Vencedor: " ++ vencedor ++ "\n"
                     ++ "Observações: " ++ observacoes
           putStrLn linha
           putStrLn "--------------------------------------------------------------"
        ) resultados

-- ====================== Eliminação direta: emparelhamento (T2.2) ======================

-- Mantém nome pedido no enunciado:
-- runElimParing :: estrutura_de_dados_torneio_elim -> estrutura_de_dados_resultados_elim
runElimParing :: [TorneioElim] -> [ResultadoTorneioElim]
runElimParing participantes =
  let
    seedsOrd :: [(Int, String)]
    seedsOrd = sortOn fst participantes

    equipas0 :: [String]
    equipas0 = map snd seedsOrd

    proxPot2 :: Int -> Int
    proxPot2 n = head $ dropWhile (< n) (iterate (*2) 1)

    n0  = length equipas0
    nP  = proxPot2 n0
    byes   = replicate (nP - n0) "BYE"
    equipasP = equipas0 ++ byes

    emparelharPrimeira :: [String] -> [(String, String)]
    emparelharPrimeira xs =
      let k    = length xs `div` 2
          left = take k xs
          right= reverse (drop k xs)
      in zip left right

    nomeRonda :: Int -> String
    nomeRonda m = case m of
      2  -> "Final"
      4  -> "Meias-Finais"
      8  -> "Quartos"
      16 -> "Oitavos"
      32 -> "R32"
      64 -> "R64"
      _  -> "R" ++ show m

    construirRondas :: [String] -> [(String, [(String, String, String, Maybe String)])]
    construirRondas xs
      | length xs <= 1 = []
      | otherwise =
          let m      = length xs
              rLabel = nomeRonda m
              pairs  = emparelharPrimeira xs
              jogos1 = zipWith
                         (\idx (a,b) ->
                            let vOpt =
                                  if a == "BYE" && b == "BYE" then Just "BYE"
                                  else if a == "BYE" then Just b
                                  else if b == "BYE" then Just a
                                  else Nothing
                                jId = "J" ++ show idx
                            in (jId, a, b, vOpt))
                         [1..] pairs
              winners :: [String]
              winners = [ maybe ("Vencedor " ++ rLabel ++ "-" ++ jId) id v
                        | (jId, _, _, v) <- jogos1 ]
          in (rLabel, jogos1) : construirRondas winners

    rondas = construirRondas equipasP

    toResultado :: String -> (String, String, String, Maybe String) -> ResultadoTorneioElim
    toResultado rLabel (jId, a, b, vOpt) =
      let pontosA = ""
          pontosB = ""
          data_   = ""
          hora    = ""
          local   = ""
          arbitro = ""
          estado  = maybe "Por jogar" (const "Avançou por BYE / definido") vOpt
          vencedor= maybe "" id vOpt
          obs     = if a == "BYE" || b == "BYE" then "Emparelhamento com BYE" else ""
      in ( rLabel, jId, a, b, pontosA, pontosB
         , data_, hora, local, arbitro, estado, vencedor, obs )
  in
    [ toResultado r j
    | (r, js) <- rondas
    , j <- js
    ]

-- ====================== Menu ======================

menu :: IO ()
menu = do
  putStrLn "\n== Menu =="
  putStrLn "1. Ler o torneio AVE"
  putStrLn "2. Ler os resultados do torneio AVE"
  putStrLn "3. Ler o Torneio Elim 16 clubes"
  putStrLn "4. Ler os resultados do torneio Elim 16 clubes"
  putStrLn "5. Sair"
  putStrLn "6. Gerar próxima ronda AVE e mostrar"
  putStrLn "7. Gerar emparelhamento eliminação direta e mostrar"
  putStr   "> Opção: "
  opcao <- getLine
  case opcao of
    "1" -> do
      lerTorneioAVE "torneio_ave_vila_real.csv" >>= printTorneioAVE
      pausa
      menu
    "2" -> do
      lerResultadosTorneioAVE "resultados_torneio_ave_vila_real.csv" >>= printResultadosTorneioAVE
      pausa
      menu
    "3" -> do
      lerTorneioElim "torneio_16_clubes.csv" >>= printTorneioElim "Torneio 16 Clubes"
      pausa
      menu
    "4" -> do
      lerResultadosTorneioElim "resultados_torneio_16_clubes.csv" >>= printResultadosTorneioElim
      pausa
      menu
    "5" -> putStrLn "\nAté à próxima :)"
    "6" -> do
      jogadores <- lerTorneioAVE "torneio_ave_vila_real.csv"
      resAnt    <- lerResultadosTorneioAVE "resultados_torneio_ave_vila_real.csv"
      let novos = runAVEparing jogadores resAnt
      putStrLn "\n== Resultados com nova ronda AVE adicionada ==\n"
      printResultadosTorneioAVE novos
      pausa
      menu
    "7" -> do
      ts <- lerTorneioElim "torneio_16_clubes.csv"
      let bracket = runElimParing ts
      putStrLn "\n== Bracket de Eliminação Direta (todas as rondas) ==\n"
      printResultadosTorneioElim bracket
      pausa
      menu
    _ -> do
      putStrLn "\nOpção inválida. Tente novamente."
      pausa
      menu

main :: IO ()
main = menu
