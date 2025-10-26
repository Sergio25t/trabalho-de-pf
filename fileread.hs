{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Use when" #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Redundant return" #-}

import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Data.List (groupBy)

-- util: partir string por um delimitador simples (sem aspas)
splitBy :: Char -> String -> [String]
splitBy _ [] = [""]
splitBy d (c:cs)
  | c == d    = "" : rest
  | otherwise = (c : head rest) : tail rest
  where
    rest = splitBy d cs

limparLinha :: String -> String
limparLinha = unwords . words  -- Remove espaços extras

separarCSV :: String -> [String]
separarCSV = splitBy ','

separarCampos :: String -> [String]
separarCampos = separarCSV . limparLinha

safeReadInt :: String -> Int
safeReadInt s = fromMaybe 0 (readMaybe s :: Maybe Int)

safeReadDouble :: String -> Double
safeReadDouble s = fromMaybe 0.0 (readMaybe s :: Maybe Double)

-----------------------------------------------------------------------------------------------------
type JogadorAVE     = (String, Char, Int, Int, Double)  -- Nome, Género, Vitórias, Derrotas, AVE
type TorneioAVE     = [JogadorAVE]
type ResultadoAVE   = (String, String, String, (Int, Int), String)
type Torneio        = (Int, String) -- Número, equipa
type ResultadoTorneio =
  ( String, String, String, String, String, String
  , String, String, String, String, String, String, String )

--------------------------------- 16 Clubes ---------------------------------

readResultadoTorneio16 :: String -> IO [ResultadoTorneio]
readResultadoTorneio16 filename = do
  content <- readFile filename
  let linesOfFile = lines content
      validLines  = filter (not . null) linesOfFile
  pure (parseResultadosTorneio16 validLines)

parseResultadosTorneio16 :: [String] -> [ResultadoTorneio]
parseResultadosTorneio16 = map parseResultadoTorneio . drop 2  -- ignora 2 linhas

parseResultadoTorneio :: String -> ResultadoTorneio
parseResultadoTorneio line =
  case separarCampos line of
    [ronda, jogo, equipaA, equipaB, pontosA, pontosB, date, hora, local, arbitro, estado, vencedor, observacoes] ->
      (ronda, jogo, equipaA, equipaB, pontosA, pontosB, date, hora, local, arbitro, estado, vencedor, observacoes)
    _ -> error ("Formato inválido na linha: " ++ line)

printResultadosTorneio16 :: [ResultadoTorneio] -> IO ()
printResultadosTorneio16 resultados = do
  putStrLn "Torneio,Taça Nacional de Clubes - Eliminatórias Diretas 2025"
  putStrLn "====================================================="
  mapM_ (\(ronda, jogo, equipaA, equipaB, pontosA, pontosB, date, hora, local, arbitro, estado, vencedor, observacoes) -> do
           let linha = ronda ++ " - " ++ jogo ++ "\n"
                    ++ equipaA ++ " vs " ++ equipaB ++ "\n"
                    ++ "Pontuação A: " ++ pontosA ++ "   Pontuação B: " ++ pontosB ++ "\n"
                    ++ "Data: " ++ date ++ "   Hora: " ++ hora ++ "   Local: " ++ local ++ "\n"
                    ++ "Árbitro/Oficial: " ++ arbitro ++ "\n"
                    ++ "Estado: " ++ estado ++ "\n"
                    ++ "Vencedor: " ++ vencedor ++ "\n"
                    ++ "Observações: " ++ observacoes
           putStrLn linha
           putStrLn "-----------------------------------------------------"
        ) resultados

readTorneio :: String -> IO [Torneio]
readTorneio fileName = do
  content <- readFile fileName
  let ls = lines content
  case ls of
    (info1:info2:rest) -> do
      putStrLn ("\n" ++ info1)
      let validLines = filter (not . null) rest
      pure (parseTorneio16 validLines)
    _ -> error "Ficheiro de torneio (16 clubes) com menos de 2 linhas de header."

parseTorneio16 :: [String] -> [Torneio]
parseTorneio16 = map parseTorneio . drop 1

parseTorneio :: String -> Torneio
parseTorneio line =
  case separarCSV line of
    [numero, equipa] -> (safeReadInt numero, equipa)
    _ -> error ("Formato inválido na linha: " ++ line)

printTorneio :: [Torneio] -> IO ()
printTorneio ts = mapM_ (\(n, eq) -> putStrLn $ " " ++ show n ++ ". Equipa: " ++ eq) ts

--------------------------------- AVE ---------------------------------

readResultadosTorneioAVE :: String -> IO [ResultadoAVE]
readResultadosTorneioAVE fileName = do
  content <- readFile fileName
  let validLines = filter (not . null) (lines content)
  pure (parseResultadosAVE validLines)

parseResultadosAVE :: [String] -> [ResultadoAVE]
parseResultadosAVE = map parseResultadoAVE . drop 2

-- Corrigida: filtragem de caracteres
parseResultado :: String -> (Int, Int)
parseResultado s =
  let sCleaned = map (\c -> if c == 'Ô' then '-' else c) s
      sValid   = filter (\c -> c `elem` ('-':['0'..'9'])) sCleaned
  in case splitBy '-' sValid of
       [a, b] -> (safeReadInt a, safeReadInt b)
       _      -> error ("Formato inválido de resultado: " ++ s)

parseResultadoAVE :: String -> ResultadoAVE
parseResultadoAVE line =
  case separarCSV line of
    [ronda, jogador1, jogador2, resultado, vencedor] ->
      (ronda, jogador1, jogador2, parseResultado resultado, vencedor)
    _ -> error ("Formato inválido na linha: " ++ line)

printResultadosAVE :: [ResultadoAVE] -> IO ()
printResultadosAVE resultados = do
  putStrLn "Torneio: Torneio AVE - Open de Vila Real"
  putStrLn "Resultados Individuais - 3 Rondas\n"
  let grupos = groupBy (\(r1,_,_,_,_) (r2,_,_,_,_) -> r1 == r2) resultados
  mapM_ imprimirRonda grupos

imprimirRonda :: [(String, String, String, (Int, Int), String)] -> IO ()
imprimirRonda [] = pure ()
imprimirRonda jogos@((ronda, _, _, _, _):_) = do
  putStrLn $ "--- Ronda " ++ ronda ++ " ---"
  mapM_ (\(_, j1, j2, (g1, g2), v) ->
           putStrLn $ j1 ++ " vs " ++ j2 ++ " -> " ++ show g1 ++ "-" ++ show g2 ++ " (Vencedor: " ++ v ++ ")"
        ) jogos
  putStrLn "-----------------------------------------------------"

readTorneioAVE :: String -> IO TorneioAVE
readTorneioAVE fileName = do
  content <- readFile fileName
  let ls = lines content
  case ls of
    (info1:info2:rest) -> do
      putStrLn ("\n" ++ info1)
      putStrLn (info2 ++ "\n")
      let validLines = filter (not . null) rest
      pure (parseJogadoresAVE validLines)
    _ -> error "Ficheiro AVE com menos de 2 linhas de header."

parseJogadoresAVE :: [String] -> TorneioAVE
parseJogadoresAVE = map parseJogadorAVE . drop 1

parseJogadorAVE :: String -> JogadorAVE
parseJogadorAVE line =
  case separarCSV line of
    [nome, genero, vitoria, derrota, ave] ->
      ( nome
      , case genero of { (c:_)-> c; [] -> '?' }
      , safeReadInt vitoria
      , safeReadInt derrota
      , safeReadDouble ave
      )
    _ -> error ("Formato inválido na linha: " ++ line)

printTorneioAVE :: TorneioAVE -> IO ()
printTorneioAVE torneio = do
  putStrLn "Jogadores (Nome, Género, Vitórias, Derrotas, AVE):\n"
  mapM_ (\(nome, genero, vitoria, derrota, ave) ->
           putStrLn $ "Nome: " ++ nome
                   ++ " (" ++ [genero] ++ ") -"
                   ++ " Vitórias: " ++ show vitoria
                   ++ "; Derrotas: " ++ show derrota
                   ++ "; AVE: " ++ show ave
        ) torneio

--------------------------------- Menu ---------------------------------

menu :: IO ()
menu = do
  putStrLn "\n==Menu=="
  putStrLn "1. Ler torneio AVE\n"
  putStrLn "2. Ler resultados do torneio AVE\n"
  putStrLn "3. Ler Torneio 16 clubes\n"
  putStrLn "4. Ler resultados do torneio de 16 clubes\n"
  putStrLn "5. Sair\n"
  opcao <- getLine
  case opcao of
    "1" -> readTorneioAVE "torneio_ave_vila_real.csv"        >>= printTorneioAVE >> menu
    "2" -> readResultadosTorneioAVE "resultados_torneio_ave_vila_real.csv" >>= printResultadosAVE >> menu
    "3" -> readTorneio "torneio_16_clubes.csv"               >>= printTorneio   >> menu
    "4" -> readResultadoTorneio16 "resultados_torneio_16_clubes.csv" >>= printResultadosTorneio16 >> menu
    "5" -> putStrLn "\nAté a próxima :)!"
    _   -> putStrLn "\nOpção inválida. Tente novamente." >> menu

main :: IO ()
main = menu
