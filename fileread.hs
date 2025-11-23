module Main where

import Text.Read (readMaybe)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.List (groupBy, sortOn)
import Data.Function (on)
import Pairing (runAVEparing, runElimParing)

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

lerInteiroSeguro :: String -> Int
lerInteiroSeguro s = fromMaybe 0 (readMaybe s :: Maybe Int)

lerDoubleSeguro :: String -> Double
lerDoubleSeguro s = fromMaybe 0.0 (readMaybe s :: Maybe Double)

pausa :: IO ()
pausa = do
  putStrLn "\n[Enter] para voltar ao menu..."
  _ <- getLine
  pure ()

type JogadorAVE           = (String, Char, Int, Int, Double)
type TorneioAVE           = [JogadorAVE]
type ResultadoAVE         = (String, String, String, (Int, Int), String)
type TorneioElim          = (Int, String)
type ResultadoTorneioElim =
  ( String, String, String, String, String, String
  , String, String, String, String, String, String, String )

-- -------- AVE --------
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

-- ---- Eliminação Direta ----
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
parseTorneioElim = mapMaybe parseTorneioElimLinhaMaybe

parseTorneioElimLinhaMaybe :: String -> Maybe TorneioElim
parseTorneioElimLinhaMaybe linha =
  case separarPorVirgula linha of
    (numeroStr:equipa:_) ->
      case (readMaybe numeroStr :: Maybe Int) of
        Just n | n > 0 && not (null (limparEspacos equipa)) -> Just (n, equipa)
        _ -> Nothing
    _ -> Nothing

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

-- -------- Menu --------
menu :: IO ()
menu = do
  putStrLn "\n== Menu =="
  putStrLn "1. Ler o torneio AVE"
  putStrLn "2. Ler os resultados do torneio AVE"
  putStrLn "3. Ler o Torneio Elim 16 clubes"
  putStrLn "4. Ler os resultados do torneio Elim 16 clubes"
  putStrLn "5. Gerar próxima ronda AVE e mostrar"
  putStrLn "6. Gerar emparelhamento eliminação direta e mostrar"
  putStrLn "7. Sair"
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
    "5" -> do
      jogadores <- lerTorneioAVE "torneio_ave_vila_real.csv"
      resAnt    <- lerResultadosTorneioAVE "resultados_torneio_ave_vila_real.csv"
      let novos = runAVEparing jogadores resAnt
      putStrLn "\n== Resultados com nova ronda AVE adicionada ==\n"
      printResultadosTorneioAVE novos
      pausa
      menu
    "6" -> do
      ts <- lerTorneioElim "torneio_16_clubes.csv"
      let bracket = runElimParing ts
      putStrLn "\n== Bracket de Eliminação Direta (todas as rondas) ==\n"
      printResultadosTorneioElim bracket
      pausa
      menu
    "7" -> putStrLn "\nAté à próxima :)"
    _ -> do
      putStrLn "\nOpção inválida. Tente novamente."
      pausa
      menu

main :: IO ()
main = menu
