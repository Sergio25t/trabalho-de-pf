module FileRead (
    readTorneioElim,
    readResultadosTorneioElim,
    readTorneioAVE,
    readResultadosTorneioAVE,
    printTorneioElim,
    printResultadosTorneioElim,
    printTorneioAVE,
    printResultadosTorneioAVE
) where

import System.IO

-- Função para substituir caracteres especiais por hífen
replaceSpecialChars :: String -> String
replaceSpecialChars = map (\c -> if c == '\8211' then '-' else c)

-- Tipos de Dados

-- Para o torneio Eliminatórias
data TorneioElim = TorneioElim String [String]  -- Nome do Torneio e Listagem de Equipas

-- Para os resultados Eliminatórias
data ResultadoElim = ResultadoElim String String String String String String String String String String  -- Jogo, Equipa A, Equipa B, Pontuação A, Pontuação B, Data, Hora, Local, Árbitro/Oficial, Vencedor

-- Para o torneio AVE
data JogadorAVE = JogadorAVE String String Int Int Double  -- Nome, Género, Vitórias, Derrotas, AVE
data TorneioAVE = TorneioAVE String Int [JogadorAVE]  -- Nome, Rondas, Jogadores

-- Para os resultados AVE
data ResultadoAVE = ResultadoAVE String String Int Int String  -- Jogo, Vencedor, Pontuação A, Pontuação B, Data

-- Funções de Leitura (Leitura de arquivos simples)

readTorneioElim :: String -> IO (Either String TorneioElim)
readTorneioElim filename = do
    contents <- readFile filename
    let linhas = lines contents
        nome = replaceSpecialChars (head linhas)  -- Substitui caracteres especiais no nome do torneio
        equipas = map (takeWhile (/= ',') . drop 1) (tail linhas)
    return $ Right (TorneioElim nome equipas)

readResultadosTorneioElim :: String -> IO (Either String [ResultadoElim])
readResultadosTorneioElim filename = do
    contents <- readFile filename
    let linhas = lines contents
        resultados = map parseResultado (filter (\line -> "J" `elem` words line) (tail linhas))  -- Filtra apenas as linhas que contêm "J" (Jogos)
    return $ Right resultados
  where
    parseResultado line =
        let campos = wordsBy (==',') line
        in if length campos >= 10
           then ResultadoElim (replaceSpecialChars (campos !! 0)) (replaceSpecialChars (campos !! 1)) (replaceSpecialChars (campos !! 2)) (campos !! 3) (campos !! 4) (campos !! 5) (campos !! 6) (campos !! 7) (campos !! 8) (campos !! 9)
           else ResultadoElim "Erro" "Erro" "Erro" "Erro" "Erro" "Erro" "Erro" "Erro" "Erro"

-- Função auxiliar para dividir strings por um delimitador
wordsBy :: (Char -> Bool) -> String -> [String]
wordsBy p s =  case dropWhile p s of
                 "" -> []
                 s' -> w : wordsBy p s''
                       where (w, s'') = break p s'

-- Funções de Leitura para o torneio AVE

readTorneioAVE :: String -> IO (Either String TorneioAVE)
readTorneioAVE filename = do
    contents <- readFile filename
    let linhas = lines contents
        nome = replaceSpecialChars (head linhas)  -- Substitui caracteres especiais no nome do torneio
        rondas = 3  -- Número fixo de rondas para este exemplo
        jogadores = parseJogadores (drop 2 linhas)
    return $ Right (TorneioAVE nome rondas jogadores)
  where
    parseJogadores lines = map parseJogador lines
    parseJogador line = 
        let [nome, gen, vit, der, ave] = words line
        in JogadorAVE nome gen (read vit) (read der) (read ave)

readResultadosTorneioAVE :: String -> IO (Either String [ResultadoAVE])
readResultadosTorneioAVE filename = do
    contents <- readFile filename
    let linhas = lines contents
        resultados = map parseResultado (tail linhas)
    return $ Right resultados
  where
    parseResultado line = 
        let [jogo, vencedor, pA, pB, dataJogo] = words line
        in ResultadoAVE jogo vencedor (read pA) (read pB) dataJogo

-- Funções de Escrita (Impressão no Ecrã)

printTorneioElim :: String -> TorneioElim -> IO ()
printTorneioElim nomeTorneio (TorneioElim _ equipas) = do
    putStrLn $ nomeTorneio ++ " – Eliminatórias Diretas 2025"
    putStrLn "Participantes:"
    mapM_ putStrLn equipas

printResultadosTorneioElim :: [ResultadoElim] -> IO ()
printResultadosTorneioElim resultados = do
    putStrLn "--- Resultados Eliminatórias ---"
    mapM_ printResultado resultados
  where
    printResultado (ResultadoElim jogo equipaA equipaB pA pB dataJogo hora local arbitro vencedor) = do
        putStrLn $ jogo ++ ": " ++ equipaA ++ " vs " ++ equipaB
        putStrLn $ "Pontuação: " ++ pA ++ "-" ++ pB
        putStrLn $ "Data: " ++ dataJogo ++ ", Hora: " ++ hora
        putStrLn $ "Local: " ++ local ++ ", Árbitro: " ++ arbitro
        putStrLn $ "Vencedor: " ++ vencedor

printTorneioAVE :: TorneioAVE -> IO ()
printTorneioAVE (TorneioAVE nome rondas jogadores) = do
    putStrLn $ "Torneio: " ++ nome
    putStrLn $ "Número de rondas: " ++ show rondas
    putStrLn "Jogadores (Nome, Género, Vitórias, Derrotas, AVE):"
    mapM_ printJogador jogadores
  where
    printJogador (JogadorAVE nome gen vit der ave) = do
        putStrLn $ nome ++ " (" ++ gen ++ ") — Vitórias: " ++ show vit ++ "; Derrotas: " ++ show der ++ "; AVE: " ++ show ave

printResultadosTorneioAVE :: [ResultadoAVE] -> IO ()
printResultadosTorneioAVE resultados = do
    putStrLn "--- Resultados ---"
    mapM_ printResultado resultados
  where
    printResultado (ResultadoAVE jogo vencedor pA pB dataJogo) = do
        putStrLn $ jogo ++ " → " ++ vencedor ++ " (" ++ show pA ++ "-" ++ show pB ++ ")"
        putStrLn $ "Data: " ++ dataJogo

-- Função main para testar

main :: IO ()
main = do
    -- Exemplo de uso das funções de leitura
    torneioElim <- readTorneioElim "torneio_16_clubes.txt"
    resultadosElim <- readResultadosTorneioElim "resultados_torneio_16_clubes.csv"
    torneioAVE <- readTorneioAVE "torneio_ave_vila_real.txt"
    resultadosAVE <- readResultadosTorneioAVE "resultados_torneio_ave_vila_real.txt"
    
    -- Exemplo de uso das funções de escrita
    case torneioElim of
        Left err -> putStrLn $ "Erro ao ler o torneio de Eliminatórias: " ++ err
        Right torneio -> printTorneioElim "Taça Nacional de Clubes" torneio
    
    case resultadosElim of
        Left err -> putStrLn $ "Erro ao ler os resultados Eliminatórias: " ++ err
        Right resultados -> printResultadosTorneioElim resultados
    
    case torneioAVE of
        Left err -> putStrLn $ "Erro ao ler o torneio AVE: " ++ err
        Right torneio -> printTorneioAVE torneio
    
    case resultadosAVE of
        Left err -> putStrLn $ "Erro ao ler os resultados AVE: " ++ err
        Right resultados -> printResultadosTorneioAVE resultados











