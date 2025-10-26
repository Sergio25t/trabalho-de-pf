{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Use when" #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Redundant return" #-}
import Data.List.Split ( splitOn )
import System.IO ( readFile, Handle, TextEncoding, openFile, utf8, hGetContents, IOMode(ReadMode), hClose )
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Data.List (groupBy, intercalate)
import GHC.IO.Handle (hSetEncoding)

limparLinha :: String -> String
limparLinha = unwords . words  -- Remove espaços extras entre as palavras

separarCampos :: String -> [String]
separarCampos = separarCSV . limparLinha

safeReadInt :: String -> Int
safeReadInt s = fromMaybe 0 (readMaybe s :: Maybe Int)  -- Valor padrão em caso de erro

safeReadDouble :: String -> Double
safeReadDouble s = fromMaybe 0.0 (readMaybe s :: Maybe Double)  -- Valor padrão em caso de erro

separarCSV :: String -> [String]
separarCSV = splitOn ","
-----------------------------------------------------------------------------------------------------
type JogadorAVE = (String, Char, Int, Int, Double)  -- Nome, Gênero, Jogos Ganhos, Jogos Perdidos, AVE
type TorneioAVE = [JogadorAVE]  -- Lista de jogadores no torneio AVE

type ResultadoAVE = (String, String, String, (Int, Int), String) -- Ronda, Jogador 1, Jogador 2, Resultado ex(1-2), Vencedor

type Torneio = (Int, String) -- Numero, equipa

type ResultadoTorneio = (String, String, String, String, String, String, String, String, String, String, String, String, String) -- Ronda,Jogo, Equipa A, Equipa B, Pontuação A, Pontuação B, Data, Hora, Local, Arbitro/Oficial, Estado, Vencedor, Observações


readResultadoTorneio16 :: String -> IO [ResultadoTorneio]
readResultadoTorneio16 filename = do
    content <- readFile filename
    let linesOfFile = lines content
        validLines = filter (not . null) linesOfFile  -- Remove linhas vazias
    return (parseResultadosTorneio16 validLines)

parseResultadosTorneio16 :: [String] -> [ResultadoTorneio]
parseResultadosTorneio16 = map parseResultadoTorneio . drop 2  -- Ignora as 2 primeiras linhas

parseResultadoTorneio :: String -> ResultadoTorneio
parseResultadoTorneio line =
    let campos = separarCampos line
    in case campos of
        [ronda, jogo, equipaA, equipaB, pontosA, pontosB, date, hora, local, arbitro, estado, vencedor, observacoes] ->
            ( ronda
            , jogo
            , equipaA
            , equipaB
            , pontosA
            , pontosB
            , date
            , hora
            , local
            , arbitro
            , estado
            , vencedor
            , observacoes
            )
        _ -> error ("Formato inválido na linha: " ++ line)

printResultadosTorneio16 :: [ResultadoTorneio] -> IO ()
printResultadosTorneio16 resultados = do
    putStrLn "Torneio,Taça Nacional de Clubes - Eliminatórias Diretas 2025"
    putStrLn "====================================================="
    mapM_ (\(ronda, jornada, equipaA, equipaB, pontosA, pontosB, date, hora, local, arbitro, estado, vencedor, observacoes) -> do
        let linha = ronda ++" - " ++ jornada ++ "\n"
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

-- Leitura do ficheiro de eliminatórias
readTorneio :: String -> IO [Torneio]
readTorneio fileName = do
    content <- readFile fileName
    let linesOfFile = lines content
        (info1:info2:rest) = linesOfFile
    putStrLn ("\n" ++ info1)
    let validLines = filter (not . null) rest  -- Remove linhas vazias
    return (parseTorneio16 validLines)

parseTorneio16 :: [String] -> [Torneio]
parseTorneio16 = map parseTorneio . drop 1

parseTorneio :: String -> Torneio
parseTorneio line =
    let campos = separarCSV line
    in case campos of
        [numero, equipa] -> (safeReadInt numero, equipa)
        _ -> error ("Formato inválido na linha: " ++ line)

printTorneio :: [Torneio] -> IO ()
printTorneio torneios = do
    mapM_ (\(numero, equipa) -> do
        let linha = " "++ show numero ++ ". Equipa: " ++ equipa
        putStrLn linha
        ) torneios


-- Leitura de resultados do torneio AVE
readResultadosTorneioAVE :: String -> IO [ResultadoAVE]
readResultadosTorneioAVE fileName = do
    content <- readFile fileName
    let validLines = filter (not . null) (lines content)  -- Remove linhas vazias
    return (parseResultadosAVE validLines)

parseResultadosAVE :: [String] -> [ResultadoAVE]
parseResultadosAVE = map parseResultadoAVE . drop 2  -- Ignora as 2 primeiras linhas

parseResultado :: String -> (Int, Int)
parseResultado s =
    -- Substitui caracteres errados (como ÔÇô) por hífen e outros caracteres estranhos por nada
    let sCleaned = map (\c -> if c == 'Ô' then '-' else c) s  -- Substitui Ô por -
        sValid = filter (elem ['0'..'9'] ++ ['-']) sCleaned  -- Remove caracteres não válidos
    in case splitOn "-" sValid of
        [a, b] -> (safeReadInt a, safeReadInt b)  -- Converte para inteiros
        _      -> error ("Formato inválido de resultado: " ++ s)

parseResultadoAVE :: String -> ResultadoAVE
parseResultadoAVE line =
    let campos = separarCSV line
    in case campos of
        [ronda, jogador1, jogador2, resultado, vencedor] ->
            ( ronda
            , jogador1
            , jogador2
            , parseResultado resultado
            , vencedor
            )
        _ -> error ("Formato inválido na linha: " ++ line)

printResultadosAVE :: [ResultadoAVE] -> IO ()
printResultadosAVE resultados = do
    putStrLn "Torneio: Torneio AVE - Open de Vila Real"
    putStrLn "Resultados Individuais - 3 Rondas\n"

    -- Agrupar resultados por ronda
    let resultadosAgrupados = groupBy (\(ronda1, _, _, _, _) (ronda2, _, _, _, _) -> ronda1 == ronda2) resultados

    -- Imprimir os resultados por ronda
    mapM_ imprimirRonda resultadosAgrupados

-- Função para imprimir cada ronda com seus jogos
imprimirRonda :: [(String, String, String, (Int, Int), String)] -> IO ()
imprimirRonda [] = return ()
imprimirRonda ((ronda, jogador1, jogador2, (g1, g2), vencedor):rest) = do
    -- Imprimir a informação da ronda
    putStrLn $ "--- Ronda " ++ ronda ++ " ---"
    -- Imprimir todos os jogos dessa ronda
    let linha = jogador1 ++ " vs " ++ jogador2 ++ " -> " ++ show g1 ++ "-" ++ show g2
                 ++ " (Vencedor: " ++ vencedor ++ ")"
    putStrLn linha
    -- Recursivamente imprimir os jogos restantes da mesma ronda
    mapM_ (\(_, j1, j2, (g1', g2'), v) -> putStrLn $ j1 ++ " vs " ++ j2 ++ " -> " ++ show g1' ++ "-" ++ show g2' ++ " (Vencedor: " ++ v ++ ")") rest
    putStrLn "-----------------------------------------------------"


-- Leitura do torneio AVE
readTorneioAVE :: String -> IO TorneioAVE
readTorneioAVE fileName = do
    content <- readFile fileName
    let linesOfFile = lines content
        -- Ler as duas primeiras linhas
        (info1:info2:rest) = linesOfFile
    -- Imprimir as informações no terminal
    putStrLn ("\n" ++ info1)
    putStrLn (info2 ++ "\n")
    -- Processar as linhas restantes dos jogadores
    let validLines = filter (not . null) rest  -- Remove linhas vazias
    return (parseJogadoresAVE validLines)

-- Função para processar as linhas e separar os dados dos jogadores
parseJogadoresAVE :: [String] -> TorneioAVE
parseJogadoresAVE = map parseJogadorAVE . drop 1

-- Função para parsear os dados de cada jogador
parseJogadorAVE :: String -> JogadorAVE
parseJogadorAVE line =
    let campos = separarCSV line
    in case campos of
        [nome, genero, vitoria, derrota, ave] ->
            ( nome
            , head genero  -- Pegando o primeiro carácter do género
            , safeReadInt vitoria  -- Converte vitórias com verificação
            , safeReadInt derrota  -- Converte derrotas com verificação
            , safeReadDouble ave   -- Converte AVE com verificação
            )
        _ -> error ("Formato inválido na linha: " ++ line)

-- Função de impressão do torneio, que agora vai imprimir todas as informações dos jogadores
printTorneioAVE :: TorneioAVE -> IO ()
printTorneioAVE torneio = do
    putStrLn "Jogadores (Nome, Gênero, Vitórias, Derrotas, AVE):\n"
    mapM_ (\(nome, genero, vitoria, derrota, ave) -> do
        let linha = "Nome: " ++ nome
                 ++ " (" ++ [genero] ++ ") -"
                 ++ " Vitórias: " ++ show vitoria
                 ++ "; Derrotas: " ++ show derrota
                 ++ "; AVE: " ++ show ave
        putStrLn linha
        ) torneio


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
        "1" -> do
            torneio <- readTorneioAVE "torneio_ave_vila_real.csv"
            printTorneioAVE torneio
            putStrLn "\n"
            menu
        "2" -> do
            resultados <- readResultadosTorneioAVE "resultados_torneio_ave_vila_real.csv"
            printResultadosAVE resultados
            putStrLn "\n"
            menu
        "3" -> do
            torneioEliminados <- readTorneio "torneio_16_clubes.csv"
            printTorneio torneioEliminados
            putStrLn "\n"
            menu
        "4" -> do
            resultados <- readResultadoTorneio16 "resultados_torneio_16_clubes.csv"
            printResultadosTorneio16 resultados
            putStrLn "\n"
            menu
        "5" -> do
            putStrLn "\nAté a próxima :)!"
        _   -> do
                putStrLn "\nOpção inválida. Tente novamente."
                menu
main :: IO ()
main = menu