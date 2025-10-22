{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics
import System.IO

-- Estruturas de Dados

-- Estrutura para os dados do Torneio AVE
data JogadorAVE = JogadorAVE
  { nomeJogadorAVE :: String
  , generoAVE :: String
  , vitoriasAVE :: Int
  , derrotasAVE :: Int
  , ave :: Float
  } deriving (Show, Generic)

data TorneioAVE = TorneioAVE
  { nomeTorneioAVE :: String
  , numeroRondas :: Int
  , jogadoresAVE :: [JogadorAVE]
  } deriving (Show, Generic)

-- Estrutura para os resultados do Torneio AVE
data ResultadoAVE = ResultadoAVE
  { jogador1 :: String
  , jogador2 :: String
  , score :: String
  , vencedor :: String
  } deriving (Show, Generic)

-- Estrutura para os dados do Torneio Eliminatórias
data MatchElim = MatchElim
  { time1Elim :: String
  , time2Elim :: String
  , pontuacaoA :: String
  , pontuacaoB :: String
  , vencedorElim :: String
  } deriving (Show, Generic)

data TorneioElim = TorneioElim
  { nomeTorneioElim :: String
  , clubesParticipantes :: [String]
  , matchesElim :: [MatchElim]
  } deriving (Show, Generic)

-- Estrutura para os resultados do Torneio Eliminatórias
data ResultadoElim = ResultadoElim
  { time1ElimRes :: String
  , time2ElimRes :: String
  , resultadoElim :: String
  , vencedorElimRes :: String
  } deriving (Show, Generic)

-- Funções de Leitura

-- Leitura do Torneio AVE
readTorneioAVE :: String -> IO TorneioAVE
readTorneioAVE filePath = do
  content <- readFile filePath
  let parsedContent = parseTorneioAVE content
  return parsedContent

parseTorneioAVE :: String -> TorneioAVE
parseTorneioAVE content =
  let linesOfFile = lines content
      header = head linesOfFile
      body = tail linesOfFile
      nomeTorneio = head (words header)
      numeroRondas = read (head (tail (words header))) :: Int
      jogadores = map parseJogadorAVE body
  in TorneioAVE nomeTorneio numeroRondas jogadores

parseJogadorAVE :: String -> JogadorAVE
parseJogadorAVE line =
  let [nome, genero, vitorias, derrotas, aveStr] = words line
      ave = read aveStr :: Float
  in JogadorAVE nome genero (read vitorias) (read derrotas) ave

-- Leitura dos Resultados do Torneio AVE
readResultadosTorneioAVE :: String -> IO [ResultadoAVE]
readResultadosTorneioAVE filePath = do
  content <- readFile filePath
  let parsedResults = parseResultadosTorneioAVE content
  return parsedResults

parseResultadosTorneioAVE :: String -> [ResultadoAVE]
parseResultadosTorneioAVE content =
  let linesOfFile = lines content
  in map parseResultadoAVE linesOfFile

parseResultadoAVE :: String -> ResultadoAVE
parseResultadoAVE line =
  let [jogador1, _, jogador2, "→", score, "(Vencedor:", vencedor] = words line
  in ResultadoAVE jogador1 jogador2 score (init vencedor)

-- Leitura do Torneio Eliminatórias
readTorneioElim :: String -> IO TorneioElim
readTorneioElim filePath = do
  content <- readFile filePath
  let parsedTorneioElim = parseTorneioElim content
  return parsedTorneioElim

parseTorneioElim :: String -> TorneioElim
parseTorneioElim content =
  let linesOfFile = lines content
      header = head linesOfFile
      clubes = map head . map (words) . takeWhile (/= "") . tail $ linesOfFile
      matches = map parseMatchElim (drop (length clubes + 1) linesOfFile)
  in TorneioElim "Taça Nacional de Clubes" clubes matches

parseMatchElim :: String -> MatchElim
parseMatchElim line =
  let [time1, _, time2, _, scoreA, _, scoreB, _, _, winner] = words line
  in MatchElim time1 time2 scoreA scoreB winner

-- Leitura dos Resultados do Torneio Eliminatórias
readResultadosTorneioElim :: String -> IO [ResultadoElim]
readResultadosTorneioElim filePath = do
  content <- readFile filePath
  let parsedResults = parseResultadosTorneioElim content
  return parsedResults

parseResultadosTorneioElim :: String -> [ResultadoElim]
parseResultadosTorneioElim content =
  let linesOfFile = lines content
  in map parseResultadoElim linesOfFile

parseResultadoElim :: String -> ResultadoElim
parseResultadoElim line =
  let [time1, time2, resultado, vencedor] = words line
  in ResultadoElim time1 time2 resultado vencedor

-- Funções de Impressão

-- Função para imprimir os dados do Torneio AVE
printTorneioAVE :: TorneioAVE -> IO ()
printTorneioAVE torneio =
  putStrLn ("Torneio: " ++ nomeTorneioAVE torneio ++ "\nNúmero de Rondas: " ++ show (numeroRondas torneio)) >>
  mapM_ printJogadorAVE (jogadoresAVE torneio)

printJogadorAVE :: JogadorAVE -> IO ()
printJogadorAVE jogador =
  putStrLn (nomeJogadorAVE jogador ++ " (" ++ generoAVE jogador ++ ") — Vitórias: " ++ show (vitoriasAVE jogador) ++ "; Derrotas: " ++ show (derrotasAVE jogador) ++ "; AVE: " ++ show (ave jogador))

-- Função para imprimir os resultados do Torneio AVE
printResultadosTorneioAVE :: [ResultadoAVE] -> IO ()
printResultadosTorneioAVE resultados =
  mapM_ printResultadoAVE resultados

printResultadoAVE :: ResultadoAVE -> IO ()
printResultadoAVE resultado =
  putStrLn (jogador1 resultado ++ " vs " ++ jogador2 resultado ++ " → " ++ score resultado ++ " (Vencedor: " ++ vencedor resultado ++ ")")

-- Função para imprimir os dados do Torneio Eliminatórias
printTorneioElim :: String -> TorneioElim -> IO ()
printTorneioElim nome torneio =
  putStrLn ("Torneio: " ++ nome ++ "\nClubes Participantes:") >>
  mapM_ putStrLn (clubesParticipantes torneio) >>
  mapM_ printMatchElim (matchesElim torneio)

printMatchElim :: MatchElim -> IO ()
printMatchElim match =
  putStrLn (time1Elim match ++ " vs " ++ time2Elim match ++ " - " ++ pontuacaoA match ++ " : " ++ pontuacaoB match ++ " (Vencedor: " ++ vencedorElim match ++ ")")

-- Função para imprimir os resultados do Torneio Eliminatórias
printResultadosTorneioElim :: [ResultadoElim] -> IO ()
printResultadosTorneioElim resultados =
  mapM_ printResultadoElim resultados

printResultadoElim :: ResultadoElim -> IO ()
printResultadoElim resultado =
  putStrLn (time1ElimRes resultado ++ " vs " ++ time2ElimRes resultado ++ " → " ++ resultadoElim resultado ++ " (Vencedor: " ++ vencedorElimRes resultado ++ ")")

-- Função main para processar e imprimir os dados
main :: IO ()
main = do
  -- Caminhos para os ficheiros
  let caminhoTorneioAVE = "torneio_ave_vila_real.csv"
  let caminhoResultadosTorneioAVE = "resultados_torneio_ave_vila_real.csv"
  let caminhoTorneioElim = "torneio_16_clubes.csv"
  let caminhoResultadosTorneioElim = "resultados_torneio_16_clubes.csv"

  -- Ler os dados do Torneio AVE
  torneioAVE <- readTorneioAVE caminhoTorneioAVE
  printTorneioAVE torneioAVE

  -- Ler e imprimir os resultados do Torneio AVE
  resultadosAVE <- readResultadosTorneioAVE caminhoResultadosTorneioAVE
  printResultadosTorneioAVE resultadosAVE

  -- Ler os dados do Torneio Eliminatórias
  torneioElim <- readTorneioElim caminhoTorneioElim
  printTorneioElim "Torneio Eliminatórias" torneioElim

  -- Ler e imprimir os resultados do Torneio Eliminatórias
  resultadosElim <- readResultadosTorneioElim caminhoResultadosTorneioElim
  printResultadosTorneioElim resultadosElim






