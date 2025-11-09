import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Data.List (groupBy, sortBy, findIndex, sortOn)
import Data.Function (on)
import Data.Ord  (comparing, Down(..))
import qualified Data.Map.Strict as M
import qualified Data.Set as S

splitBy :: Char -> String -> [String]
splitBy _ [] = [""]
splitBy d (c:cs)
  | c == d    = "" : rest
  | otherwise = (c : head rest) : tail rest
  where
    rest = splitBy d cs

limparLinha :: String -> String
limparLinha = unwords . words

separarCSV :: String -> [String]
separarCSV = splitBy ','

separarCampos :: String -> [String]
separarCampos = separarCSV . limparLinha

safeReadInt :: String -> Int
safeReadInt s = fromMaybe 0 (readMaybe s :: Maybe Int)

safeReadDouble :: String -> Double
safeReadDouble s = fromMaybe 0.0 (readMaybe s :: Maybe Double)

pause :: IO ()
pause = do
  putStrLn "\nPressiona ENTER para voltar ao menu..."
  _ <- getLine
  pure ()

type JogadorAVE     = (String, Char, Int, Int, Double)
type TorneioAVE     = [JogadorAVE]
type ResultadoAVE   = (String, String, String, (Int, Int), String)
type Torneio        = (Int, String)
type ResultadoTorneio =
  ( String, String, String, String, String, String
  , String, String, String, String, String, String, String )

readResultadoTorneio16 :: String -> IO [ResultadoTorneio]
readResultadoTorneio16 filename = do
  content <- readFile filename
  let linesOfFile = lines content
      validLines  = filter (not . null) linesOfFile
  pure (parseResultadosTorneio16 validLines)

parseResultadosTorneio16 :: [String] -> [ResultadoTorneio]
parseResultadosTorneio16 = map parseResultadoTorneio . drop 2

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

readTorneioElim :: String -> IO [Torneio]
readTorneioElim fileName = do
  content <- readFile fileName
  let ls = lines content
  case ls of
    (_h1:_h2:rest) -> do
      let validLines = filter (not . null) rest
      pure (parseTorneio16 validLines)
    _ -> error "Ficheiro de torneio (eliminação direta) com menos de 2 linhas de header."

parseTorneio16 :: [String] -> [Torneio]
parseTorneio16 = map parseTorneio

parseTorneio :: String -> Torneio
parseTorneio line =
  case separarCSV line of
    [numero, equipa] -> (safeReadInt numero, equipa)
    _ -> error ("Formato inválido na linha: " ++ line)

printTorneio :: [Torneio] -> IO ()
printTorneio ts = mapM_ (\(n, eq) -> putStrLn $ " " ++ show n ++ ". Equipa: " ++ eq) ts

readResultadosTorneioAVE :: String -> IO [ResultadoAVE]
readResultadosTorneioAVE fileName = do
  content <- readFile fileName
  let validLines = filter (not . null) (lines content)
  pure (parseResultadosAVE validLines)

parseResultadosAVE :: [String] -> [ResultadoAVE]
parseResultadosAVE = map parseResultadoAVE . drop 2

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
  let ordenados = sortOn (\(r,_,_,_,_) -> safeReadInt r) resultados
      grupos    = groupBy ((==) `on` (\(r,_,_,_,_) -> r)) ordenados
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
parseJogadoresAVE = map parseJogadorAVE

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

runAVEParing :: TorneioAVE -> [ResultadoAVE] -> [ResultadoAVE]
runAVEParing jogadores resultadosAntigos =
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
    hasPlayed a b = S.member b (M.findWithDefault S.empty a opps)
    ordenados = sortBy (comparing (\j -> (Down (getWins j), Down (getAve j), j))) nomes
    pairUp []       = []
    pairUp [x]      = [(x,"BYE")]
    pairUp (x:y:xs)
      | not (hasPlayed x y) = (x,y) : pairUp xs
      | otherwise =
          case findIndex (\z -> not (hasPlayed x z)) xs of
            Nothing  -> (x,y) : pairUp xs
            Just k   -> let z   = xs !! k
                            xs' = take k xs ++ y : drop (k+1) xs
                        in (x,z) : pairUp xs'
    pares = pairUp ordenados
    nextRonda =
      let rs = [ safeReadInt r | (r,_,_,_,_) <- resultadosAntigos ]
      in show (if null rs then 1 else maximum rs + 1)
    novaRonda =
      [ case b of
          "BYE" -> (nextRonda, a, b, (1,0), a)
          _     -> (nextRonda, a, b, (0,0), "")
      | (a,b) <- pares
      ]
  in resultadosAntigos ++ novaRonda

runAVEparing :: TorneioAVE -> [ResultadoAVE] -> [ResultadoAVE]
runAVEparing = runAVEParing

runElimParing :: [Torneio] -> [ResultadoTorneio]
runElimParing participantes =
  let
    seedsOrd :: [(Int, String)]
    seedsOrd = sortOn fst participantes
    teams0 :: [String]
    teams0 = map snd seedsOrd
    nextPow2 :: Int -> Int
    nextPow2 n = head $ dropWhile (< n) (iterate (*2) 1)
    n0  = length teams0
    nP  = nextPow2 n0
    byes   = replicate (nP - n0) "BYE"
    teamsP = teams0 ++ byes
    pairFirstRound :: [String] -> [(String, String)]
    pairFirstRound xs =
      let k    = length xs `div` 2
          left = take k xs
          right= reverse (drop k xs)
      in zip left right
    roundName :: Int -> String
    roundName m = case m of
      2  -> "Final"
      4  -> "Meias-Finais"
      8  -> "Quartos"
      16 -> "Oitavos"
      32 -> "R32"
      64 -> "R64"
      _  -> "R" ++ show m
    buildAllRounds :: [String] -> [(String, [(String, String, String, Maybe String)])]
    buildAllRounds xs
      | length xs <= 1 = []
      | otherwise =
          let m      = length xs
              rLabel = roundName m
              pairs  = pairFirstRound xs
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
          in (rLabel, jogos1) : buildAllRounds winners
    rounds = buildAllRounds teamsP
    toResultado :: String -> (String, String, String, Maybe String) -> ResultadoTorneio
    toResultado rLabel (jId, a, b, vOpt) =
      let pontosA = ""
          pontosB = ""
          date    = ""
          hora    = ""
          local   = ""
          arbitro = ""
          estado  = maybe "Por jogar" (const "Avançou por BYE / definido") vOpt
          vencedor= maybe "" id vOpt
          obs     = if a == "BYE" || b == "BYE" then "Emparelhamento com BYE" else ""
      in ( rLabel, jId, a, b, pontosA, pontosB
         , date, hora, local, arbitro, estado, vencedor, obs )
  in
    [ toResultado r j
    | (r, js) <- rounds
    , j <- js
    ]

menu :: IO ()
menu = do
  putStrLn "\n== Menu =="
  putStrLn "1. Ler torneio AVE"
  putStrLn "2. Ler resultados do torneio AVE"
  putStrLn "3. Ler Torneio 16 clubes"
  putStrLn "4. Ler resultados do torneio de 16 clubes"
  putStrLn "5. Sair"
  putStrLn "6. Gerar próxima ronda AVE e mostrar"
  putStrLn "7. Gerar emparelhamento eliminação direta completo e mostrar"
  putStr "Escolhe uma opção: "
  opcao <- getLine
  case opcao of
    "1" -> do
      readTorneioAVE "torneio_ave_vila_real.csv" >>= printTorneioAVE
      pause
      menu
    "2" -> do
      readResultadosTorneioAVE "resultados_torneio_ave_vila_real.csv" >>= printResultadosAVE
      pause
      menu
    "3" -> do
      readTorneio "torneio_16_clubes.csv" >>= printTorneio
      pause
      menu
    "4" -> do
      readResultadoTorneio16 "resultados_torneio_16_clubes.csv" >>= printResultadosTorneio16
      pause
      menu
    "5" -> putStrLn "\nAté a próxima :)!"
    "6" -> do
      jogadores <- readTorneioAVE "torneio_ave_vila_real.csv"
      resAnt    <- readResultadosTorneioAVE "resultados_torneio_ave_vila_real.csv"
      let novos = runAVEparing jogadores resAnt
      putStrLn "\n== Resultados com nova ronda AVE adicionada ==\n"
      printResultadosAVE novos
      pause
      menu
    "7" -> do
      ts <- readTorneioElim "torneio_16_clubes.csv"
      let bracket = runElimParing ts
      putStrLn "\n== Bracket de Eliminação Direta (todas as rondas) ==\n"
      printResultadosTorneio16 bracket
      pause
      menu
    _ -> do
      putStrLn "\nOpção inválida. Tente novamente."
      pause
      menu

main :: IO ()
main = menu
