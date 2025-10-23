module Main where

import System.IO
import Data.Char (isSpace)
import Text.Read (readMaybe)

data Jogador = Jogador
  { nome     :: String
  , genero   :: Char
  , vitorias :: Int
  , derrotas :: Int
  , ave      :: Double
  } deriving (Show)

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

splitComma :: String -> [String]
splitComma s = case break (== ',') s of
  (a, [])     -> [a]
  (a, _:rest) -> a : splitComma rest

parseJogador :: String -> Maybe Jogador
parseJogador linha0 =
  case map trim (splitComma (trim linha0)) of
    -- ignora vazias e o cabeçalho
    []                        -> Nothing
    ("Nome":_)                -> Nothing
    ("Torneio":_)             -> Nothing
    ("Número de rondas":_)    -> Nothing
    (n:g:v:d:a:[]) -> do
      v' <- readMaybe v
      d' <- readMaybe d
      a' <- readMaybe a
      let gc = case g of (c:_) -> c; _ -> '?'
      pure (Jogador n gc v' d' a')
    _ -> Nothing

readTorneioAve :: FilePath -> IO [Jogador]
readTorneioAve fp = do
  h <- openFile fp ReadMode
  hSetEncoding h utf8
  content <- hGetContents h  -- Lê o conteúdo completo do arquivo
  let jogadores = mapMaybe parseJogador (lines content)  -- Usa o conteúdo lido
  hClose h  -- Fecha o arquivo depois de processar todo o conteúdo
  pure jogadores
  where
    mapMaybe _ [] = []
    mapMaybe f (x:xs) = case f x of
      Just y  -> y : mapMaybe f xs
      Nothing ->     mapMaybe f xs

printJogador :: Jogador -> IO ()
printJogador j = do
  putStrLn $ "Nome: "     ++ nome j
  putStrLn $ "Género: "   ++ [genero j]
  putStrLn $ "Vitórias: " ++ show (vitorias j)
  putStrLn $ "Derrotas: " ++ show (derrotas j)
  putStrLn $ "AVE: "      ++ show (ave j)
  putStrLn ""

main :: IO ()
main = do
  jogadores <- readTorneioAve "torneio_ave_vila_real.csv"
  if null jogadores
    then putStrLn "Sem jogadores (CSV vazio ou formato inesperado)."
    else mapM_ printJogador jogadores
