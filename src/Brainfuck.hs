module Brainfuck where

import Data.Bifunctor (first, second)
import GHC.IOArray
import Data.Word (Word8)
import Data.ByteString.Internal (c2w,w2c)

-- Brainfuckの命令
data BFOP = PINC      -- メモリ参照インデックスをインクリメント
          | PDEC      -- メモリ参照インデックスをデクリメント
          | VINC      -- メモリの値をインクリメント
          | VDEC      -- メモリの値をデクリメント
          | PUTC      -- メモリの値を出力
          | GETC      -- 入力された値をメモリに格納
          | LOOPS Int -- Loop開始命令。保持しているインデックスは対応するLoop終了命令のインデックス
          | LOOPE Int -- Loop終了命令。保持しているインデックスは対応するLoop開始命令のインデックス
          deriving Show

-- Brainfuck実行時の命令のサイズ
memorySize = 30000 :: Int

-- Brainfxxkのコードを命令の配列に変換する。
compile :: String -> Either String [BFOP]
compile bfcode = case compile' 0 bfcode [] of
                   Right (bfops, es) -> if null [] then Right bfops
                                        else Left "[COMPILE ERROR] Lacking loop-end character(])."
                   Left err          -> Left err
  where
    compile' :: Int    -- 現在のインデックス
             -> String -- bfコード文字列
             -> [Int]  -- LOOPSのインデックスのスタック
             -> Either String ([BFOP],[Int]) -- BFの命令列と、LOOPEのインデックスのスタック
    compile' _ [] _            = Right ([],[])
    compile' index (c:str) ss = case c of
      '>' -> addOperation PINC
      '<' -> addOperation PDEC
      '+' -> addOperation VINC
      '-' -> addOperation VDEC
      '.' -> addOperation PUTC
      ',' -> addOperation GETC
      '[' -> case compile' (index+1) str (index:ss) of
               Right (bfops,es) -> case es of
                                     []    -> Left  "[COMPILE ERROR] Lacking loop-end character(])."
                                     e:es' -> Right ((LOOPS e):bfops, es')
               Left err         -> Left err
      ']' -> case ss of
               []    -> Left "[COMPILE ERROR] Lacking loop-start character(]).."
               s:ss' -> case compile' (index+1) str ss' of
                          Right (bfops,es) -> Right ((LOOPE s):bfops, index:es)
                          Left err         -> Left err
      _   -> compile' index str ss
      where
        addOperation op = second (first (op:)) $ compile' (index+1) str ss

-- Brainfuckの命令の列を実行する。
-- ポインタはIOArrayで表現する。
runBF :: [BFOP] -> IO ()
runBF bfops = do
  memory <- newIOArray (0,memorySize-1) 0 :: IO (IOArray Int Word8)
  runBF' 0 0 memory
  where
    oplength = length bfops
    runBF' :: Int -> Int -> IOArray Int Word8 -> IO ()
    runBF' memindex opindex memory
      | 0 > opindex  || opindex  >= oplength    = return ()
      | 0 > memindex || memindex >= memorySize  = putStrLn "[RUNTIME ERROR] Memory-reference index is out of range."
      | otherwise = do
          case bfops !! opindex of
            PINC -> next (memindex+1)
            PDEC -> next (memindex-1)
            VINC -> updateMemory (+1)        >> next memindex
            VDEC -> updateMemory (\v -> v-1) >> next memindex
            GETC -> c2w `fmap` getChar   >>= writeMemory >> next memindex
            PUTC -> w2c `fmap` readMemory >>= putChar   >> next memindex
            LOOPS enindex -> do
              w <- readMemory
              if w == 0 then -- 対応するLOOPEの直後の命令へジャンプ
                jump (enindex+1)
              else
                next memindex
            LOOPE stindex -> do
              w <- readMemory
              if w /= 0 then -- 対応するLOOPSの直後の命令へジャンプ
                jump (stindex+1)
              else
                next memindex
      where
        next memindex' = runBF' memindex' (opindex+1) memory
        jump opindex'  = runBF' memindex opindex' memory
        readMemory = readIOArray memory memindex
        writeMemory v = writeIOArray memory memindex v
        updateMemory f = do
          v <- readMemory
          writeMemory (f v)

-- Brainfuckkコードを実行する。
evaluate :: String -> IO ()
evaluate bfcode = do
  case compile bfcode of
    Right bfops -> runBF bfops
    Left  err   -> putStrLn err
