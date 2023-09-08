module Util.AbrirFecharArquivo where
import System.IO (withFile, IOMode(ReadWriteMode, WriteMode), hClose)

-- Fecha o arquivo em questão para permitir a edição
fecharArquivo :: String -> IO ()
fecharArquivo path = do

  -- Abre o arquivo em modo de leitura e escrita para fechá-lo
  withFile path ReadWriteMode $ \handle -> do

    -- Fecha o arquivo
    hClose handle
    
  -- Abre o arquivo novamente em modo de escrita para permitir a edição
  withFile path WriteMode $ \_ -> return ()