module Main where
import Menus.MenuGeral (menuPrincipal)

-- Menu principal do projeto, que chama todos os demais
main :: IO ()
main = do
    menuPrincipal