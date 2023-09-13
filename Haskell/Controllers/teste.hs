-- import Controllers.Usuario
-- import System.IO.Unsafe (unsafePerformIO)
-- import Data.Maybe

-- getEntity :: String -> [Usuario]
-- getEntity path = do
--     let file = unsafePerformIO (B.readFile path)
--     let decodeFile =decode file :: Maybe [Usuario]
--     case decodeFile of
--         Nothing -> []
--         Just out -> out

-- saveUsuario :: String -> Usuario -> IO()
-- saveUsuario path user = do
--     let newUser = user {id = length (getEntity "") + 1}
--     -- metodos p salvar


--     let newID = length (getEntity "") + 1