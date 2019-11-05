module Upload where
 
import Control.Promise (Promise)

foreign import upload :: {key :: String, file :: String} -> Promise String
foreign import resolveAndUpload :: {key :: String, file :: String} -> Promise String