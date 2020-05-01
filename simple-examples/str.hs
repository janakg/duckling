import Asterius.ByteString
import Asterius.Text
import Asterius.Types   
import Data.Text (Text)
import Data.String
-- import Data.JSString.Text
-- import Data.Text.Conversions (cs)
import Data.ByteString (ByteString, empty)
import qualified Data.Text as Text
import Data.Text.Encoding
-- import qualified Data.ByteString.Lazy as LBS
foreign export javascript "st_test" text_test :: JSString -> JSString

main :: IO ()
main = do
  let y = mid_step "[{\"body\":\"today at 9am\",\"start\":0,\"value\":{\"values\":[{\"value\":\"2020-04-28T09:00:00.000-02:00\",\"grain\":\"hour\",\"type\":\"value\"}],\"value\":\"2020-04-28T09:00:00.000-02:00\",\"grain\":\"hour\",\"type\":\"value\"},\"end\":12,\"dim\":\"time\",\"latent\":false}]"
  print y

text_test :: JSString -> JSString
text_test inText = do
    -- let hsText =  fromJSString inText
    -- mid_step $ Text.pack hsText
    mid_step $ textFromJSString inText

mid_step :: Text -> JSString
mid_step inText = do
    -- let s = t_test $ toJSString $ Text.unpack inText
    let s = t_test $ textToJSString inText
    textToJSString $decodeUtf8 s

t_test :: JSString -> ByteString
t_test inputTxt = do
    let s = textFromJSString inputTxt
    encodeUtf8 s