{-# LANGUAGE DeriveFunctor, GADTs, OverloadedStrings #-}
module Main where
import Control.Monad.Free
import Control.Monad.State.Lazy
import Control.Monad
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T

-- data Sebas = Sebas { functions :: [Text] }
-- | セバスは執事ですわ。執事たるもの,わたくしの物全て管理するべきよね!
--
-- お嬢様が活動する際の全てのコンテクストを管理します。
-- 現状は定義された変数名とその内容のみを保管していますが,
-- 後々増えてきたらデータ型に再定義されます。
type Sebas = [(Text, Text)]

type Myお嬢様 a = StateT Sebas IO a

data Keyword = Aありますの | Aといって | Aには何がありますの | Aですわよ | Aでしたら | Aですわ | Aそうでなければ
  deriving (Eq, Ord)

といって = Aといって
がありますの = Aありますの
には何がありますの  = Aには何がありますの
ですわよ = Aですわよ
でしたら = Aでしたら
ですわ = Aですわ
そうでなければ = Aそうでなければ

-- | 変数を定義するわ。 文法ミスに気をつけることね
--
-- 変数定義の文法です。
この部屋は :: Text -> Keyword -> Text -> Keyword -> StateT Sebas IO ()
この部屋は thing といって content がありますの
  | といって == Aといって && がありますの == Aありますの = modify addVal
  | otherwise = return ()
  where
    val = (thing, content)
    addVal sebas = val:sebas

セバス :: Text -> Keyword -> StateT Sebas IO (Maybe Text)
セバス roomName には何がありますの
  | には何がありますの == Aには何がありますの = gets getValue
  | otherwise = return Nothing
  where
    getValue sebas = fmap snd $ find (\c -> fst c == roomName) sebas

-- こちらの funcName 様は引数として arg をお受け取りになって次のことをなさいます :: Text -> a -> Keyword -> (a -> b) -> Anお嬢様 (a -> b)


みなさま :: Text -> Keyword -> StateT Sebas IO ()
みなさま content ですわよ  | ですわよ == Aですわよ = liftIO (putStrLn . T.unpack $ content)
                           | otherwise = return ()
-- もし = liftF If
-- でしたら = liftF Else
-- お返しするのは = liftF Return
-- ですわ = liftF EOL ()

もし :: Bool -> Keyword -> Myお嬢様 a -> Keyword -> Myお嬢様 a -> Myお嬢様 a
もし condition でしたら ifTrue そうでなければ ifFalse
  | でしたら == Aでしたら
    && そうでなければ == Aそうでなければ = if condition then ifTrue else ifFalse
  | otherwise = ifFalse

main = flip execStateT [] $ do
  この部屋は "cat room" といって "cat" がありますの
  whatsInsideCatRoom <- セバス "cat room" には何がありますの
  みなさま (maybe "" (flip T.append " ですわ!みなさま~!!") whatsInsideCatRoom) ですわよ

  もし (whatsInsideCatRoom == (Just "cat")) でしたら
    (みなさま "猫が!猫がおりますわよ~!" ですわよ)
    そうでなければ (みなさま "猫が...猫がおりませんのよ!?" ですわよ)

  この部屋は "cat room" といって "dog" がありますの
  whatsInsideCatRoom <- セバス "cat room" には何がありますの

  もし (whatsInsideCatRoom == (Just "cat")) でしたら
    (みなさま "猫が!猫がおりますわよ~!" ですわよ)
    そうでなければ (みなさま "猫が...猫がおりませんのよ!?" ですわよ)

  -- この部屋は "Result" といって (maybe "" "atta" whatsInsideCatRoom) がありますの
  
  -- お屋敷には "ねこ" がいましてよ
  -- 以上ですの
  -- こちらの fib 様は 引数として n をお受け取りになって 次のことをなさいます $
  --   もし n == 0 でしたら お返しするのは 1 ですわ

  --   以上ですわ
