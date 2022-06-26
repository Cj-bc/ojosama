{-# LANGUAGE DeriveFunctor, GADTs, OverloadedStrings #-}
module Main where
import Control.Monad.Free
import Control.Monad.State.Lazy
import Control.Monad
import Data.List (find)
import Data.Text (Text)

-- data Sebas = Sebas { functions :: [Text] }
-- | セバスは執事ですわ。執事たるもの,わたくしの物全て管理するべきよね!
--
-- お嬢様が活動する際の全てのコンテクストを管理します。
-- 現状は定義された変数名とその内容のみを保管していますが,
-- 後々増えてきたらデータ型に再定義されます。
type Sebas = [(Text, Text)]

-- | コンピューターには自然言語は難しいらしいから, 分かりやすくASTにしてあげたわ!
--
-- 残念ながらHaskellでは,型名や型コンストラクターをUTF-8で始められないので
-- 'My' 接頭辞を付けています
-- data Myお嬢様 r = DefineFunc Text r
--                 | DefineVariable Text Text r
--                 | ReadVariable Text r
--                 | Arg r
--                 | Return Text r
--                 deriving (Functor)
data Myお嬢様 r where
  DefineFunc :: Text -> (a -> b) -> r -> Myお嬢様  r
  DefineVariable :: Text -> Text -> r ->  Myお嬢様  r
  ReadVariable :: Text -> r -> Myお嬢様  r
  Output :: Text -> r -> Myお嬢様 r
  Arg :: r -> Myお嬢様  r
  Return :: a -> r -> Myお嬢様  r

instance Functor Myお嬢様 where
  fmap f (DefineFunc fn body r) = DefineFunc fn body (f r)
  fmap f (DefineVariable name content r) = DefineVariable name content (f r)
  fmap f (ReadVariable name r) = ReadVariable name (f r)
  fmap f (Output content r) = Output content (f r)
  fmap f (Arg r) = Arg (f r)
  fmap f (Return v r) = Return v (f r)
  

data Keyword = Aありますの | Aといって | Aには何がありますの | Aですわよ
  deriving (Eq, Ord)

といって = Aといって
がありますの = Aありますの
には何がありますの  = Aには何がありますの
ですわよ = Aですわよ

-- | 変数を定義するわ。 文法ミスに気をつけることね
--
-- 変数定義の文法です。
この部屋は :: Text -> Keyword -> Text -> Keyword -> Anお嬢様 ()
この部屋は thing といって content がありますの
  | といって == Aといって && がありますの == Aありますの = liftF $ DefineVariable thing content ()
  | otherwise = Pure ()

セバス :: Text -> Keyword -> Anお嬢様 (Maybe Text)
セバス roomName には何がありますの
    | には何がありますの == Aには何がありますの = liftF $ ReadVariable roomName Nothing
    -- | には何がありますの == Aには何がありますの = Free . fmap return $ ReadVariable roomName Nothing
    -- | には何がありますの == Aには何がありますの = Free $ ReadVariable roomName (return Nothing)
    | otherwise = Pure Nothing

-- こちらの funcName 様は引数として arg をお受け取りになって次のことをなさいます :: Text -> a -> Keyword -> (a -> b) -> Anお嬢様 (a -> b)


みなさま :: Text -> Keyword -> Anお嬢様 ()
みなさま content ですわよ  | ですわよ == Aですわよ = liftF $ Output content ()
                           | otherwise = Pure ()
-- もし = liftF If
-- でしたら = liftF Else
-- お返しするのは = liftF Return
-- ですわ = liftF EOL ()

以上ですわ :: Anお嬢様 ()
以上ですわ = Pure ()

-- 次のことをなさいます = 

-- | わたくしのことよ!
--
-- Freeモナドに包まれたお嬢様です。
-- 具象化されているのに不定冠詞に変わっていますね。不思議なこともあるものです。
type Anお嬢様 a = Free Myお嬢様 a

-- | 走らせるですわ!
--
-- お嬢様言葉のインタープリターです
ご案内しますわ :: Anお嬢様 a -> Sebas -> IO a
ご案内しますわ f sebas = case f of
  -- (DefineFunc fn r) -> do
  --   modify (\sebas -> (fn, ""):sebas)
  --   ご案内しますわ r
  (Free (DefineVariable name content r)) ->
    let s = (name, content):sebas
    in ご案内しますわ r s
  (Free (ReadVariable roomName r)) ->
    let val = fmap snd $ find (\c -> fst c == roomName) sebas
    in ご案内しますわ r sebas
    -- in ご案内しますわ (Pure val) sebas
  (Free (Output content r)) -> do
    print content
    ご案内しますわ r sebas
    
  -- (Arg r) -> 
  -- Return Sebas r
  -- EOL Sebas r -> ご案内しますわ r
  -- (Free End) -> sebas
  (Pure a) -> return a
  _ -> error "誰ですの!？わたくし知りませんわ!"

main = void . flip ご案内しますわ [] $ do
  この部屋は "cat room" といって "cat" がありますの
  whatsInsideCatRoom <- セバス "cat room" には何がありますの :: Free Myお嬢様 (Maybe Text)
  この部屋は "Result" といって (maybe "" id whatsInsideCatRoom) がありますの
  Pure whatsInsideCatRoom
  
  -- お屋敷には "ねこ" がいましてよ
  -- 以上ですの
  -- こちらの fib 様は 引数として n をお受け取りになって 次のことをなさいます $
  --   もし n == 0 でしたら お返しするのは 1 ですわ

  --   以上ですわ
