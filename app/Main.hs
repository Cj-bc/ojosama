{-# LANGUAGE DeriveFunctor, GADTs #-}
module Main where
import Control.Monad.Free
import Control.Monad.State.Lazy
import Control.Monad
import Data.List (find)

-- data Sebas = Sebas { functions :: [String] }
-- | セバスは執事ですわ。執事たるもの,わたくしの物全て管理するべきよね!
--
-- お嬢様が活動する際の全てのコンテクストを管理します。
-- 現状は定義された変数名とその内容のみを保管していますが,
-- 後々増えてきたらデータ型に再定義されます。
type Sebas = [(String, String)]

-- | コンピューターには自然言語は難しいらしいから, 分かりやすくASTにしてあげたわ!
--
-- 残念ながらHaskellでは,型名や型コンストラクターをUTF-8で始められないので
-- 'My' 接頭辞を付けています
data Myお嬢様 r a where
  DefineFunc :: String -> r -> Myお嬢様 r ()
  DefineVariable :: String -> String -> Keyword -> Myお嬢様 r ()
  ReadVariable :: String -> r -> Myお嬢様 r String
  Arg :: r -> Myお嬢様 () r
  Return :: a -> r -> Myお嬢様 r a
  EOL :: Myお嬢様 r ()
  End :: Myお嬢様 r ()
  SyntaxError :: Myお嬢様 r ()

instance Functor (Myお嬢様 r) where
  fmap f (DefineFunc fn r) = DefineFunc fn (f r)
  fmap f (DefineVariable name content r) = DefineVariable name content (f r)
  fmap f (ReadVariable name r) = ReadVariable name (f r)
  fmap f (Arg name r) = Arg name (f r)
  fmap f (Return v r) = Return (v r)
  fmap f EOL = EOL
  fmap f End = End
  fmap f SyntaxError = SyntaxError
  

data Keyword = Aありますの | Aといって | Aには何がありますの
  deriving (Eq, Ord)

といって = Aといって
がありますの = Aありますの
には何がありますの  = Aには何がありますの

-- | 変数を定義するわ。 文法ミスに気をつけることね
--
-- 変数定義の文法です。
この部屋は :: String -> Keyword -> String -> Keyword -> Anお嬢様 ()
この部屋は thing といって content がありますの
  | といって == Aといって && がありますの == Aありますの = liftF $ DefineVariable thing content  ()
  | otherwise = liftF SyntaxError

セバス :: String -> Keyword -> Anお嬢様 String
セバス roomName には何がありますの
    | には何がありますの == Aには何がありますの = liftF $ ReadVariable (const roomName) ()
    | otherwise = liftF SyntaxError

-- もし = liftF If
-- でしたら = liftF Else
-- お返しするのは = liftF Return
-- ですわ = liftF EOL ()
以上ですわ :: Anお嬢様 Sebas
以上ですわ = liftF End

-- 次のことをなさいます = 

-- | わたくしのことよ!
--
-- Freeモナドに包まれたお嬢様です。
-- 具象化されているのに不定冠詞に変わっていますね。不思議なこともあるものです。
type Anお嬢様 a = Free (Myお嬢様 a) a

-- | わたくしのお屋敷のことはセバスが何でも知ってるわ!
--
-- まぁ見ての通りです。
type O屋敷 a = State Sebas a

-- | 走らせるですわ!
--
-- お嬢様言葉のインタープリターです
ご案内しますわ :: Anお嬢様 a -> Sebas -> IO a
ご案内しますわ f sebas = case f of
  -- (DefineFunc fn r) -> do
  --   modify (\sebas -> (fn, ""):sebas)
  --   ご案内しますわ r
  (Free (DefineVariable name content r)) -> do
    let s = (name, content):sebas
    ご案内しますわ r s
  (Free (ReadVariable name r)) -> do
    var <- maybe "" snd $ find (\c -> fst c == name) sebas
    return var
    
  -- (Arg r) -> 
  -- Return Sebas r
  -- EOL Sebas r -> ご案内しますわ r
  (Free SyntaxError) -> get >>= return
  (Free End) -> get >>= return
  (Pure _) -> get >>= return
  _ -> error "誰ですの!？わたくし知りませんわ!"

main = print . flip execState [] . ご案内しますわ $ do
  この部屋は "cat room" といって "cat" がありますの
  
  --whatsInsideCatRoom <- セバス "cat room" には何がありますの
  -- お屋敷には "ねこ" がいましてよ
  -- 以上ですの
  -- こちらの fib 様は 引数として n をお受け取りになって 次のことをなさいます $
  --   もし n == 0 でしたら お返しするのは 1 ですわ

  --   以上ですわ
