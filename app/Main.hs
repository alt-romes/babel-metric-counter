{-# LANGUAGE LambdaCase #-}
module Main where

import Data.List
import System.Environment
import Parser

main :: IO ()
main = do

  [logName] <- getArgs

  logs <- filter (/= Ignore) <$> parseFile logName
  putStrLn "" -- logs

  let sl = subscribeLatencies logs
      pl = publishLatencies logs
      rl = reliability logs

  putStr "Subscribe Average (seconds):   "
  print $ average sl

  putStr "Publish Average (seconds):     "
  print $ average pl

  putStr "Reliability Average (ratio): "
  print $ average rl
  putStrLn ""

  putStrLn "Subscribe Latencies"
  print sl
  putStrLn ""

  putStrLn "Publish Latencies"
  print pl
  putStrLn ""

  putStrLn "Reliabilities"
  print rl
  putStrLn ""


subscribeLatencies :: [LogMsg] -> [Time]
subscribeLatencies msgs =
    [ t2 - t1 | SubscriptionRequest t1 topic1 <- msgs
              , SubscriptionReply   t2 topic2 <- msgs
              , topic1 == topic2 ]

publishLatencies :: [LogMsg] -> [Time]
publishLatencies msgs =
  map (maximum . (map (\(_, _, c) -> c))) $
    groupBy (\(a,b,_) (c,d,_) -> a == c && b == d)
      [ (topic1, s1, t2 - t1) | PublishRequest t1 topic1 s1 <- msgs
                              , Delivered      t2 topic2 s2 <- msgs
                              , topic1 == topic2, s1 == s2 ]


reliability :: [LogMsg] -> [Double]
reliability msgs = map msgReliability publishedMsgs

  where

    publishedMsgs :: [(Topic, Id)]
    publishedMsgs = [ (t,i) | PublishRequest _ t i <- msgs ]

    nDelivered :: (Topic, Id) -> Double
    nDelivered (t,i) = sum [ 1 | Delivered _ dt di <- msgs , dt == t, di == i ]

    nSubscriptions :: Topic -> Double
    nSubscriptions t = sum [ 1 | SubscriptionRequest _  dt <- msgs, dt == t ]

    msgReliability :: (Topic, Id) -> Double
    msgReliability (t, i) = nDelivered (t, i) / nSubscriptions t


average :: Fractional a => [a] -> a
average l = sum l / fromIntegral (length l)

