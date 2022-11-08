{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Data.Map as M
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
    groupBy (\(a,b,_) (c,d,_) -> a == c && b == d) $ sort
      [ (topic1, s1, t2 - t1) | PublishRequest t1 topic1 s1 <- msgs
                              , Delivered      t2 topic2 s2 <- msgs
                              , topic1 == topic2, s1 == s2 ]


reliability :: [LogMsg] -> [Double]
reliability msgs = map msgReliability publishedMsgs

  where

    publishedMsgs :: [(Topic, Id)]
    publishedMsgs = [ (t,i) | PublishRequest _ t i <- msgs ]

    nDelivered :: M.Map (Topic, Id) Double
    nDelivered = foldr (\m acc -> case m of
                                           Delivered _ dt di -> M.alter (\case Nothing -> Just 1
                                                                               Just x  -> Just $ x + 1) (dt, di) acc
                                           _ -> acc
                                           ) mempty msgs
                -- sum [ 1 | Delivered _ dt di <- msgs , dt == t, di == i ]

    nSubscriptions :: M.Map Topic Double
    nSubscriptions = foldr (\m acc -> case m of
                                           SubscriptionRequest _ dt -> M.alter (\case Nothing -> Just 1
                                                                                      Just x  -> Just $ x + 1) dt acc
                                           _ -> acc
                                           ) mempty msgs
                        -- sum [ 1 | SubscriptionRequest _  dt <- msgs, dt == t ]

    msgReliability :: (Topic, Id) -> Double
    msgReliability (t, i) = nDelivered M.! (t, i) / nSubscriptions M.! t


average :: Fractional a => [a] -> a
average l = sum l / fromIntegral (length l)

