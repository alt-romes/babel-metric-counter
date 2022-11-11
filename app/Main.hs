{-# LANGUAGE LambdaCase #-}
module Main where

import Debug.Trace
import qualified Data.Map as M
import Data.Maybe
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
subscribeLatencies msgs = mapMaybe toTimes $ M.toList $
  foldr (\m acc -> case m of
            SubscriptionRequest t topic1 -> M.alter (\case Nothing -> Just (Just t, Nothing); Just (_, t2) -> Just (Just t, t2)) topic1 acc
            SubscriptionReply   t topic2 -> M.alter (\case Nothing -> Just (Nothing, Just t); Just (t1, _) -> Just (t1, Just t)) topic2 acc
            _ -> acc) mempty msgs

  where
    toTimes (a,b) = case b of
                      (Just c, Just d) -> Just $ d - c
                      _ -> trace ("Couldn't find any subscription replies for " <> show a) Nothing

publishLatencies :: [LogMsg] -> [Time]
publishLatencies msgs = mapMaybe toTimes $ M.toList $
  foldr (\m acc -> case m of
                     PublishRequest t topic s ->
                       M.alter (\case Nothing -> Just (Just t, Nothing); Just (_, t2) -> Just (Just t,t2)) (topic, s) acc
                     Delivered t topic s ->
                       M.alter (\case Nothing -> Just (Nothing, Just t); Just (t1, t2) -> Just (t1, max t <$> t2)) (topic, s) acc
                     _ -> acc) mempty msgs
  where
    toTimes (a,b) = case b of
                      (Just c, Just d) -> Just (d - c)
                      (Nothing, _) -> trace ("Couldn't find any publish requests for " <> show a) Nothing
                      (_, Nothing) -> trace ("Couldn't find any delivered for " <> show a) Nothing


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

    nSubscriptions :: M.Map Topic Double
    nSubscriptions = foldr (\m acc -> case m of
                                           SubscriptionRequest _ dt -> M.alter (\case Nothing -> Just 1
                                                                                      Just x  -> Just $ x + 1) dt acc
                                           _ -> acc
                                           ) mempty msgs

    msgReliability :: (Topic, Id) -> Double
    msgReliability (t, i) = (case M.lookup (t, i) nDelivered of Nothing -> 0; Just x -> x) / nSubscriptions M.! t


average :: Fractional a => [a] -> a
average l = sum l / fromIntegral (length l)

