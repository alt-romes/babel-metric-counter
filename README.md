# Generate Metrics for the Babel Project

## Logging

There are 5 different types of messages that must be be logged by the upper
layer application protocol in a particular format for the tool to parse and take
into consideration when calculating the metrics.

The 4 types and their format are the following

```
[[SUBSCRIPTION REQUEST]]:topic_uuid
[[SUBSCRIPTION REPLY]]:topic_uuid
[[PUBLISH REQUEST]]:(topic_uuid)(message_uuid)
[[DELIVERED]]:(topic_uuid)(message_uuid)
```

In the `AutomatedApp` class, this would look like:

```java
    public void init(Properties props) {
        // ...
        for (String topic : this.subscribeTopics) {
            SubscriptionRequest sreq = new SubscriptionRequest(topic);
            sendRequest(sreq, pubSubProtoId);
            // ...
            logger.info("[[SUBSCRIPTION REQUEST]]:" + topic);
        }
    }

    private void uponSubscriptionReply(SubscriptionReply reply, short sourceProto) {
        // ...
        logger.info("[[SUBSCRIPTION REPLY]]:" + reply.getTopic());
    }

    private void uponBroadcastTimer(DisseminationTimer broadcastTimer, long timerId) {
        // ... 
        logger.info("[[PUBLISH REQUEST]]:({})({})", request.getTopic(), request.getMsgID());
    }

    private void uponDeliver(DeliverNotification reply, short sourceProto) {
        // ...
        logger.info("[[DELIVERED]]:({})({})", reply.getTopic(), reply.getMsgId());
    }
```


Additionally, the logging configuration `log4j2.xml` should be changed so that the time is
printed as the number of miliseconds:
```xml
<Configuration status="INFO" shutdownHook="disable">
    <Appenders>
        <Console name="Console" target="SYSTEM_OUT">
				<PatternLayout pattern="%highlight{%level{length=1}[%d{UNIX_MILLIS}] [%t]%logger{0}:} %msg%n"/>
    ...
```

## Using the metric_counter

To use the metric counter tool, all log files should reflect the above
modifications and be concatenated into a single file, which could be done with
```
cat log1.txt log2.txt log4.txt > all_logs.txt
```

If you redirect from the stdout to a log file, you might get color escape codes mixed in the logs.
To remove them, you can run
```
sed -e 's/\x1b\[[0-9;]*m//g' all_logs.txt > all_logs.clean.txt
```

Then, run metric counter with cabal passing the file name as an argument
```
cabal run metric-counter all_logs.txt
```

## Compiling the application

The application is written in Haskell. To run it, download GHC and cabal
(haskell's package manager) using [GHCUP](https://www.haskell.org/ghcup/)
(regarding options, you don't need to install any other optional tools such as
`haskell-language-server` and `stack` when prompted; unless you want to play a
bit with haskell, in which case I'd recommend installing
the `haskell-language-server` and the VSCode extension that uses it).

With that installed, you can run the above command `cabal run metric-counter
all_logs.txt` which will compile the app if it hasn't yet.

