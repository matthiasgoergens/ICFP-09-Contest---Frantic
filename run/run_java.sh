#!/bin/sh

VERSION=0.1
LOGGING_CONFIG=logging.properties

java -Djava.util.logging.config.file=$LOGGING_CONFIG -Xms128m -Xmx1024m -jar ../JavaVM/dist/java_vm-$VERSION.jar $1