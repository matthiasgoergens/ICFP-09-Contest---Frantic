#!/bin/sh

VERSION=0.1
LOGGING_CONFIG=logging.properties

java -Djava.util.logging.config.file=$LOGGING_CONFIG -Xdebug -Xrunjdwp:transport=dt_socket,server=y,suspend=y,address=5005 -jar ../JavaVM/dist/java_vm-$VERSION.jar $1