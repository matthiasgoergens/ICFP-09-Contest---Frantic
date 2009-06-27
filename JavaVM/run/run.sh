#!/bin/sh

VERSION=0.1
LOGGING_CONFIG=logging.properties

java -ea -Djava.util.logging.config.file=$LOGGING_CONFIG -jar ../dist/java_vm-$VERSION.jar $1