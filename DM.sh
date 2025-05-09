#!/bin/bash

javac -cp "./lib/abcl-1.9.2/abcl.jar:./lib/abcl-1.9.2/abcl-contrib.jar:./lib-build/commons-compress-1.27.1.jar:./lib-build/commons-io-2.18.0.jar:./lib-build/commons-lang3-3.17.0.jar" -d "./build-temp" ./src-java/dm_java/*.java
java  -cp "./lib/abcl-1.9.2/abcl.jar:./lib/abcl-1.9.2/abcl-contrib.jar:./lib-build/commons-compress-1.27.1.jar:./lib-build/commons-io-2.18.0.jar:./lib-build/commons-lang3-3.17.0.jar:./build-temp" dm_java.Main %*
