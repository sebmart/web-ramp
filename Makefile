.PHONY: clean web pullramp

clean:
	./sbt clean

web:
	sh server.sh start

pullramp:
	cd ../ramp-metering; mvn install -DskipTests
	./sbt update

