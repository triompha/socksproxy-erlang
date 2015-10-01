#!/bin/bash
exec erl \
    -pa ebin \
	-boot start_sasl \
    -sname socksproxy_dev \
    -config conf.config \
    -s socksproxy \
    +K true \
	-noshell 1>>stdout.log 2>>stderr.log&

