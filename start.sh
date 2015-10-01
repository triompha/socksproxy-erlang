#!/bin/bash
#-noshell 1>>stdout.log 2>>stderr.log&
exec erl \
    -pa ebin \
	-boot start_sasl \
    -sname socksproxy_dev \
    -config conf.config \
    -s socksproxy \
    +K true 

