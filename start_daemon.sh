#!/bin/bash

run_erl -daemon log/ log "exec erl -pa ebin deps/*/ebin -s erbot -name erbot@`hostname -s`"
