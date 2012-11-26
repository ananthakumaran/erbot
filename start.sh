#!/bin/bash

exec erl -pa ebin deps/*/ebin -s erbot -name erbot@`hostname -s`
