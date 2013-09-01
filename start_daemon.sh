#!/bin/bash

run_erl -daemon log/ log "exec elixir --no-halt --name erbot@`hostname -s` --sname erbot -S mix app.start"
