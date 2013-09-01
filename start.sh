#!/bin/bash

exec elixir --no-halt --name erbot@`hostname -s` --sname erbot -S mix app.start
