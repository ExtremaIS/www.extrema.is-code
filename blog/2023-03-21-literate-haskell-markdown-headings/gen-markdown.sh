#!/usr/bin/env bash

literatex -i Demo.lhs | sed 's/^\\#/#/' > Demo.md
