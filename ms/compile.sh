#!/bin/bash
cd "$(dirname "$0")"
latexmk -xelatex -interaction=nonstopmode main.tex 2>&1 | grep -v "^$"
