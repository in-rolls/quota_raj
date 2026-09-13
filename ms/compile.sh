#!/bin/bash
set -eu
cd "$(dirname "$0")"
exec latexmk -xelatex -interaction=nonstopmode -halt-on-error main.tex
