#!/usr/bin/env bash

if [[ -z $(which -s pylint) ]]
then
    pylint "$1" 2>/dev/null
fi
if [[ -z $(which -s pyflakes) ]]
then
    pyflakes "$1"
fi
if [[ -z $(which -s pep8) ]]
then
    # http://pep8.readthedocs.org/en/latest/intro.html#error-codes
    pep8 --ignore=E701,E501,E302 --repeat "$1"
fi
true
