#!/bin/bash
# Test deploy script - writes the output folder arg to a marker file
echo "$1" > "$1/.deploy-marker"
