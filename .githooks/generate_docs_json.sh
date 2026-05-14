#! /bin/bash

echo "Generating docs.json..."

if elm make --docs=docs.json; then
    echo "Success!"
else 
    echo "Failed!"
fi