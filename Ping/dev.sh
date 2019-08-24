#!/bin/bash

echo "Setting up local Elm 0.18.0 dev environment..."
npm i
export PATH="$(pwd)/node_modules/.bin:$PATH"
echo "All done."
