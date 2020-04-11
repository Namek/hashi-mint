@echo off
echo Run this batch from project's directory!
docker run --tty -v "%cd%":/proj -p 3001:3001 --rm -ti mint test --manual --host 0.0.0.0
