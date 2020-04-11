@echo off
echo Run this batch from project's directory!
docker run --tty -v "%cd%":/proj -p 3000:3000 --rm -ti mint start -h 0.0.0.0 --auto-format
