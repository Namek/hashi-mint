@echo off
rem Run the batch from project's directory
docker run --tty -v "%cd%":/proj --rm -ti mint %*
