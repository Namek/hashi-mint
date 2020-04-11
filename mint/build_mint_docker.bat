@echo off
pushd %~dp0
docker build -t mint .
popd