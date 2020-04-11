# build docker mint image from the Dockerfile folder

`docker build -t mint .` OR (Windows) `mint\build_mint_docker.bat`

# init project called `my_project` in folder c:/dev/Projects, this will create a folder named `my_project`

`docker run --tty -v c:/dev/Projects:/proj mint init my_project`

OR (Windows) `mint.bat init my_project`

# `mint start` from the project above

`docker run --tty -v c:/dev/Projects/my_project:/proj -p 3000:3000 --rm -ti mint start -h 0.0.0.0`

OR (Windows) `mint\mint_start.bat`

Then open http://localhost:3000
