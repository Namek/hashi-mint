FROM alpine:latest

# RUN wget -O mint https://github.com/mint-lang/mint/releases/download/0.9.0/mint-0.9.0-linux
RUN wget -O mint https://mint-lang.s3-eu-west-1.amazonaws.com/mint-latest-linux
# ADD mint /mint
RUN chmod +x /mint

EXPOSE 3000
EXPOSE 3001
WORKDIR /proj
ENTRYPOINT ["/mint"]

