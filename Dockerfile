FROM ubuntu:21.04
#Install all needed libraries
RUN apt update && apt upgrade -y \
  && apt install libsodium-dev -y \
  && apt install libnuma-dev -y \
  && apt install libffi-dev -y \
  && apt install curl -y;

#Install previsous versions of libffi libs
RUN curl -LO http://archive.ubuntu.com/ubuntu/pool/main/libf/libffi/libffi6_3.2.1-8_amd64.deb \
    && dpkg -i libffi6_3.2.1-8_amd64.deb;
RUN apt install libffi6 libffi7 -y 

# Preparing binary to run in container. 
WORKDIR /cardano-markets-tracker
COPY temp-build/tracker-app /cardano-markets-tracker/
COPY tracker/resources/config.dhall /etc/cardano-markets-tracker/
EXPOSE 8082
ENTRYPOINT /cardano-markets-tracker/tracker-app $0