# Docker stuffs!

* Shell scripts: Installs docker containers within the directory the script has ran. Therefore, copy or link them to somewhat secluded directory.

* *.yml files: They are docker compose directive files. These guys need to be re-named to 'docker-compose.yml' at some secluded directory. Make sure you have docker-compose first!!

* _env file: They need to be placed as filename of .env with corresponding docker application.

- TODO: Probably we might need to implement some shell script to place those scripts as well.

# Some helpful documentations
* Installing Docker to Fedora https://docs.docker.com/engine/install/fedora/

  - You can just use dnf to install docker. But the docker official repository provides much newer version.

* Installing Docker Compose
  https://docs.docker.com/compose/install/
  
  - Super duper useful tool when installing a Docker stack!!

* Installing nvidia-docker2 to Fedora
  https://www.if-not-true-then-false.com/2020/install-nvidia-container-toolkit-on-fedora/
  
  - Note that Fedora isn't officially supported by NVIDIA for the NVIDIA docker container. Therefore, we need a bit of hacking... especially the _/etc/nvidia-container-runtime/config.toml_.
  

