# TMPDIR=/mnt/ramdisk podman build --security-opt=seccomp=unconfined -t localhost/ende-dev:trixie-slim .
FROM debian:trixie-slim

RUN apt-get update \
 && apt-get install -y --no-install-recommends \
      build-essential make git curl \
      python3 python3-pip python3-venv ca-certificates \
      emacs-nox \
 && rm -rf /var/lib/apt/lists/*

# Install the same ELPA package into the directory used by `make test`.
RUN emacs -Q --batch \
      --eval '(require (quote package))' \
      --eval '(setq package-user-dir "/root/.emacs.d/elpa")' \
      --eval '(package-refresh-contents)' \
      --eval '(package-install (quote websocket))'

WORKDIR /workspace