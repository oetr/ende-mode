# TMPDIR=/mnt/ramdisk podman build --security-opt=seccomp=unconfined -t localhost/ende-dev:trixie-slim .
FROM debian:trixie-slim

RUN apt-get update \
 && apt-get install -y --no-install-recommends \
      emacs-nox build-essential make git curl \
      python3 python3-pip python3-venv ca-certificates \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /workspace