#!/bin/bash

mac1 = $(openssl rand -hex 6 | sed 's/\(..\)/\1:/g; s/.$//')
echo "{mac1}"
sudo ifconfig en0 ether MAC1  
ifconfig en0 | grep ether
