#!/bin/bash
# Como usarlo: ./create_nodes.sh N_NODES PORT

N_NODES=$1
PORT=$2
LOCAL_IP=$(hostname -I)
PIDS=()
NODE=serverN0@$LOCAL_IP

erl -name ${NODE%?} -setcookie galletita -noshell -eval "server:init($PORT)." &
PIDS[0]=$!

for i in $(seq 1 "$(($N_NODES-1))");
do
  #echo "erl -name serverN$i@$LOCAL_IP -setcookie galletita -noshell -eval \"server:init($(($PORT+$i+1)),'${NODE%?}').\" "
	erl -name serverN$i@$LOCAL_IP -setcookie galletita -noshell -eval "server:init($(($PORT+$i)),'${NODE%?}')." &
	PIDS[$i]=$!
done

echo "${PIDS[*]}"


# Para pararlos: killall beam.smp
