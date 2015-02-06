#!bin/sh

# Extract dbPath and port number
FILE='../DBCredentials.dat'
exec 3<&0
exec 0<$FILE
read line
read line
read line
port=$line
read line
host=$line
exec 0<&3

# Start DB
mongo -port $port -host $host
