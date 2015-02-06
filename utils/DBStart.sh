#!bin/sh

# Extract dbPath and port number
FILE='../DBCredentials.dat'
exec 3<&0
exec 0<$FILE
read line
read line
dbpath=$line
read line
port=$line
exec 0<&3

# Start DB
mongod --dbpath $dbpath --port $port
