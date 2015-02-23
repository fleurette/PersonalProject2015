% Load client
MongoStart;

% Load database credentials
credentials = 'dbCredentials.dat';
fid = fopen(credentials);
fgetl(fid);
fgetl(fid);
port = fgetl(fid);
host = strcat(fgetl(fid),':',port);
dbName = fgetl(fid);
rawData = strcat(dbName,'.',fgetl(fid));
maleData = strcat(dbName,'.',fgetl(fid));
femaleData = strcat(dbName,'.',fgetl(fid));
users = strcat(dbName,'.',fgetl(fid));
fclose(fid);
disp('Read credentials from the database')

% Connect to the database extract collections
client = Mongo(host)

% BuildQuery
bb = BsonBuffer
bb.append('','')
findAllQuery = bb.finish()

% Get collections
cursor = MongoCursor(query)


% Shut down the datbase
%clearvars
%MongoStop
