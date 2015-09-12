URL=http://`boot2docker ip`:3000

curl -i \
    -H "Accept: application/json" \
    -H "Content-Type: application/json" \
    -X POST \
    -d "@json/building1.json" \
    $URL/api/buildings

curl -i \
    -H "Accept: application/json" \
    -H "Content-Type: application/json" \
    -X POST \
    -d "@json/room1.json" \
    $URL/api/buildings/1/rooms


