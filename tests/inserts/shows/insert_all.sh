# Inserts all shows into the DB

shows=$(ls *.json)

for show in $shows;
do
    CMD="curl -XPOST -H 'Content-Type: application/json' -d @$show localhost:8080/shows\n"
    echo "$CMD"
    curl -XPOST -H 'Content-Type: application/json' -d @$show localhost:8080/shows
done

