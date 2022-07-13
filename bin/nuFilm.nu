#!/usr/bin/env nu

ls
| where type == file
| par-each {
    insert duration (
        ffprobe -v quiet -print_format json -show_entries format=duration $in.name
        | from json
        | get format.duration
        | into int
        | into string
        | sed 's/$/sec/'
        | into duration
    )
}
| select name size duration
| sort-by duration
| save /tmp/films.json
