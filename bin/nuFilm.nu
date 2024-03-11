#!/usr/bin/env nu

ls | where type == file
| where {|it| ($it.name | path parse).extension in [mp4 mkv avi]}
| par-each {
    insert duration (
        ffprobe -v quiet -print_format json -show_entries format=duration $in.name
        | from json
        | get format.duration
        | into int
        | into string
        | $in ++ sec
        | into duration
    )
}
| sort-by duration
| select duration size name
| to text
| save -f /tmp/film_list.films
