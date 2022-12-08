#!/usr/bin/awk -f

function fs_index(dirs, dir_n,    fsi, i) {
    for (i = 1; i <= dir_n; i++) {
        fsi = sprintf("%s%s%s", fsi, SUBSEP, dirs[i])
    }

    return fsi
}

function add_size(fs, dirs, dir_n, size,    i) {
    for (i = 1; i <= dir_n; i++) {
        fsi = fs_index(dirs, i)

        fs[fsi]["_size"] += size
    }
}

BEGIN {
    root_dir = "root"

    dir = root_dir
    dir_n = 0
}

/^\$ cd [^\.]+/ {
    dir_n++
    dir = $3
    dirs[dir_n] = dir
}

/^\$ cd \.\./ {
    delete dirs[dir_n]
    dir_n--
    dir = dirs[dir_n]
}

/^dir .+/ {
    dir_n++
    dirs[dir_n] = $2
    fsi = fs_index(dirs, dir_n)
    delete dirs[dir_n]
    dir_n--

    fs[fsi][_size] = 0
}

/^[0-9]+/ {
    fsi = fs_index(dirs, dir_n)
    add_size(fs, dirs, dir_n, $1)
}

END {
    must_free = fs[SUBSEP "/"]["_size"] - 40000000
    closest = 70000000

    for (key in fs) {
        size = fs[key]["_size"]

        if (size < 100000) {
            gsub(SUBSEP, ":", key)
            summed = sprintf("%s, %s", summed, key)
            sum_1 += size
        }
    
        if (must_free <= size && size < closest) {
            gsub(SUBSEP, ":", key)
            closest_dir = key
            closest = size
        }
    }

    gsub(/^, /, "", summed)

    printf "%d (%s)\n", sum_1, summed
    printf "%d (%s)\n", closest, closest_dir
}

