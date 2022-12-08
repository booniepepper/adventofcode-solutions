#!/usr/bin/awk -f

function fs_index(dirs, dir_n,    i) {
    fsi = ""

    for (i = 1; i <= dir_n; i++) {
        fsi = sprintf("%s%s%s", fsi, SUBSEP, dirs[i])
    }

    return fsi
}

function print_fs_index(fsi) {
    gsub(/\034/, ":", fsi)
    printf "%s\n", fsi
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

    dirs[dir_n] = dir
    delete dirs[dir_n]
    
    fs["dummy"] = "dummy"
    delete fs["dummy"]
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

    size = $1

    add_size(fs, dirs, dir_n, size)
}

END {
    sum_1 = 0
    
    for (key in fs) {
        size = fs[key]["_size"]
        gsub(/\034/, ":", key)

        if (size < 100000) {
            sum_1 += size
        }
    }

    print sum_1
}

