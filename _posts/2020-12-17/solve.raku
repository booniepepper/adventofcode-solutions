#!/usr/bin/env raku
use v6;

say 'After 6 generations (3d): ', transform_3d(load_coords, 6).elems, ' active cells.';
say 'After 6 generations (4d): ', transform_4d(load_coords, 6).elems, ' active cells.';

# Debug for checking (e.g.) the parsed coordinates
sub say_map(%m) {
    for %m.kv -> $key, $val {
        say "$key: $val";
    }
}

sub transform_4d(%coords, $n) {
    for 1..$n {
        my %new_coords;
        my %bounds = get_bounds %coords;
        iter_4d %bounds, -> $x, $y, $z, $w {
            my $xyzw = to_coord_key $x, $y, $z, $w;
            if next_cell_state $xyzw, %coords {
                %new_coords{$xyzw} = True;
            }
        }
        %coords = %new_coords;
    }
    return %coords;
}

sub transform_3d(%coords, $n) {
    for 1..$n {
        my %new_coords;
        my %bounds = get_bounds %coords;
        iter_3d %bounds, -> $x, $y, $z {
            my $xyz = to_coord_key $x, $y, $z;
            if next_cell_state $xyz, %coords {
                %new_coords{$xyz} = True;
            }
        }
        %coords = %new_coords;
    }
    return %coords;
}

sub next_cell_state($xyzw, %coords) {
    my $active = %coords{$xyzw} || False;
    my $neighbors = neighbor_count $xyzw, %coords;

    if $active && ($neighbors < 2 || 3 < $neighbors) {
        $active = False;
    }
    elsif !$active && $neighbors == 3 {
        $active = True;
    }

    return $active;
}

sub iter_4d(%bounds, &f) {
    for minmax_as_range %bounds{'x'} -> $x {
        for minmax_as_range %bounds{'y'} -> $y {
            for minmax_as_range %bounds{'z'} -> $z {
                for minmax_as_range %bounds{'w'} -> $w {
                    &f($x, $y, $z, $w);
                }
            }
        }
    }
}

sub iter_3d(%bounds, &f) {
    for minmax_as_range %bounds{'x'} -> $x {
        for minmax_as_range %bounds{'y'} -> $y {
            for minmax_as_range %bounds{'z'} -> $z {
                &f($x, $y, $z);
            }
        }
    }
}
sub neighbor_count($xyzw, %coords) {
    my $sum = 0;
    for neighbors $xyzw -> $n {
        if %coords{$n}:exists {
            $sum += 1;
        }
    }
    return $sum;
}

sub neighbors($xyzw) {
    my @neighbors;
    my %coords = from_coord_key $xyzw;
    my ($x, $y, $z, $w) = (
        %coords{'x'},
        %coords{'y'},
        %coords{'z'},
        %coords{'w'}
    );

    for ($x - 1)..($x + 1) -> $n_x {
        for ($y - 1)..($y + 1) -> $n_y {
            for ($z - 1)..($z + 1) -> $n_z {
                for ($w - 1)..($w + 1) -> $n_w {
                    my $n = to_coord_key $n_x, $n_y, $n_z, $n_w;
                    @neighbors.push($n) if $n ne $xyzw;
                }
            }
        }
    }
    return @neighbors;
}

sub get_bounds(%coords) {
    my %bounds;
    for %coords.keys -> $xyzw {
        for from_coord_key($xyzw).kv -> $d, $val {
            if %bounds{$d}{'min'}:!exists || $val < %bounds{$d}{'min'} {
                %bounds{$d}{'min'} = $val;
            }
            if %bounds{$d}{'max'}:!exists || $val > %bounds{$d}{'max'} {
                %bounds{$d}{'max'} = $val;
            }
        }
    }
    return %bounds;
}

sub minmax_as_range(%minmax) {
    return (%minmax{'min'} - 1)..(%minmax{'max'}+1);
}

constant COORD_DELIM = ',';

sub from_coord_key($xyzw) {
    my ($x, $y, $z, $w) = $xyzw.split(COORD_DELIM);
    return {x => $x, y => $y, z => $z, w => $w};
}

sub to_coord_key($x, $y, $z=0, $w=0) {
    return join(COORD_DELIM, $x, $y, $z, $w);
}

sub load_coords {
    my %coords;
    my $input = open 'input';
    my $x = 0;
    for $input.lines -> $line {
        next if $line.chars == 0;
        my $y = 0;
        for $line.split('') -> $cell {
            next if $cell.chars == 0;
            if $cell ~~ '#' {
                %coords{to_coord_key($x, $y)} = True;
            }
            $y += 1;
        }
        $x += 1;
    }
    return %coords;
}

