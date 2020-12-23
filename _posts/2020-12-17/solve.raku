#!/usr/bin/env raku
use v6;

my %coords = load_coords;

say 'After 6 iterations we get ', iterate(%coords, 6).elems, ' active cells.';

# Debug for checking (e.g.) the parsed coordinates
sub say_map(%m) {
    for %m.kv -> $key, $val {
        say "$key: $val";
    }
}

sub iterate(%coords, $n) {
    for 1..$n {
        my %new_coords;
        my %bounds = get_bounds %coords;
        iter_3d %bounds, -> $x, $y, $z {
            my $xyz = to_coord_key $x, $y, $z;
            my $active = %coords{$xyz} || False;
            my $neighbors = neighbor_count $xyz, %coords;

            if $active && ($neighbors < 2 || 3 < $neighbors) {
                $active = False;
            }
            elsif !$active && $neighbors == 3 {
                $active = True;
            }

            if $active {
                %new_coords{$xyz} = True;
            }
        }
        %coords = %new_coords;
    }
    return %coords;
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

sub neighbor_count($xyz, %coords) {
    my $sum = 0;
    for neighbors $xyz -> $n {
        if %coords{$n}:exists {
            $sum += 1;
        }
    }
    return $sum;
}

sub neighbors($xyz) {
    my @neighbors;
    my %coords = from_coord_key $xyz;
    my ($x, $y, $z) = (%coords{'x'}, %coords{'y'}, %coords{'z'});

    for ($x - 1)..($x + 1) -> $n_x {
        for ($y - 1)..($y + 1) -> $n_y {
            for ($z - 1)..($z + 1) -> $n_z {
                my $n = to_coord_key $n_x, $n_y, $n_z;
                @neighbors.push($n) if $n ne $xyz;
            }
        }
    }
    return @neighbors;
}

sub get_bounds(%coords) {
    my %bounds;
    for %coords.keys -> $xyz {
        for from_coord_key($xyz).kv -> $d, $val {
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

sub from_coord_key($xyz) {
    my ($x, $y, $z) = $xyz.split(COORD_DELIM);
    return {x => $x, y => $y, z => $z};
}

sub to_coord_key($x, $y, $z) {
    return join(COORD_DELIM, $x, $y, $z);
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
                %coords{"$x,$y,0"} = True;
            }
            $y += 1;
        }
        $x += 1;
    }
    return %coords;
}

