// Run with `cargo run`
use std::convert::TryInto;
use std::fs;
use std::ops::Deref;

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
enum Seat {
    Empty,
    Full,
}
use Seat::*;

type SparseMatrix<A> = Vec<Vec<Option<A>>>;

// Note: This is particularly messy, I should come back when I've got time.
fn main() {
    let input = load_input();
    println!(
        "Found {} full seats when it settled (v1).",
        count_full_seats(fix(change_seats_v1, &input))
    );
    println!(
        "Found {} full seats when it settled (v2).",
        count_full_seats(fix(change_seats_v2, &input))
    );
}

fn count_full_seats(seats: SparseMatrix<Seat>) -> usize {
    seats
        .into_iter()
        .flatten()
        .filter_map(|seat| match seat {
            Some(Full) => Some(Full),
            _ => None,
        })
        .count()
}

fn change_seats_v1(seats: &SparseMatrix<Seat>) -> SparseMatrix<Seat> {
    //eprintln!("=== seats ===\n{}", debug_view_of_seats(seats));
    deep_enum_map(seats, |cell, (i, j)| {
        let ns = neighbors(seats, i, j);
        match cell {
            Empty => {
                if ns.into_iter().all(|n| n == Empty) {
                    Full
                } else {
                    *cell
                }
            }
            Full => {
                if ns.into_iter().filter(|n| *n == Full).count() >= 4 {
                    Empty
                } else {
                    *cell
                }
            }
        }
    })
}

fn neighbors(seats: &SparseMatrix<Seat>, row: usize, col: usize) -> Vec<Seat> {
    let row: i64 = row.try_into().unwrap();
    let col: i64 = col.try_into().unwrap();
    let r_bound: i64 = seats.len().try_into().unwrap();
    let c_bound: i64 = seats[0].len().try_into().unwrap();
    [
        (-1, -1),
        (-1, 0),
        (-1, 1),
        (0, -1),
        (0, 1),
        (1, -1),
        (1, 0),
        (1, 1),
    ]
    .iter()
    .map(|(r_off, c_off)| (r_off + row, c_off + col))
    .filter(|(row, col)| 0 <= *row && *row < r_bound && 0 <= *col && *col < c_bound)
    .filter_map(|(row, col)| seats[row as usize][col as usize])
    .collect()
}

fn change_seats_v2(seats: &SparseMatrix<Seat>) -> SparseMatrix<Seat> {
    //eprintln!("=== seats ===\n{}", debug_view_of_seats(seats));
    let width = seats.len();
    let height = seats[0].len();
    deep_enum_map(seats, |cell, (i, j)| {
        let ns = line_of_sight(seats, i, j, width, height);
        match cell {
            Empty => {
                if ns.into_iter().all(|n| n == Empty) {
                    Full
                } else {
                    *cell
                }
            }
            Full => {
                if ns.into_iter().filter(|n| *n == Full).count() >= 5 {
                    Empty
                } else {
                    *cell
                }
            }
        }
    })
}

fn line_of_sight(
    seats: &SparseMatrix<Seat>,
    row: usize,
    col: usize,
    width: usize,
    height: usize,
) -> Vec<Seat> {
    let row: i64 = row.try_into().unwrap();
    let col: i64 = col.try_into().unwrap();
    let r_bound: i64 = width.try_into().unwrap();
    let c_bound: i64 = height.try_into().unwrap();
    [
        (-1, -1),
        (-1, 0),
        (-1, 1),
        (0, -1),
        (0, 1),
        (1, -1),
        (1, 0),
        (1, 1),
    ]
    .iter()
    .filter_map(|(r_off, c_off)| {
        let mut r = row + r_off;
        let mut c = col + c_off;
        let ptr_in_bounds = |r, c| 0 <= r && r < r_bound && 0 <= c && c < c_bound;
        let seat = |r, c| seats[r as usize][c as usize];
        while ptr_in_bounds(r, c) && seat(r, c).is_none() {
            r += r_off;
            c += c_off;
        }
        if ptr_in_bounds(r, c) {
            seat(r, c)
        } else {
            None
        }
    })
    .collect()
}

// https://en.wikipedia.org/wiki/Fixed-point_combinator
// More accessible in SICP: https://mitpress.mit.edu/sites/default/files/sicp/index.html
fn fix<A: PartialEq + Clone>(f: impl Fn(&A) -> A, initial: &A) -> A {
    let mut prev = Box::new(initial.clone());
    let mut curr = Box::new(f(&initial));
    while curr != prev {
        prev = curr;
        curr = Box::new(f(&prev));
    }
    curr.deref().clone()
}

fn deep_enum_map<A, F>(matrix: &SparseMatrix<A>, transform: F) -> SparseMatrix<A>
where
    F: Fn(&A, (usize, usize)) -> A,
{
    matrix
        .iter()
        .enumerate()
        .map(|(i, row)| {
            row.iter()
                .enumerate()
                .map(|(j, cell)| match cell {
                    None => None,
                    Some(value) => Some(transform(value, (i, j))),
                })
                .collect()
        })
        .collect()
}

fn load_input() -> SparseMatrix<Seat> {
    fs::read_to_string("./input")
        .unwrap()
        .lines()
        .map(|line| {
            line.chars()
                .map(|c| match c {
                    'L' => Some(Empty),
                    _ => None,
                })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>()
}

fn debug_view_of_seats(seats: &SparseMatrix<Seat>) -> String {
    seats
        .iter()
        .map(|row| {
            row.iter()
                .map(|seat| match seat {
                    None => ' ',
                    Some(Empty) => 'L',
                    Some(Full) => 'X',
                })
                .fold(String::new(), |mut s, c| {
                    s.push(c);
                    s
                })
        })
        .fold(String::new(), |mut a, b| {
            a.push_str("\n");
            a.push_str(&b);
            a
        })
}
