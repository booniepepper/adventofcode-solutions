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

fn main() {
    let input = load_input();
    let solution_one = fix(change_seats, input)
        .into_iter()
        .flatten()
        .filter_map(|seat| match seat {
            Some(Full) => Some(Full),
            _ => None,
        })
        .count();
    println!("Found {} full seats when it settled.", solution_one);
}

// https://en.wikipedia.org/wiki/Fixed-point_combinator
// More accessible in SICP: https://mitpress.mit.edu/sites/default/files/sicp/index.html
fn fix<A: PartialEq + Clone>(f: impl Fn(&A) -> A, initial: A) -> A {
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

fn change_seats(seats: &SparseMatrix<Seat>) -> SparseMatrix<Seat> {
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
