use std::error::Error;
use aoclib_rs::aoc::*;

use nom::*;

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
enum Cell {
    Empty,
    Symbol,
    Gear,
    Value(i32),
}

type Ix = Ixn<2>;

#[derive(Debug)]
struct Grid {
    nums: Vec<(Cell, Vec<Ix>)>,
    dim: i32,
    cells: std::collections::BTreeMap<Ix, Cell>,
}

fn ixf(el: [i32; 2]) -> Ix { Ixn::<2>(el) }

fn parse(i: &str) -> Parsed<Grid> {
    fn cell(j: &str) -> Parsed<Cell> {
        let digit = combinator::map(
            character::complete::satisfy(|c| c.is_ascii_digit()),
            |c: char| c.to_digit(10).unwrap());

        let symbol = character::complete::satisfy(|c| c.is_ascii_graphic());

        branch::alt((
            combinator::value(Cell::Empty, character::complete::char('.')),
            combinator::value(Cell::Gear, character::complete::char('*')),
            combinator::map(digit, |v| Cell::Value(v as i32)),
            combinator::value(Cell::Symbol, symbol),
        ))(j)
    }

    fn line(j: &str) -> Parsed<Vec<(usize, Cell)>> {
        combinator::map(multi::many1(cell),
                        |vs| vs.iter().copied().enumerate().collect())(j)
    }

    fn coalesce(vs: Vec<(usize, Cell)>) -> Vec<(Cell, Vec<i32>)> {
        let mut rs = Vec::<(Cell, Vec<i32>)>::new();
        for (j, cell) in vs {
            match cell {
                Cell::Value(t) => match rs.last_mut() {
                    Some((Cell::Value(ref mut n), ref mut ixs)) => {
                        ixs.push(j as i32);
                        *n = *n * 10 + t;
                    }
                    _ => rs.push((cell, vec![j as i32]))
                }
                _ => rs.push((cell, vec![j as i32]))
            }
        }
        rs
    }

    let (i, lines) = multi::separated_list1(character::complete::newline,
                                            combinator::map(line, coalesce))(i)?;

    let nums: Vec<(Cell, Vec<Ix>)> = lines.iter()
        .enumerate()
        .flat_map(|(i, row)|
            row.iter()
                .map(move |(c, js)|
                    (*c, std::iter::repeat(i as i32).zip(js).map(|(a, b)| Ixn([a, *b])).collect())))
        .filter(|(v, _)| *v != Cell::Empty).collect();
    let dim = lines.len() as i32;
    let cells = nums.iter()
        .flat_map(|(c, ixs)|
            ixs.iter().copied().zip(std::iter::repeat(*c))).collect();
    Ok((i, Grid { dim, nums, cells }))
}

fn solve1(Grid { dim, nums, cells }: &Grid) -> i32 {
    let valid_ix = |ix: &Ix| ix.in_range(&Ix::from(0), &Ix::from(dim - 1));

    let has_gear = |ix: &Ix| valid_ix(ix)
        && cells.get(ix).map(|v| *v == Cell::Symbol || *v == Cell::Gear)
        .unwrap_or(false);

    nums.iter()
        .filter(|(c, _)| matches!(c, Cell::Value(_)))
        .filter(|(_, ixs)| {
            let left: Ix = ixf([0, -1]) + *ixs.first().unwrap();
            let right: Ix = ixf([0, 1]) + *ixs.last().unwrap();
            let sides = [left, right];
            let up = ixs.iter().copied().chain(sides).map(|ix| ixf([-1, 0]) + ix);
            let down = ixs.iter().copied().chain(sides).map(|ix| ixf([1, 0]) + ix);
            up.chain(down).chain([left, right]).into_iter().any(|e| has_gear(&e))
        })
        .map(|e| if let Cell::Value(n) = e.0 { n } else { 0 })
        .sum()
}

fn solve2(Grid { dim: _, nums, cells }: &Grid) -> i32 {
    nums.iter()
        .filter_map(|(c, ixs)| if matches!(c, Cell::Gear) { Some(ixs[0]) } else { None })
        .filter_map(|ix| {
            let deltas = |x: &Ix| (-1..2)
                .flat_map(|i| (-1..2)
                    .map(move |j| ixf([i, j])))
                .filter(|ix| *ix != Ix::from(0))
                .map(|r| *x + r)
                .filter(|ix| cells.contains_key(ix)).collect::<Vec<_>>();

            let nums_around: std::collections::HashSet<i32> = deltas(&ix).iter()
                .filter_map(|i| match cells.get(&i) {
                    Some(Cell::Value(n)) => Some(*n),
                    _ => None,
                }).collect();
            if nums_around.len() == 2 { Some(nums_around.iter().product::<i32>()) } else { None }
        })
        .sum()
}

fn main() -> Result<(), Box<dyn Error>> {
    let input = parse_input(parse)?;
    println!("{}", solve1(&input));
    println!("{}", solve2(&input));
    Ok(())
}
