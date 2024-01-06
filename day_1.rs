use std::error::Error;

use nom::{character, IResult, combinator, branch, multi, bytes};

type Parsed<'a, T> = IResult<&'a str, T>;

fn digit(i: &str) -> Parsed<u32> {
    combinator::map(|i| character::complete::satisfy(|c| c.is_ascii_digit())(i), |c| c.to_digit(10).unwrap())(i)
}

fn filter_digit(i: &str) -> Parsed<u32> {
    fn drop(j: &str) -> Parsed<u32> {
        let (rest, _) = character::complete::satisfy(|c| c.is_alphanumeric())(j)?;
        filter_digit(rest)
    }
    branch::alt((digit, drop))(i)
}

fn many_digits(i: &str) -> Parsed<Vec<u32>> {
    multi::many1(filter_digit)(i)
}

fn parse1(i: &str) -> Parsed<Vec<u32>> {
    multi::separated_list1(multi::many1(branch::alt((character::complete::newline,
                                                     character::complete::satisfy(|c| c.is_alphabetic())))),
                           combinator::map(many_digits, solve1))(i)
}

fn solve1(result: Vec<u32>) -> u32 {
    if result.is_empty() { 0 } else { 10 * result.first().unwrap() + result.last().unwrap() }
}

fn solve2(result: Vec<u32>) -> u32 {
    if result.is_empty() { 0 } else { 10 * result.last().unwrap() + result.first().unwrap() }
}

fn parse_text_num(i: &str) -> Parsed<u32> {
    let keys: [_; 9] = [(1, "one"), (2, "two"), (3, "three"), (4, "four"), (5, "five"),
        (6, "six"), (7, "seven"), (8, "eight"), (9, "nine")]
        .map(|(n, t)| combinator::value(n, bytes::complete::tag(t)));
    let mut err = None;
    for mut x in keys {
        match x(i) {
            Ok(a) => return Ok(a),
            Err(a) => {
                err = Some(Err(a));
                continue;
            }
        }
    }
    err.unwrap()
}

fn parse2_line(i: &str) -> Parsed<Vec<u32>> {
    let peek: Parsed<char> = character::complete::satisfy(|c| c.is_alphanumeric())(i);
    if let Ok((rest, _)) = peek {
        let (rest, mut ns) = parse2_line(rest).unwrap_or((rest, vec![]));
        match branch::alt((digit, parse_text_num))(i) {
            Ok((_, n)) => ns.push(n),
            Err(..) => {}
        }
        Ok((rest, ns))
    } else {
        Ok((i, vec![]))
    }
}

fn parse2(i: &str) -> Parsed<Vec<u32>> {
    fn sep(j: &str) -> Parsed<char> {
        let (j, _) = bytes::complete::take_while(|c: char| !c.is_ascii_whitespace())(j)?;
        character::complete::newline(j)
    }

    multi::separated_list1(sep, combinator::map(parse2_line, solve2))(i)
}

fn main() -> Result<(), Box<dyn Error>> {
    let in1 = aoclib_rs::aoc::parse_input(parse1)?;
    println!("{}", in1.iter().sum::<u32>());
    let in2 = aoclib_rs::aoc::parse_input(parse2)?;
    println!("{}", in2.iter().sum::<u32>());
    Ok(())
}
