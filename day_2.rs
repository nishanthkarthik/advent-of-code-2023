use std::error::Error;

use nom::*;

type Parsed<'a, T> = IResult<&'a str, T>;

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum Color {
    Red,
    Green,
    Blue,
}

type Draw = [i32; 3];

#[derive(Debug)]
struct Game {
    id: i32,
    draws: Vec<Draw>,
}

fn parse(i: &str) -> Parsed<Vec<Game>> {
    fn color(j: &str) -> Parsed<Color> {
        branch::alt((combinator::value(Color::Red, bytes::complete::tag("red")),
                     combinator::value(Color::Green, bytes::complete::tag("green")),
                     combinator::value(Color::Blue, bytes::complete::tag("blue"))))(j)
    }

    fn draw_item(j: &str) -> Parsed<(Color, i32)> {
        let (j, n) = character::complete::i32(j)?;
        let (j, _) = character::complete::space1(j)?;
        let (j, c) = color(j)?;
        Ok((j, (c, n)))
    }

    fn draw(j: &str) -> Parsed<Draw> {
        let (j, items) = multi::separated_list1(bytes::complete::tag(", "),
                                                draw_item)(j)?;
        let r = |c| items.iter()
            .find(|e| e.0 == c)
            .map(|e| e.1).unwrap_or(0);
        Ok((j, [r(Color::Red), r(Color::Green), r(Color::Blue)]))
    }

    fn line(j: &str) -> Parsed<Game> {
        let (j, _) = bytes::complete::tag("Game ")(j)?;
        let (j, id) = character::complete::i32(j)?;
        let (j, _) = bytes::complete::tag(": ")(j)?;
        let (j, draws) = multi::separated_list1(bytes::complete::tag("; "), draw)(j)?;
        Ok((j, Game { id, draws }))
    }

    multi::separated_list1(character::complete::newline, line)(i)
}

fn main() -> Result<(), Box<dyn Error>> {
    let input = aoclib_rs::aoc::parse_input(parse)?;
    let bag_size = [12, 13, 14];
    let valid_draw =
        |xs: &Draw| xs
            .iter()
            .zip(bag_size).all(|(x, b)| b >= *x);
    let valid_games = input.iter()
        .filter(|Game { id: _, draws }| draws.iter().all(|e| valid_draw(e)));
    let solve1: i32 = valid_games.map(|g| g.id).sum();
    println!("{solve1}");
    let min_cubes =
        |draws: &Vec<Draw>| draws
            .iter()
            .fold([0,0,0], |[a,b,c],[p,q,r]|
                                  [i32::max(a, *p), i32::max(b, *q), i32::max(c, *r)]);
    let power = |game: &Game| min_cubes(&game.draws).iter().product::<i32>();
    let solve2 = input.iter().map(power).sum::<i32>();
    println!("{solve2}");
    Ok(())
}
