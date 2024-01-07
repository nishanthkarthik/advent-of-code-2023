pub mod aoc {
    use std::error::Error;
    use std::fmt::{Display, Formatter};
    use std::hash::{Hash, Hasher};
    use std::io::Read;

    #[derive(Debug, Clone)]
    struct InputMissing;

    impl Display for InputMissing {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.write_str("Missing input file")
        }
    }

    impl Error for InputMissing {}

    pub fn read_input() -> Result<String, Box<dyn Error>> {
        let arg = std::env::args().skip(1).next().ok_or(InputMissing)?;
        let path = std::path::Path::new(&arg);
        let mut file = std::fs::File::open(path)?;
        let mut buffer = String::new();
        file.read_to_string(&mut buffer)?;
        Ok(buffer)
    }

    pub fn parse_input<Out>(parser: fn(&str) -> nom::IResult<&str, Out>) -> Result<Out, Box<dyn Error>> {
        let input = read_input()?;
        let out = parser(&input).
            map(|e| e.1)
            .map_err(|e| e.to_owned())?;
        Ok(out)
    }

    #[derive(Eq, PartialEq, PartialOrd, Ord, Copy, Clone)]
    pub struct Ixn<const N: usize>(pub [i32; N]);

    impl<const N: usize> Hash for Ixn<N> {
        fn hash<H: Hasher>(&self, state: &mut H) {
            self.0.iter().for_each(|e| state.write_i32(*e))
        }
    }

    impl<const N: usize> Ixn<N> {
        pub fn in_range(&self, lbound: &Self, rbound: &Self) -> bool {
            self.0
                .iter()
                .zip(lbound.0.iter().zip(rbound.0.iter()))
                .all(|(i, (l, r))| l <= i && i <= r)
        }
    }

    impl<const N: usize> std::fmt::Debug for Ixn<N> {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            f.write_fmt(format_args!("({},{})", self.0[0], self.0[1]))
        }
    }

    impl<const N: usize> std::ops::Add for Ixn<N> {
        type Output = Self;

        fn add(mut self, rhs: Self) -> Self::Output {
            self.0.iter_mut().zip(rhs.0).for_each(|(l,r)| *l += r);
            self
        }
    }

    impl<T: Into<i32>, const N: usize> From<T> for Ixn<N> {
        fn from(value: T) -> Self {
            Ixn([value.into(); N])
        }
    }

    pub type Parsed<'a, T> = nom::IResult<&'a str, T>;
}
