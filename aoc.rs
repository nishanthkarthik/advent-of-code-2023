pub mod aoc {
    use std::error::Error;
    use std::fmt::Display;
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
}
