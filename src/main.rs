
use std::env::set_var;
const GAMES: &str = include_str!("../answers.txt");
fn main() {

    set_var("RUST_BACKTRACE", "1");

    
    let w = solvit::Wordle::new();

    for answer in GAMES.split_whitespace() {
       
        let guesser = solvit::naive::Naive::new();
        if let Some(score) = w.play(answer, guesser) {
            println!(" guessed '{}' in {}", answer, score);
        }
        else {
            println!("failed to guess");
        }
    }
}
