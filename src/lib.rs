use std::collections::HashSet;
const DICTIONARY: &str = include_str!("../dictionary.txt");
#[allow(dead_code)]

pub struct Wordle {
    dictionary: HashSet<&'static str>,
}

impl Wordle {
    pub fn new() -> Self {
        Self {
            dictionary: HashSet::from_iter(DICTIONARY.lines().map(|line| {
                line.split_once(' ')
                    //.expect("every line is word + space + frequency")
                    .unwrap()
                    .1 // .# is the tuple position desired to be returned.
            })),
        }
    }

    pub fn play<G: Guesser>(&self, answer: &'static str, mut guesser: G) -> Option<usize> {
        // play six rounds where it invokes guesser each round
        let mut history = Vec::new();

        // start at 1 because 1 is first guess
        // let it guess forever so we can see the distrubution of the guesses <not sure what this means
        // but represented with downwards slope.

        //  Wordle only allows 6 guess, we allow more to avoid chopping off the score distribution for stats purposes
        //  DevNotes

        for i in 1..=32 {
            let guess = guesser.guess(&history);

            if guess == answer {
                return Some(i);
            }
            // "lkjlkjdf".to_string();
            // assert!(self.dictionary.contains(&*guess));

            let correctness = Correctness::compute(answer, &guess);
            history.push(Guess {
                word: guess,
                mask: correctness,
            });
        }
        None
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum Correctness {
    /// Green
    Correct,
    /// Yellow
    Misplaced,
    /// Gray
    Wrong,
}

pub struct Guess {
    pub word: String,
    pub mask: [Correctness; 5],
}

impl Guess {
    pub fn matches(&self, word: &str) -> bool {
        // First check greens
        assert_eq!(self.word.len(), 5);
        assert_eq!(word.len(), 5);
        let mut used = [false; 5];
        //
        // `word` is the answer
        //  `guess.word`  is the guess
        // create iterator tuple (( guess, mask),word))
        //
        for (i, ((g, &m), w)) in self
            .word
            .chars()
            .zip(&self.mask)
            .zip(word.chars())
            .enumerate()
        {
            if m == Correctness::Correct {
                if g !=w {
                    return false;
                } else {
                    used[i] = true;
                }
            }    
        }   
            
        for (i, (w, &m)) in self
            .word
            .chars()
            .zip(&self.mask)
            .enumerate() {
        
            
            if m == Correctness::Correct {
                // must be coreect or wee'd have returned in earliear loops
                continue;
            }
            let mut plausible = true;
            if self
                .word
                .chars()
                .zip(&self.mask)
                .enumerate()
                .any(|(j, (g, m))| {
                    if g != w {
                        return false;
                    }
                    if used[j] {
                        return false;
                    }
                    // We're looking for an `w` in `word`, and have found an `w` in the previous guess.
                    // The color of that previous `w` will tell us whether this `w` _might_ be ok.
                    match m {
                        Correctness::Correct => {unreachable!(
                            "all correct guesses should have resulted in return or be used"
                        );
                        },
                        Correctness::Misplaced if j == i => {
                            // `w` was yellow in this same position last time around, which means
                            // `word` _cannot_ be the answer
                            plausible = false;
                            return false;
                        },
                        Correctness::Misplaced => {
                            used[j] = true;
                            return true;
                            
                        },
                        Correctness::Wrong => {
                            // TODO: early return
                            plausible = false;
                            return false;
                        },
                    };
                })
                && plausible 
            { 
                // The character `w`  was yellow in the previous guess
            } else if !plausible {
                return false;
            } else {
                // We have no information about character `w`, so word might still match
            }
        }
        true
    }
}

pub trait Guesser {
    fn guess(&mut self, history: &[Guess]) -> String;
}

impl Correctness {
    fn compute(answer: &'static str, guess: &str) -> [Self; 5] {
        assert_eq!(answer.len(), 5);
        assert_eq!(guess.len(), 5);
        let mut c = [Correctness::Wrong; 5];

        // mark GREEN by checking each i:usize against a and g for a mactch
        for (i, (a, g)) in answer.chars().zip(guess.chars()).enumerate() {
            if a == g {
                // set element in array of 5 to correct for a==g Correct
                c[i] = Correctness::Correct;
            }
        }
        //
        // mark YELLOW by setting true if used (GREEN/YELLOW will both be correct/true
        //
        let mut used = [false; 5];

        for (i, &c) in c.iter().enumerate() {
            if c == Correctness::Correct {
                used[i] = true;
            }
        }
        // Finding Yellow
        for (i, g) in guess.chars().enumerate() {
            // move on where Correct is found, possible adding a deeper logic here
            if c[i] == Correctness::Correct {
                // Already marked as green
                continue;
            }
            // enumerating chars on answer then putting iteration in method any() that returns True
            // validating if  statement or false "if" will ignore or use in else
            // recheck check for match of a == g from the main iteration....
            // WHY??? anywyas if a==g and used[i] false then set true andn return true otherwise return false
            if answer.chars().enumerate().any(|(i, a)| {
                if g == a && used[i] == false {
                    used[i] = true;
                    return true;
                }
                false
            }) {
                //  all any(true)  will be set to Misplaced
                c[i] = Correctness::Misplaced;
                println!("Correctness of {}: {:?}", i, &c[i]);
            }
        }
        c
    }
pub fn patterns () -> impl Iterator<Item = [Self; 5]> {
        itertools::iproduct!(
            [Self::Correct, Self::Misplaced, Self::Wrong],
            [Self::Correct, Self::Misplaced, Self::Wrong],
            [Self::Correct, Self::Misplaced, Self::Wrong],
            [Self::Correct, Self::Misplaced, Self::Wrong],
            [Self::Correct, Self::Misplaced, Self::Wrong]
            ).map(|(a, b, c, d, e)| [a, b, c, d, e])
        }
    
    }


pub mod naive {
    use std::{collections::HashMap};
    use crate::{Guess, Guesser, DICTIONARY, Correctness};

    pub struct Naive {
        remaining: HashMap<&'static str, usize>,
    }

    impl Naive {
        pub fn new() -> Self {
            Naive {
                remaining: HashMap::from_iter(DICTIONARY.lines().map(|line| {
                    let (word, count) = line
                        .split_once(' ')
                        .expect("every line word + space + frequency");

                    let count: usize = count.parse().expect("every count is a number");
                    (word, count)
                })),
            }
        }
    }

    #[derive(Debug, Copy, Clone)]
    struct Candidate {
        word: &'static str,
        goodness: f64,
    }

    impl Guesser for Naive {
        fn guess(&mut self, history: &[Guess]) -> String {
            if let Some(last) = history.last() {
                // TODO: update self.remaining based on history
                self.remaining.retain(|word, _| last.matches(word));
            }
            if history.is_empty() {
                return "tares".to_string();
               
            }
            let remaining_count: usize  = self.remaining.iter().map(|(_,&c)|c).sum();
            let mut best: Option<Candidate> = None;

            for (&word, &_ ) in &self.remaining {
                // considering a world where we _did_ guess `word` and got `patter` and
                // correctness. now, computer what _then_ is left.
                eprintln!("progress");
                let mut sum = 0.0;
                for pattern in Correctness::patterns() {
                   
                   let mut in_pattern_total = 0;
                   for (candidate, count) in &self.remaining {
                
                        let g = Guess {
                            word: word.to_string(),
                            mask: pattern,
                        }; 
                        if g.matches(candidate) {
                            in_pattern_total += count;
                        }
                    }
                    if in_pattern_total == 0 {
                        continue;
                    }
                
                    let p_of_this_pattern = in_pattern_total as f64 / remaining_count as f64; 
                    sum += p_of_this_pattern * p_of_this_pattern.log2();
                }
                
                
                // let p_word = count as f64 / remaining_count as f64;
                let goodness =  - sum;
                if let Some(c) = best {
                    if goodness > c.goodness {
                        eprintln!("{} is better than {} ( {} > {} )", word, c.word, goodness, c.goodness);
                        best = Some(Candidate { word, goodness, });
                    }
                // is this one better
                } else {
                    eprintln!("starting with {} (goodness: {})", word, goodness);
                    best = Some(Candidate { word, goodness, });
                }
            }
            best.unwrap().word.to_string()
        }
    }
}

impl Guesser for fn(history: &[Guess]) -> String {
    fn guess(&mut self, history: &[Guess]) -> String {
        (*self)(history)
    }
}

#[cfg(test)]
macro_rules! guesser {
    (|$history:ident| $impl: block) => {{
        struct G;
        impl $crate::Guesser for G {
            fn guess(&mut self, $history: &[crate::Guess]) -> String {
                $impl
            }
        }
        G
    }};
}

#[cfg(test)]
macro_rules! mask {
        (C) => {$crate::Correctness::Correct};
        (M) => {$crate::Correctness::Misplaced};
        (W) => {$crate::Correctness::Wrong};
        ($($c:tt)+) => {[
            $(mask!($c)),+
            ]}
}

#[cfg(test)]
mod tests { mod guess_matcher {
        use crate::Guess; 

        macro_rules! check {
            ($prev:literal + [$($mask:tt)+] allows $next:literal) => {
                assert!(Guess {
                        word: $prev.to_string(),
                        mask: mask![$($mask )+]

                }
                .matches($next));
                // assert_eq!($crate::Correctness::compute($next, $prev), mask![$($mask )+]);
            };

            ($prev:literal + [$($mask:tt)+] disallows $next:literal) => {
                assert!(!Guess {
                        word: $prev.to_string(),
                        mask: mask![$($mask )+]
            }
            .matches($next));
           
                // assert_ne!($crate::Correctness::compute($next, $prev), mask![$($mask )+]);
           }
        }
       
        #[test]
        fn matches() {
            check!("abcde" + [C C C C C] allows "abcde");
            check!("abcdf" + [C C C C C] disallows "abcde");
            check!("abcde" + [W W W W W] allows "fghij");
            check!("abcde" + [M M M M M] allows "eabcd");
            check!("baaaa" + [W C M W W] allows "aaccc");
            check!("baaaa" + [W C M W W] disallows "caacc");
        }
    }

    mod game {
        use crate::Wordle;

        #[test]
        fn play() {
            let w = Wordle::new();
            let guesser = guesser!(|_history| { "right".to_string() });
            assert_eq!(w.play("right", guesser), Some(1));
        }

        #[test]
        fn play2() {
            // let w = Wordle::new();
            let _guesser = guesser!(|history| {
                if history.len() == 1 {
                    "right".to_string();
                }
                {
                    "wrong".to_string()
                }
            });
            // assert_eq(w.play("right", guesser),Some(1));
        }
    }

    mod compute {

        use crate::Correctness;
        #[test]
        fn all_green() {
            assert_eq!(Correctness::compute("abcde", "abcde"), mask![C C C C C])
        }

        #[test]
        fn all_gray() {
            assert_eq!(
                Correctness::compute("abcde", "fghij"),
                [Correctness::Wrong; 5]
            );
        }
        #[test]
        fn all_yellow() {
            assert_eq!(
                Correctness::compute("abcde", "edbca"),
                [Correctness::Misplaced; 5]
            );
        }
        #[test]
        fn green_yellow_gray() {
            assert_eq!(
                Correctness::compute("abccd", "abcde"),
                [
                    Correctness::Correct,
                    Correctness::Correct,
                    Correctness::Correct,
                    Correctness::Misplaced,
                    Correctness::Wrong
                ]
            );
        }
    }
}