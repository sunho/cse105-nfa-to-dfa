use text_io::read;
use serde_yaml::{self};
use std::io::{self, BufRead};
mod fa;
mod fa_file;
use fa::*;
use fa_file::*;

fn main() {
    println!("Input NFA file and type $ to indicate end of file:");
    let stdin = io::stdin();
    let mut source = String::new();
    loop {
        let mut input: String = String::new();
        stdin.lock().read_line(&mut input).unwrap();
        if input == "$\n" {
            break
        }
        source += &input;
    }
    let nfa_file: NFAFile = serde_yaml::from_str::<NFAFile>(&source).unwrap();
    let nfa = nfa_file.import();
    let dfa = convert_nfa_to_dfa(&nfa);

    let dfa_file = DFAFile::export(&dfa);
    println!("========================");
    println!("Converted to DFA: ");
    println!("========================");
    println!("{}", serde_yaml::to_string(&dfa_file).unwrap());

    let f = std::fs::OpenOptions::new()
        .write(true)
        .create(true)
        .open("dfa.yml")
        .expect("Couldn't open file");
    serde_yaml::to_writer(f, &dfa_file).unwrap();

    loop {
        print!("Type in string to test: ");
        let input: String = read!();   
        let mut run = AutomataRun::new(&nfa);
        run.consume_string(&input);
        if run.is_accepted() {
            println!("Accepted!");
        } else {
            println!("Rejected.");
        }
    }
}
