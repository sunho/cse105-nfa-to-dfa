use std::{collections::{HashMap, HashSet, VecDeque}, marker::PhantomData, fs};
use serde::{Deserialize, Serialize};
use text_io::read;
use serde_yaml::{self};

type StateID = usize;
type StateSet = HashSet<StateID>;

pub trait AcceptChecker<StateType> {
    fn apply(accepted_states: &StateSet, state: &StateType) -> bool;
}

pub trait TransitionHandler<StateType> {
    fn apply(transitions: &Vec<HashMap<char,StateType>>, state: &StateType, ch: char) -> StateType;
}

// Generic finite automata class that is used to implement NFA and DFA
// StateType is the type of the output of delta transition function and the intermediate state.
// AcceptChecker checks whether state should be accepted based on given F
// TransitionHandler handles transition of intermediate state
pub struct FiniteAutomata<StateType: Default+Clone, CA: AcceptChecker<StateType>, HT: TransitionHandler<StateType>> {
    pub initial_state: StateType,
    pub accepted_states: StateSet,
    pub state_names: Vec<Option<String>>,
    pub transitions: Vec<HashMap<char,StateType>>,
    // needed to avoid rust unused generics parameter error...
    // https://github.com/rust-lang/rust/issues/23246
    dummy_ca: PhantomData<CA>,
    dummy_ht: PhantomData<HT>
}

impl<ST, CA, HT> FiniteAutomata<ST, CA, HT> where ST: Default+Clone, CA: AcceptChecker<ST>, HT: TransitionHandler<ST> {
    pub fn new(num_states: usize) -> Self {
        Self {
            initial_state: Default::default(),
            accepted_states: Default::default(),
            state_names: vec![None;num_states],
            transitions: vec![HashMap::new();num_states],
            dummy_ca: PhantomData,
            dummy_ht: PhantomData
        }
    }

    pub fn add_state(&mut self, name: Option<String>) {
        self.transitions.push(HashMap::new());
        self.state_names.push(name);
    }

    pub fn is_accepted_state(&self, state: &ST) -> bool {
        CA::apply(&self.accepted_states, &state)
    }

    pub fn delta(&self, state: &ST, ch: char) -> ST {
        HT::apply(&self.transitions, &state, ch)
    }

    pub fn size(&self) -> usize {
        self.transitions.len()
    }

    pub fn get_state_name(&self, i: StateID) -> String {
        if let Some(name) = self.state_names[i].clone() {
            name
        } else {
            "state ".to_string() + &i.to_string()
        }
    }

    pub fn calculate_alphabet(&self) -> Vec<char> {
        let mut res = Vec::<char>::new();
        for i in 0usize..self.size() {
            for (k,_) in &self.transitions[i] {
                res.push(*k);
            }
        }
        res.sort();
        res.dedup();
        res
    }
}

// Lightweight class that represents one run of finite automata
// Can consume individual character or string and determine whether string is accepted or not
struct AutomataRun<'a, StateType: Default+Clone, CA: AcceptChecker<StateType>, HT: TransitionHandler<StateType>> {
    parent: &'a FiniteAutomata<StateType, CA, HT>,
    state: StateType
}

impl<'a, ST, CA, HT> AutomataRun<'a, ST, CA, HT> where ST: Default+Clone, CA: AcceptChecker<ST>, HT: TransitionHandler<ST> {
    pub fn new(parent: &'a FiniteAutomata<ST,CA,HT>) -> Self {
        Self {
            parent,
            state: parent.initial_state.clone()
        }
    }

    pub fn consume_string(&mut self, input: &str) {
        for c in input.chars() {
            self.consume_char(c);
        }
    }

    pub fn consume_char(&mut self, input: char) {
        self.state = self.parent.delta(&self.state, input);
    }

    pub fn is_accepted(&self) -> bool {
        self.parent.is_accepted_state(&self.state)
    }
}

// NFA implementation
struct AcceptCheckerNFA;
impl AcceptChecker<StateSet> for AcceptCheckerNFA {
    // In NFA, string is aceepted if any accepted state could be reached
    fn apply(accepted_states: &StateSet, state: &StateSet) -> bool {
        state.iter().any(|&s| accepted_states.contains(&s))
    }
}

struct TransitionHandlerNFA;
impl TransitionHandler<StateSet> for TransitionHandlerNFA {
    fn apply(transitions: &Vec<HashMap<char,StateSet>>, state: &StateSet, ch: char) -> StateSet {
        let mut res = StateSet::new();
        for k in state {
            if !transitions[*k].contains_key(&ch) {
                continue;
            }
            for s in transitions[*k][&ch].iter() {
                res.insert(*s);
            }
        }
        res
    }
}

pub type NFA = FiniteAutomata<StateSet, AcceptCheckerNFA, TransitionHandlerNFA>;

// DFA implementation
struct AcceptCheckerDFA;
impl AcceptChecker<StateID> for AcceptCheckerDFA {
    fn apply(accepted_state: &StateSet, state: &StateID) -> bool {
        accepted_state.contains(state)
    }
}

struct TransitionHandlerDFA;
impl TransitionHandler<StateID> for TransitionHandlerDFA {
    fn apply(transitions: &Vec<HashMap<char,StateID>>, state: &StateID, ch: char) -> StateID {
        transitions[*state][&ch]
    }
}

pub type DFA = FiniteAutomata<StateID, AcceptCheckerDFA, TransitionHandlerDFA>;

pub trait LoadableStateType<DstType> {
    fn save_state_type(dst: &DstType, cvt: &Vec<Option<String>>) -> Self;
    fn load_state_type(&self, cvt: &HashMap<String, StateID>) -> DstType;
}

// Finite automata file implementation
#[derive(Serialize, Deserialize, PartialEq)]
pub struct FAFile<StateType: Default+Clone, LoadStateType: LoadableStateType<StateType>+Default, CA: AcceptChecker<StateType>, HT: TransitionHandler<StateType>> {
    alphabet: Vec<String>,
    states: Vec<String>,
    accept: Vec<String>,
    init: LoadStateType,
    transitions: HashMap<String, HashMap<String, LoadStateType>>,
    #[serde(skip)]
    dummy_st: PhantomData<StateType>,
    #[serde(skip)]
    dummy_ca: PhantomData<CA>,
    #[serde(skip)]
    dummy_ht: PhantomData<HT>
}

impl<ST, LST, CA, HT> FAFile<ST, LST, CA, HT> where ST: Default+Clone, LST: LoadableStateType<ST>+Default, CA: AcceptChecker<ST>, HT: TransitionHandler<ST> {
    pub fn import(&self) -> FiniteAutomata<ST,CA,HT> {
        let n = self.states.len();
        let mut res = FiniteAutomata::<ST,CA,HT>::new(n);
        let mut cvt = HashMap::<String, StateID>::new();
        for i in 0..self.states.len() {
            res.state_names[i] = Some(self.states[i].clone());
            cvt.insert(self.states[i].clone(),i);
        }
        res.initial_state = self.init.load_state_type(&cvt);
        for s in &self.accept {
            res.accepted_states.insert(cvt[s]);
        }
        for (k,v) in &self.transitions {
            let state = cvt[k];
            for (c, d) in v {
                assert_eq!(c.len(), 1);
                res.transitions[state].insert(c.chars().nth(0).unwrap(), d.load_state_type(&cvt));
            }
        }
        res
    }

    pub fn export(fa: &FiniteAutomata<ST,CA,HT>) -> Self {
        let mut res = Self {
            alphabet: Vec::<String>::new(),
            states: vec![String::new();fa.size()],
            accept: Vec::<String>::new(),
            init: Default::default(),
            transitions: Default::default(),
            dummy_st: PhantomData,
            dummy_ca: PhantomData,
            dummy_ht: PhantomData
        };
        for i in 0..fa.size() {
            res.states[i] = fa.get_state_name(i);
        }
        for c in fa.calculate_alphabet() {
            res.alphabet.push(c.to_string());
        }
        res.init = LST::save_state_type(&fa.initial_state, &fa.state_names);
        for s in &fa.accepted_states {
            res.accept.push(fa.get_state_name(*s));
        }
        for i in 0..fa.size() {
            if fa.transitions[i].is_empty() {
                continue;
            }
            for (k,v) in &fa.transitions[i] {
                res.transitions.entry(fa.get_state_name(i))
                    .or_insert(Default::default())
                    .insert(k.to_string(), LST::save_state_type(v, &fa.state_names));
            }
        }
        res
    }
}

impl LoadableStateType<StateSet> for Vec<String> {
    fn save_state_type(dst: &StateSet, cvt: &Vec<Option<String>>) -> Self {
        let mut res = Vec::<String>::new();
        for s in dst {
            res.push(cvt[*s].clone().unwrap_or("state ".to_string() + &s.to_string()));
        }
        res
    }
    fn load_state_type(&self, cvt: &HashMap<String,StateID>) -> StateSet {
        let mut res = StateSet::new();
        for s in self {
            res.insert(cvt[s]);
        }
        res
    }
}

impl LoadableStateType<StateID> for String {
    fn save_state_type(dst: &StateID, cvt: &Vec<Option<String>>) -> Self {
        cvt[*dst].clone().unwrap_or("state ".to_string() + &dst.to_string())
    }
    fn load_state_type(&self, cvt: &HashMap<String,StateID>) -> StateID {
        cvt[self]
    }
}

pub type NFAFile = FAFile<StateSet, Vec<String>, AcceptCheckerNFA, TransitionHandlerNFA>;

pub type DFAFile = FAFile<StateID, String, AcceptCheckerDFA, TransitionHandlerDFA>;

// NFA to DFA conversion implementation
pub fn convert_nfa_to_dfa(nfa: &NFA) -> DFA {
    let n = 1usize << nfa.size();
    let mut res = DFA::new(n);
    for s in &nfa.initial_state {
        res.initial_state |= 1usize << s;
    }
    // Do BFS on state graph to figure out all possible transitions
    let cvt = |state: &StateSet| -> StateID {
        let mut res = 0usize;
        for s in state {
            res |= 1usize << s;
        }
        res
    };
    let alphabet = nfa.calculate_alphabet();
    let mut visited = vec![false;n];
    let mut queue = VecDeque::<StateID>::new();
    queue.push_back(res.initial_state);
    visited[res.initial_state] = true;
    while !queue.is_empty() {
        let cur = queue.pop_front().unwrap();
        for c in &alphabet {
            let mut nxt = 0usize;
            for s in 0..nfa.size() {
                if (cur >> s & 1) == 1 && nfa.transitions[s].contains_key(c) {
                    nxt |= cvt(&nfa.transitions[s][c]);
                }
            }
            res.transitions[cur].insert(*c, nxt);
            if !visited[nxt] {
                visited[nxt] = true;
                queue.push_back(nxt);
            }
        }
    }

    // Accept state is when at least one of mask is in accept state of NFA
    for msk in 0usize..n {
        for s in &nfa.accepted_states {
            if (msk >> s & 1) == 1 {
                res.accepted_states.insert(msk);
                continue;
            }
        }
    }

    // Translate state names
    for msk in 0usize..n {
        let mut str = String::new();
        for i in 0usize..nfa.size() {
            if (msk >> i & 1) == 1 {
                if !str.is_empty() {
                    str += " | ";
                }
                str += &nfa.get_state_name(i);
            }
        }
        res.state_names[msk] = Some(str);
    }
    res
}

fn main() {
    let contents = fs::read_to_string("./test.yml")
        .expect("Should have been able to read the file");
    let nfa_file: NFAFile = serde_yaml::from_str::<NFAFile>(&contents).unwrap();
    let mut nfa = nfa_file.import();
    let mut dfa = convert_nfa_to_dfa(&nfa);
    let input: String = read!();   
    let mut run = AutomataRun::new(&dfa);
    run.consume_string(&input);
    if run.is_accepted() {
        println!("AC");
    } else {
        println!("WA");
    }

    let dfa_file = DFAFile::export(&dfa);
    let f = std::fs::OpenOptions::new()
        .write(true)
        .create(true)
        .open("dfa.yml")
        .expect("Couldn't open file");
    serde_yaml::to_writer(f, &dfa_file).unwrap();
}
