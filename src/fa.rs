use std::{collections::{HashMap, HashSet, VecDeque}, marker::PhantomData};

pub type StateID = usize;
pub type StateSet = HashSet<StateID>;

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
pub struct AutomataRun<'a, StateType: Default+Clone, CA: AcceptChecker<StateType>, HT: TransitionHandler<StateType>> {
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
pub struct AcceptCheckerNFA;
impl AcceptChecker<StateSet> for AcceptCheckerNFA {
    // In NFA, string is aceepted if any accepted state could be reached
    fn apply(accepted_states: &StateSet, state: &StateSet) -> bool {
        state.iter().any(|&s| accepted_states.contains(&s))
    }
}

pub struct TransitionHandlerNFA;
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
pub struct AcceptCheckerDFA;
impl AcceptChecker<StateID> for AcceptCheckerDFA {
    fn apply(accepted_state: &StateSet, state: &StateID) -> bool {
        accepted_state.contains(state)
    }
}

pub struct TransitionHandlerDFA;
impl TransitionHandler<StateID> for TransitionHandlerDFA {
    fn apply(transitions: &Vec<HashMap<char,StateID>>, state: &StateID, ch: char) -> StateID {
        transitions[*state][&ch]
    }
}

pub type DFA = FiniteAutomata<StateID, AcceptCheckerDFA, TransitionHandlerDFA>;

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

