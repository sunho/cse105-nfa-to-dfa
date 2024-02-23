use std::{collections::{HashMap}, marker::PhantomData};
use serde::{Deserialize, Serialize};
use crate::fa::*;

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

