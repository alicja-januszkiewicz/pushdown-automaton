#![allow(non_camel_case_types)]
use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use Eq;
use std::marker::PhantomData;

pub enum Action<Γ> {
    Push(Γ),
    Pop,
    PopAndPush(Γ),
}

type K<Q, Σ, Γ> = (Q, Σ, Option<Γ>);
type V<Q, Γ> = (Q, Action<Γ>);
type M<Q, Σ, Γ> = HashMap<K<Q, Σ, Γ>, V<Q, Γ>>;

pub trait HashMapExt<Q, Σ, Γ> {
    fn get(&self, k: &K<Q, Σ, Γ>) -> Option<&V<Q, Γ>>;
}

impl<Q, Σ, Γ> HashMapExt<Q, Σ, Γ> for M<Q, Σ, Γ> 
where Q: Eq + Hash, Σ: Eq + Hash, Γ: Eq + Hash, {
    fn get(&self, k: &(Q, Σ, Option<Γ>)) -> Option<&(Q, Action<Γ>)> {
        HashMap::get(&self, k)
    }
}

/// Q is a finite set of states
/// Σ is the input alphabet
/// Γ is the stack alphabet
/// δ is the transition function
struct PushdownAutomaton<Q, Σ, Γ, δ = M<Q, Σ, Γ>> {
    stack: Vec<Γ>,
    transitions: δ,
    start_state: Q,
    final_states: HashSet<Q>,
    _phantom: PhantomData<Σ>,
}

impl<Q, Σ, Γ, δ> PushdownAutomaton<Q, Σ, Γ, δ> 
where δ: HashMapExt<Q, Σ, Γ>, Q: Default, Γ: Clone, Q: Clone {
    pub fn new(start_state: Q, transitions: δ, final_states: HashSet<Q>) -> Self {
        let stack = Vec::new();
        Self {stack, transitions, start_state, final_states, _phantom: PhantomData}
    }
    pub fn transition(&mut self, input_symbol: Σ) -> core::result::Result<(), &'static str> {
        let current_state = std::mem::take(&mut self.start_state);
        let stack_top = self.stack.pop();
        let stack_top_clone = stack_top.clone();
        match self.transitions.get(&(current_state, input_symbol, stack_top)) {
            Some((next_state, Action::Push(symbol))) => {
                match stack_top_clone {
                    Some(v) => self.stack.push(v),
                    _ => {},
                }
                self.stack.push(symbol.clone());
                self.start_state = next_state.clone();
            },
            Some((next_state, Action::Pop)) => {
                // self.stack.pop();
                self.start_state = next_state.clone();
            },
            Some((next_state, Action::PopAndPush(symbol))) => {
                // self.stack.pop();
                // warning: will not error on an empty stack
                self.stack.push(symbol.clone());
                self.start_state = next_state.clone();
            },
            None => {return Err("Error: Transition not defined");},
        };
        Ok(())
    }
}

fn main() {
    test_pushdown_automaton();
}


// Function to test the Pushdown Automaton
fn test_pushdown_automaton() {
    // Define states and symbols
    #[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
    enum State {
        #[default]
        Start,
        A,
        B,
        C,
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    enum Symbol {
        X,
        Y,
        Z,
    }

    // Example transitions using M
    let mut transitions: M<State, Symbol, char> = HashMap::new();

    // Define transitions
    transitions.insert((State::Start, Symbol::X, None), (State::A, Action::Push('a')));
    transitions.insert((State::A, Symbol::Y, Some('a')), (State::B, Action::Pop));
    transitions.insert((State::B, Symbol::Z, None), (State::C, Action::PopAndPush('b')));
    transitions.insert((State::C, Symbol::X, Some('b')), (State::Start, Action::Push('c')));

    // Create PDA instance
    let final_states = HashSet::new(); // No final states defined in this example
    let mut pda = PushdownAutomaton::new(State::Start, transitions, final_states);

    // Test transitions
    assert_eq!(pda.transition(Symbol::X), Ok(()));
    assert_eq!(pda.start_state, State::A);
    assert_eq!(pda.stack, vec!['a']);

    assert_eq!(pda.transition(Symbol::Y), Ok(()));
    assert_eq!(pda.start_state, State::B);
    assert_eq!(pda.stack, vec![]);

    assert_eq!(pda.transition(Symbol::Z), Ok(()));
    assert_eq!(pda.start_state, State::C);
    assert_eq!(pda.stack, vec!['b']);

    assert_eq!(pda.transition(Symbol::X), Ok(()));
    assert_eq!(pda.start_state, State::Start);
    assert_eq!(pda.stack, vec!['b', 'c']);

    // Test error cases
    assert_eq!(pda.transition(Symbol::Y), Err("Error: Transition not defined"));
}
