#![allow(non_camel_case_types)]
use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::marker::PhantomData;

pub enum Action<Γ> {
    Push(Γ),
    Pop,
    PopAndPush(Γ),
}

type K<'a, Q, Σ, Γ> = (&'a Q, Σ, Option<&'a Γ>);
type V<Q, Γ> = (Q, Action<Γ>);
type M<'a, Q, Σ, Γ> = HashMap<K<'a, Q, Σ, Γ>, V<Q, Γ>>;

pub trait HashMapExt<'a, Q, Σ, Γ> {
    fn get(&self, k: &K<'a, Q, Σ, Γ>) -> Option<&V<Q, Γ>>;
}

impl<'a, Q, Σ, Γ> HashMapExt<'a, Q, Σ, Γ> for M<'a, Q, Σ, Γ>
where
    Q: Eq + Hash,
    Σ: Eq + Hash,
    Γ: Eq + Hash,
{
    fn get(&self, k: &K<'a, Q, Σ, Γ>) -> Option<&V<Q, Γ>> {
        HashMap::get(self, k)
    }
}

/// Q is a finite set of states
/// Σ is the input alphabet
/// Γ is the stack alphabet
/// δ is the transition function
struct PushdownAutomaton<'a, Q, Σ, Γ, δ = M<'a, Q, Σ, Γ>> {
    stack: Vec<*const Γ>,
    start_state: *const Q,
    final_states: HashSet<*const Q>,
    transitions: δ,
    _phantom: &'a PhantomData<Σ>
}

impl<'a, Q, Σ, Γ, δ> PushdownAutomaton<'a, Q, Σ, Γ, δ>
where
    Q: Eq + Hash + 'a,
    Σ: Eq + Hash,
    Γ: Eq + Hash + 'a,
    δ: HashMapExt<'a, Q, Σ, Γ>,
{
    pub fn new(
        start_state: &Q,
        final_states: HashSet<&Q>,
        transitions: δ,
    ) -> Self {
        let stack = Vec::new();
        let final_states: HashSet<*const Q> = unsafe { core::mem::transmute(final_states) };
        Self {
            stack,
            start_state,
            final_states,
            transitions,
            _phantom: &PhantomData,
        }
    }

    pub fn transition(&mut self, input_symbol: Σ) -> Result<(), &'static str> {
        let stack_top =self.stack.pop();
        let stack_top_ptr: Option<&Γ> = unsafe { stack_top.map(|r| &*r) };

        let start_state = unsafe { &*self.start_state as &Q };

        match self.transitions.get(&(start_state, input_symbol, stack_top_ptr)) {
            Some((next_state, Action::Push(symbol))) => {
                if let Some(value) = stack_top {
                    self.stack.push(value as *const Γ); // Push back the original top
                }
                self.stack.push(symbol as *const Γ);
                self.start_state = next_state;
            }
            Some((next_state, Action::Pop)) => {
                self.start_state = next_state;
            }
            Some((next_state, Action::PopAndPush(symbol))) => {
                self.stack.push(symbol as *const Γ);
                self.start_state = next_state;
            }
            None => return Err("Error: Transition not defined"),
        };
        Ok(())
    }
    pub fn get_state(&self) -> &Q {
        unsafe { &*self.start_state as &Q }
    }
    pub fn get_stack(&self) -> Vec<&Γ> {
        let mut stack = Vec::with_capacity(self.stack.len());
        unsafe {        
            stack.set_len(self.stack.len());
    
            core::ptr::copy_nonoverlapping(
                self.stack.as_ptr() as *const &Γ,
                stack.as_mut_ptr(),
                self.stack.len(),
            );
    
            stack
        }
    }
}

fn main() {
    test_pushdown_automaton()
}

// Function to test the Pushdown Automaton
fn test_pushdown_automaton() {
    // Example transitions using M
    let mut transitions: M<State, Symbol, StackElement> = HashMap::new();

    // Define transitions
    transitions.insert((&State::Start, Symbol::X, None), (State::A, Action::Push(StackElement::A)));
    transitions.insert((&State::A, Symbol::Y, Some(&StackElement::A)), (State::B, Action::Pop));
    transitions.insert((&State::B, Symbol::Z, None), (State::C, Action::PopAndPush(StackElement::B)));
    transitions.insert((&State::C, Symbol::X, Some(&StackElement::B)), (State::Start, Action::Push(StackElement::C)));

    // Create PDA instance
    let final_states = HashSet::new(); // No final states defined in this example
    let start_state = &State::Start;
    let mut pda = PushdownAutomaton::new(start_state, final_states, transitions);

    // Test transitions
    assert_eq!(pda.transition(Symbol::X), Ok(()));
    assert_eq!(pda.get_state(), &State::A);
    assert_eq!(pda.get_stack(), vec![&StackElement::A]);

    assert_eq!(pda.transition(Symbol::Y), Ok(()));
    assert_eq!(pda.get_state(), &State::B);
    let empty_vec: Vec<&StackElement> = Vec::new();
    assert_eq!(pda.get_stack(), empty_vec);

    assert_eq!(pda.transition(Symbol::Z), Ok(()));
    assert_eq!(pda.get_state(), &State::C);
    assert_eq!(pda.get_stack(), vec![&StackElement::B]);

    assert_eq!(pda.transition(Symbol::X), Ok(()));
    assert_eq!(pda.get_state(), &State::Start);
    assert_eq!(pda.get_stack(), vec![&StackElement::B, &StackElement::C]);

    // Test error cases
    assert_eq!(pda.transition(Symbol::Y), Err("Error: Transition not defined"));
}

// Enum definitions for the PDA
#[derive(Debug, PartialEq, Eq, Hash)]
enum State {
    Start,
    A,
    B,
    C,
}

#[derive(Debug, PartialEq, Eq, Hash)]
enum Symbol {
    X,
    Y,
    Z,
}

#[derive(Debug, PartialEq, Eq, Hash)]
enum StackElement {
    A,
    B,
    C,
}
