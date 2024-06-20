#![allow(non_camel_case_types)]
use std::collections::HashSet;
use std::hash::Hash;
use std::marker::PhantomData;
use std::cmp::Eq;
use std::cmp::PartialEq;

pub enum Action<Γ> {
    Push(Γ),
    Pop,
    PopAndPush(Γ),
}

#[derive(Hash, PartialEq, Eq)]
pub struct TransitionCondition<Q, Σ, Γ>(Q, Σ, Option<Γ>);
#[derive(Hash, PartialEq, Eq)]
pub struct TransitionConditionRef<'a, Q, Σ, Γ>(&'a Q, Σ, Option<&'a Γ>);
pub struct TransitionAction<Q, Γ>(Q, Action<Γ>);

type K<Q, Σ, Γ> = TransitionCondition<Q, Σ, Γ>;
type V<Q, Γ> = TransitionAction<Q, Γ>;
type M<Q, Σ, Γ> = hashbrown::HashMap<K<Q, Σ, Γ>, V<Q, Γ>>;

impl<'a, 'b, Q, Σ, Γ> hashbrown::Equivalent<TransitionCondition<Q, Σ, Γ>> for TransitionConditionRef<'a, Q, Σ, Γ>
where Q: PartialEq, Σ: PartialEq, Γ: PartialEq,{
    fn equivalent(&self, key: &TransitionCondition<Q, Σ, Γ>) -> bool {
        self.0 == &key.0 && self.1 == key.1 && self.2 == key.2.as_ref()
    }
}

pub trait HashMapExt<'a, Q, Σ, Γ> {
    fn get(&self, k: &TransitionConditionRef<'a, Q, Σ, Γ>) -> Option<&V<Q, Γ>>;
}

impl<'a, 'b, Q, Σ, Γ> HashMapExt<'a, Q, Σ, Γ> for M<Q, Σ, Γ>
where
    Q: Eq + Hash,
    Σ: Eq + Hash,
    Γ: Eq + Hash,
{
    fn get(&self, k: &TransitionConditionRef<'_, Q, Σ, Γ>) -> Option<&TransitionAction<Q, Γ>> {
    // fn get(&self, k: &TransitionCondition<Q, Σ, Γ>) -> Option<&TransitionAction<Q, Γ>> {
        hashbrown::HashMap::get(self, k)
    }
}

/// Q is a finite set of states
/// Σ is the input alphabet
/// Γ is the stack alphabet
/// δ is the transition function
/// https://en.wikipedia.org/wiki/Pushdown_automaton#Formal_definition
pub struct PushdownAutomaton<Q, Σ, Γ, δ> {
    stack: Vec<*const Γ>,
    start_state: *const Q,
    final_states: HashSet<*const Q>,
    transitions: δ,
    _phantom: PhantomData<Σ>
}

impl<'a, Q, Σ, Γ, δ> PushdownAutomaton<Q, Σ, Γ, δ>
where
    Q: Eq + Hash + 'a,
    Σ: Eq + Hash,
    Γ: Eq + Hash + 'a,
    δ: HashMapExt<'a, Q, Σ, Γ> + 'a,
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
            _phantom: PhantomData,
        }
    }

    pub fn transition(&mut self, input_symbol: Σ) -> Result<(), &'static str> {
        let stack_top =self.stack.pop();
        let stack_top_ptr: Option<&Γ> = unsafe { stack_top.map(|r| &*r) };
        let start_state = unsafe { &*self.start_state as &Q };
        let key = TransitionConditionRef(start_state, input_symbol, stack_top_ptr);

        match self.transitions.get(&key) {
            Some(TransitionAction(next_state, Action::Push(symbol))) => {
                if let Some(value) = stack_top {
                    self.stack.push(value as *const Γ); // Push back the original top
                }
                self.stack.push(symbol as *const Γ);
                self.start_state = next_state;
            }
            Some(TransitionAction(next_state, Action::Pop)) => {
                self.start_state = next_state;
            }
            Some(TransitionAction(next_state, Action::PopAndPush(symbol))) => {
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
    let mut transitions: M<State, Symbol, StackElement> = hashbrown::HashMap::new();

    // Define transitions
    transitions.insert(
        TransitionCondition(State::Start, Symbol::X, None), 
        TransitionAction(State::A, Action::Push(StackElement::A))
    );
    transitions.insert(
        TransitionCondition(State::A, Symbol::Y, Some(StackElement::A)), 
        TransitionAction(State::B, Action::Pop)
    );
    transitions.insert(
        TransitionCondition(State::B, Symbol::Z, None), 
        TransitionAction(State::C, Action::PopAndPush(StackElement::B))
    );
    transitions.insert(
        TransitionCondition(State::C, Symbol::X, Some(StackElement::B)), 
        TransitionAction(State::Start, Action::Push(StackElement::C))
    );

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
