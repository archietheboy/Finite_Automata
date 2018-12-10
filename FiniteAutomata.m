(* ::Package:: *)

(*:Mathematica Version: 2.0 *)

(*:Title:  Finite Automata *)

(*:Author: Alon Levy *)

(*:Summary: 

  Implements algorithms on finite-automata and regular
  expressions.
  
*)

(*:Keywords:

 Finite-automata;
 State-machines;
 Regular-expressions;

*)

(*:Requirements:  none  *)

(*:Sources:
   Hopcroft & Ullman - "Introduction to Automata Theory,
   Languages, and Computation". Addison Wesley, 1979.
   
   Hopcroft J.E. - "An n Log n algorithm for minimizing
   the states in a finite automaton". The Theory of
   Machines and Computations (Z. Kohavi, ed.), pp. 189-196,
   Academic Press, New York, 1971.
   
*)

(*:History:
     Version 1.0 by Alon Levy, April 1991.
     
*)

(*:Acknowledgement:

   To Klaus Sutner, for the 'Language' and 'PrettyPrint'
   functions.
*)


BeginPackage["FiniteAutomata`"]

(* Usage Messages *)

(*Automata Type*)

DFA::usage = " Deterministic Finite-Automaton"

Moore::usage = " Moore machine"

Mealy::usage = " Mealy machine"

NDFA::usage = " Non-deterministic Finite-automaton"

(*Add to Automata*)

AddFinalState::usage = " AddFinalState[aut,state] adds
state to the set of final states of the automaton named by
aut."

AddState::usage = " AddState[aut,state] will add state to
the list of states named by aut." 

AddToAlphabet::usage = " AddToAlphabet[aut,elem] adds elem
to the alphabet of the automaton named by aut."

AddToAutomaton::usage = " AddToAutomaton[aut, component,
entry] will add the component entry to the automaton denoted
by aut. aut is a name of an automaton. component is one of State, Transition, FinalState or
InputLetter. See also AddState, AddTransition, AddFinalState
and AddToAlphabet."

AddTransition::usage = " AddTransition[aut,trans] adds
the transition trans to the automaton named by aut. trans is 
a triplet {initial state, input symbol, destination state}. If aut
is an NDFA, destination state can be a list. In that case, these
states will be added to the destination states of the 
transition."

(*Delete from Automata*)

DeleteFinalState::usage = " DeleteFinalState[aut,state] deletes
state from the set of final states of the automaton named by
aut."

DeleteFromAlphabet::usage = " DeleteFromAlphabet[aut,elem] deletes
elem from the alphabet of the automaton named by aut. It also deletes
all transitions on elem."

DeleteFromAutomaton::usage = " DeleteFromAutomaton[aut, component,
entry] will delete the component entry from the automaton denoted
by aut. aut is a name of an automaton. component is one of State, Transition, FinalState or
InputLetter. See also DeleteState, DeleteTransition, DeleteFinalState
and DeleteFromAlphabet."

DeleteState::usage = " DeleteState[aut,state] will delete state from
the list of states named by aut. It will also delete all transitions
associated with state and all transitions to state."

DeleteTransition::usage = " DeleteTransition[aut,trans] deletes
the transition trans from the automaton named by aut. trans is 
a triplet {initial state, input symbol, destination state}. If aut
is an NDFA, destination state can be a list. In that case, these
states will be deleted from the destination states of the 
transition."

(*TestQ*)

AutomatonQ::usage = " AutomatonQ[aut] returns True iff aut is an
automaton (either DFA, NDFA, Moore or Mealy)."

EmptyAutomatonQ::usage = " EmptyAutomatonQ[aut] returns True iff
aut does not accept any input string in its language."

EquivalentAutomataQ::usage = " EquivalentAutomataQ[aut1,aut2] returns
True iff aut1 and aut2 accept the same language."

FiniteAutomatonQ::usage = " FiniteAutomatonQ[aut] returns True iff
aut accepts a finite number of strings."

InRegSetQ::usage = " InRegSetQ[reg,st] returns True iff the list of
inputs st is a member of the regular set represented by reg."

IsomorphicAutomataQ::usage = " IsomorphicAutomataQ[aut1,aut2] returns
True iff aut1 and aut2 are isomorphic. The function is not defined
for pairs of NDFA's."

(*Emmbeding*)

Circular::usage = " Produces a circular embedding."

CircularEmbedding::usage = " CircularEmbedding[aut] returns an
embedding for the states of aut in which they are equally spaced
along the circumference of a circle. CircularEmbedding can
also accept an integer representing the number of states
instead of an automaton."

Embedding::usage = " An option to MakeAutomaton and ShowAutomaton.
Default value is False for MakeAutomaton (which 
results in no computed embedding) and 
Automatic for ShowAutomaton (which results
in circular embedding). If value is a list,
it will be used as the embedding function.
Other possible values are Grid and Spring."

Grid::usage = " A value to option Embedding that produces a grid
embedding."

GridEmbedding::usage = " GridEmbedding[aut] returns an embedding for
the states of aut in which the states are organized in a grid."

Spring::usage = " A value to option Embedding that gives a spring
embedding"

SpringEmbedding::usage = " SpringEmbedding[aut] returns the spring
embedding of the states of aut in the plane. This is a slight
variation of the spring embedding described in Page 116 of 
'Implementing Discrete Mathematics' by
Steven Skeina."

(*Extract data from Automata*)

Alphabet::usage = " Alphabet[aut] returns the list of characters in
the alphabet of the automaton aut."

AutomatonType::usage = " AutomatonType[aut] returns the type of the
 automaton x, one of the following symbols
  DFA, NDFA, Moore or Mealy."
  
FinalStates::usage = " FinalStates[aut] returns the list of final
(accepting) states of the automaton aut. For Mealy and Moore machines
it will return {}."

InitialState::usage = " InitialState[aut] returns the initial state
of the automaton aut."

Transitions::usage = " Transitions[aut] returns the list transitions
of the automaton aut. Each transition is given as a triplet of
initial state, alphabet symbol, destination state."

(*Extract computed data from Automata*)

Language::usage = " Language[aut,n] returns the strings of length
n accepted by the automaton n. If n is negative, it returns all
the strings of length less than or equal to n that are accepted
by aut."

LanguageSize::usage = " LanguagesSize[aut,n] returns the number
of strings of length n that the automaton n accepts. If n is
negative, it returns the number of strings of length less than
or equal to n that are accepted by aut." 

(**)

AcceptedBy::usage = " AcceptedBy[aut,st] will return True iff the
automaton aut accepts the input sequence st. st is a list of
symbols from the automaton's alphabet. AcceptedBy[aut,st,Trace]
returns a pair in which the second element is
the list of states visited during the execution. If aut is an
NDFA the trace will contain a list of sets of states. aut is
either a DFA or an NDFA."
  
ChangeTransition::usage = " ChangeTransition[aut,{init,inp,dest}]
will change the transition of aut from state init on input inp to
dest. aut is a name of an automaton." 

Colored::usage = " An option to ShowAutomaton. If set to True,
ShowAutomaton will graph every state in a different color. All arcs
emanating from a state will be in the same color as the state.
The option can also be given a list of color specifiers for every
state (e.g. Hue[..] or RGBColor[...]). Its default value is False, which
causes the whole graph to be plotted in black."

ComplementAutomaton::usage = " ComplementAutomaton[aut] returns and
automaton that accepts the complement language of that accepted by
aut."

DFAToNDFA::usage = " DFAToNDFA[aut] returns an NDFA that accepts the
same language as the DFA aut."

epsilon::usage = " The symbol epsilon stands for the empty string.
It's used for epsilon moves in NDFAs."

EpsilonClosure::usage = " EpsilonClosure[st,aut] returns the set
of states reachable from state st via epsilon moves. st is not
the name of the state, but its position in States[aut]."

FinalState::usage = " DeleteFromAutomaton[aut,FinalState,entry] and 
AddToAutomaton[aut,FinalState,entry] are equivalent to DeleteFinalState[aut,entry]
and AddFinalState[aut,entry]."

From::usage = "From[a1,a2] is regular set containing the symbols
a1,...,a2 where a1 and a2 are characters."

InputLetter::usage = "check"

IntersectAutomata::usage = " IntersectAutomata[aut1,aut2] returns
an automaton that accepts the intersection of the languages
accepted by aut1 and aut2. The resulting automaton is a DFA
only if both aut1 and aut2 are DFA's."

KleeneAutomaton::usage = " KleeneAutomaton[aut] returns an NDFA
that accepts the Kleene closure of the language accepted by
aut."

MakeAutomaton::usage = " MakeAutomaton[type,states,transitions,init,
final,alphabet] creates an automaton with those specifications.
type is either DFA (Deterministic Finite Automaton), NDFA (Non-deterministic
Finite Automaton), Moore (Moore machine) or Mealy (Mealy machine).
States is a list of the states of the automaton. In the case of
a Moore machine, each state is a pair of {name, output symbol}.
transitions is a list of triplets representing the transition function.
Each triplet is of the form {initial state, input symbol, destination 
state}.In the case of an NDFA, the destination state is a list
of states. epsilon is a special symbol used for epsilon-moves.
In the case of a Mealy machine, each transition has a fourth
element, the output symbol. init an element from states, denoting the
initial state of the automaton. final is a list of states that are
the accepting states of the automaton. final is specified only for
DFA's and NDFA's. Alphabet is a list of the input alphabet of the
automaton. Embedding is an option to MakeAutomaton."

MinimizeAutomaton::usage = " MinimizeAutomaton[aut] returns an automaton
equivalent to aut with a minimal number of states. The minimal
automaton is a DFA. It uses an algorithm by Hopcroft to find the
equivalence classes of the states."

NDFAToDFA::usage = " NDFAToDFA[aut] returns a deterministic finite
automaton that accept the same language as the NDFA aut. This function
might take time exponential in the number of states in aut."

NextMealyState::usage "NextMealyState[st,char,aut] returns the state
that the Mealy machine aut will enter from state st, when reading 
the input char, and the symbol that it will output on that
transition. They are returned as a list of two elements."

NextState::usage = "NextState[st,char,aut] returns the state
that aut will enter from state st, when reading the input char."

OutputString::usage = " OutputString[aut,st] will return the output
string produced by the execution of a Moore or Mealy machine on
input st. OutputString[aut,st,Trace] will produce a pair whose
second element is the names of the states visited during the
execution."

PrettyPrint::usage = " PrettyPrint[aut] pretty prints the automaton."

RandomAutomaton::usage = "RandomAutomaton[n,m] will produce a random
DFA with n states and m alphabet symbols. Random[n,m,p] will produce
and automaton in which the probability of a state being a final
state is p."

RC::usage = " RC[exp1,exp2] is the regular expression representing
the regular set of the concatenation of the sets represented by
exp1 and exp2."

ReachableStates::usage = " ReachableStates[aut] returns the list
of states reachable from the initial state via a sequence of input
symbols."

RegExp::usage = " RegExp[exp] returns an internal representation of
a regular expression. The input form on a regular expression is the
following. epsilon is a regular expression denoting the empty string.
a string is a regular expression. If reg1 and reg2 are regular expressions
then RP[reg1,reg2], RC[reg1,reg2] and RK[reg1] are regular expressions
corresponding to the union, concatenation and Kleene closure of their
arugments. If reg1 and reg2 are single characters, From[reg1,reg2]
is the set of characters from reg1 to reg2."

RegExpToAutomaton::usage = " RegExpToAutomaton[reg] returns an NDFA
that accepts the regular set represented by reg. It also produces
an embedding as it constructs the automaton."

RemoveEpsilonMoves::usage = " RemovesEpsilonMoves[aut] returns an
NDFA equivalent to aut that contains no epsilon moves."

ReverseAutomaton::usage = " ReverseAutomaton[aut] returns an automaton
in which the transitions are reversed and the final states and initial
state are swapped. If there are several final states, a new dummy
initial state is created with epsilon transitions to the new initial
states."

RK::usage = " RK[exp] is the regular set representing the Kleene closure
of the set represented by exp."

RP::usage = " RP[exp1,exp2] is the regular expression representing
the union of the regular sets represented by exp1 and exp2."

ShowAutomaton::usage = " ShowAutomaton[aut] graphs the automaton
aut. The embedding of the states in the plane is determined as follows.
If the option Embedding is set to Automatic (the default), the
embedding associated with the automaton is used if one exists,
otherwise, a circular embedding is used. Other possible emeddings
are Spring, Grid and TurboGrid. If Embedding is set to a list,
that list is taken as the embedding."

ShowExecution::usage = " ShowExecution[aut,st] will produce
an animation of the execution of the automaton aut with input st. st is a 
list of alphabet symbols. ShowExecution accepts all the options
of ShowAutomaton."

State::usage = " DeleteFromAutomaton[aut,State,entry] and 
AddToAutomaton[aut,State,entry] are equivalent to DeleteState[aut,entry]
and AddState[aut,entry]."

StateEmbedding::usage = " StateEmbedding[aut] returns the embedding
of the states of aut in the x-y plane. It is a list of {x,y} pairs.
The embedding must have been explicitly computed previously."

States::usage = " States[aut] returns the list of states of the
automaton aut."

StateSize::usage = " The value of this option determines
the size of the circles drawn around the states. The minimal
state distance is multiplied by StateSize to determine its
size. It's default value is 0.15."

Transition::usage = " DeleteFromAutomaton[aut,Transition,entry] and 
AddToAutomaton[aut,Transition,entry] are equivalent to DeleteTransition[aut,entry]
and AddTransition[aut,entry]."

UnionAutomata::usage = " UnionAutomata[aut1,aut2] returns an NDFA
that accepts the union of the languages accepted by aut1 and aut2.
If both automata contain embeddings a union embedding is created."


Begin["`Private`"]

(* Error Messages *)

MakeAutomaton::BadInit = " Initial state not in state list"
MakeAutomaton::BadTransition = "Invalid syntax for transition "
MakeAutomaton::UnknownAlphabetSymbol=" Alphabet letter used in transition unknown. "
MakeAutomaton::UnknownOriginState = "Origin state of transition unknown"
MakeAutomaton::UnknownDestState =" Destination state of transition unknown"
ChangeTransition::BadState = " State is not in the automaton "
AddTransition::ConflictingTransition = " The automaton contains a transition
that conflicts the one you are trying to add. No change make."
DeleteFromAutomaton::BadEntry = 
    "Entry to delete is not in the automaton"
DeleteFromAutomaton::InitialState= " Cannot delete initial state "
AddToAutomaton::InvalidMooreState = " Invalid state for Moore machine"
AddToAutomaton::BadFinalState= " Final state not in automaton"
AddToAutomaton::InvalidTransition= "Invalid transition "
AddToAutomaton::RedundantState = " State already is in automaton."
StringRange::InvalidSpec = " Invalid String range specification "
(* Misc. Declarations *)

Format[Automaton[x__]]:="- Automaton -"
Options[MakeAutomaton] = {Embedding->False}
Options[ShowAutomaton] = {Embedding->Automatic,Colored->False,
                          StateSize->0.15}
                          
SetAttributes[{AddToAutomaton,DeleteEntry,DeleteState,DeleteTransition,
               DeleteFinalState,
              DeleteFromAutomaton,ChangeTransition,AddState,
              DeleteFromAlphabet,AddToAlphabet,
              AddFinalState,AddTransition},HoldFirst]
              
npi= N[Pi,5]

(* Selector Functions: *)

AutomatonType[x_Automaton]:=  x[[1]]
States[x_Automaton]:=         x[[2]]
Transitions[x:Automaton[DFA,___]]:=
   Flatten[MapIndexed[{x[[2,#2[[1]]]],#1[[1]],
                       x[[2,#1[[2]]]]}&,
                       x[[3]],{2}],1]
Transitions[x:Automaton[Mealy,___]]:=
   Flatten[MapIndexed[{x[[2,#2[[1]]]],#1[[1]],
                       x[[2,#1[[2]]]],#1[[3]]}&,
                       x[[3]],{2}],1]
Transitions[x:Automaton[Moore,___]]:=
   Flatten[MapIndexed[{x[[2,#2[[1]],1]],#1[[1]],
                       x[[2,#1[[2]],1]]}&,
                       x[[3]],{2}],1]
Transitions[x:Automaton[NDFA,___]] :=
  Flatten[MapIndexed[{x[[2,#2[[1]]]],#1[[1]],
                     Map[x[[2,#]]&,
                     #1[[2]]]}&,
                     x[[3]],{2}],1] 
InitialState[x_Automaton]:=   x[[2,x[[4]]]]
FinalStates[x_Automaton]:=    x[[2,#]]& /@ x[[5]]
StateEmbedding[x_Automaton]:=      x[[6]]
Alphabet[x_Automaton]:=       x[[7]]
AutomatonQ[x_]:= MemberQ[{DFA,NDFA,Moore,Mealy},x]

(* Creator Function: *)

MakeAutomaton[type_?AutomatonQ,st_List,
              tran:{List[___]...},init_,final_List,syms_List,
              opts___Rule]:=
  Block[{tr,st1,st2,len,i,trans,stt=st,opt1,tr2,
         epsyms=If[type===NDFA,Append[syms,epsilon],syms]},
  If[!MemberQ[stt,init],Message[MakeAutomaton::BadInit],
     opt1= Embedding /. {opts} /. Options[MakeAutomaton];
     len=Length[stt];
     trans=Table[{},{len}];
     Do[tr=tran[[i]];
        tr2=tr[[2]];
        If[!MemberQ[epsyms,tr2],
           Message[MakeAutomaton::UnknownAlphabetSymbol],
        If[!(If[type===Mealy,4,3]==Length[tr]),
           Message[MakeAutomaton::BadTransition],
           If[!MemberQ[stt,tr[[1]]],
              Message[MakeAutmaton::UnknownOriginState],
              st1=Position[stt,tr[[1]]][[1,1]];
              If[type=!=NDFA,
                st2=Position[stt,tr[[3]]];
                If[Length[st2]==0,
                   Message[MakeAutomaton::UnknownDestState],
                   PrependTo[trans[[st1]],
                      If[type=!=Mealy,{tr2,st2[[1,1]]},
                                      {tr2,st2[[1,1]],tr[[4]]}]]],       
                ndest=If[ListQ[tr[[3]]], tr[[3]],{tr[[3]]}];
                If[!subsetQ[ndest,stt],
                    Message[MakeAutomaton::UnknownDestState],
                    PrependTo[trans[[st1]],
                    {tr2,Map[Position[stt,#][[1,1]]&,ndest]}]]]]]],
           {i,Length[tran]}];
      tr=Automaton[type,st,trans,
                  Position[stt,init][[1,1]],
                  Map[Position[stt,#][[1,1]]&,final],{},
                  syms];
      ReplacePart[tr,
        Switch[ opt1,
                  False,  {},
                  Automatic, CircularEmbedding[tr],
                  Spring, SpringEmbedding[tr],
                  Grid,   GridEmbedding[tr],
                  Circular, CircularEmbedding[tr],
                 
                  _List, opt1],
               6]]]
MakeAutomaton[Moore,st_List,
              tran:{List[___]...},init_,syms_List,opts___Rule]:=
   Automaton @@ Join[Automaton[Moore,st],
             Take[MakeAutomaton[DFA,Transpose[st][[1]],
             tran,init,{},syms,opts],
             -5]]
MakeAutomaton[Mealy,st_List,
              tran:{List[___]...},init_,syms_List,opts___Rule]:=
              MakeAutomaton[Mealy,st,tran,init,{},syms,opts]
RandomAutomaton[n_Integer,m_Integer,prob_:0.2] :=
  Block[{i,j},
        MakeAutomaton[DFA,Range[n],
                Flatten[Table[{i,ToString[j],
                               Ceiling[Random[] n]},
                            {i,n},{j,m}],1],
                  Ceiling[Random[] n],
                  Select[Range[n],
                         (Random[]<prob)&],
                  Table[ToString[i],{i,m}]]]
subsetQ[s1_List,s2_List]:= Length[s1]==Length[Intersection[s1,s2]]

(* Modifier Functions *)

ChangeTransition[name_,tran_List]:=Block[
       {aut=Release[name],st1,lst2,st2,st3,
        dest=If[And[SameQ[NDFA,name[[1]]],!ListQ[tran[[3]]]],
                {tran[[3]]},tran[[3]]]},
       If[Or[Length[tran]<If[name[[1]]===Mealy,4,3],
             And[!MemberQ[name[[7]],tran[[2]]],
              !And[SameQ[epsilon,tran[[2]]],
                   SameQ[aut[[1]],NDFA]]]],
          Message[MakeAutomaton::UnknownAlphabetSymbol],
       inp=tran[[2]];
       st1=Position[States[aut],tran[[1]]];
       If[AutomatonType[name]=!=NDFA,
         st2=Position[States[aut],dest];
         lst2=Length[st2],
         st2=Map[Position[aut[[2]],#]&, dest];
         lst2=Apply[Times,Map[Length,st2]]];
       If[Length[st1] lst2 ==0, Message[ChangeTransition::BadState],
         st1=st1[[1,1]]; 
         st2=If[AutomatonType[name]=!=NDFA,
                st2[[1,1]],Map[#[[1,1]]&,st2]];
         st3=Position[aut[[3,st1]],inp];
         If[Length[st3]==0, PrependTo[name[[3,st1]],{inp,st2}],
            st3=st3[[1,1]];
            name[[3,st1,st3]]=If[AutomatonType[name]===Mealy,
                                 {inp,st2,tran[[4]]},
                                 {inp,st2}]]];
            name]]
DeleteFromAutomaton[name_,State,entry_]:= DeleteState[name,entry]
DeleteFromAutomaton[name_,Transition,entry_]:=
                                   DeleteTransition[name,entry]
DeleteFromAutomaton[name_,FinalState,entry_]:=
                                   DeleteFinalState[name,entry]
DeleteFromAutomaton[name_,InputLetter,entry_]:=
                                   DeleteFromAlphabet[name,entry]
DeleteState[name_,entry_]:=Block[{n},
  n=Position[States[name], entry];
  If[Length[n]==0, 
     Message[DeleteFromAutomaton::BadEntry],
     n=n[[1,1]];
     If[name[[4]]==n, Message[DeleteFromAutomaton::InitialState],
        name = Automaton[
                 name[[1]],
                 Drop[name[[2]],{n,n}],
                 Map[Select[#,Length[#]>0 &]&,
                            Map[AdjustTransitions
                                      [#,n,AutomatonType[name]]&,
                                Drop[name[[3]],{n,n}],{2}]],
                 If[name[[4]]>n, name[[4]]-1, name[[4]]],
                 Map[If[#>n, #-1,#]&,Complement[name[[5]],{n}]],
                 name[[6]],name[[7]]]]]]
AdjustTransitions[x_,n_,t_]:=
  Block[{bb},
    bb=Apply[Union,Map[Which[
           #==n, {},
           #>n, {#-1},
           True, {#}]&, 
    If[t=!=NDFA,{x[[2]]},x[[2]]]]];
    If[Length[bb]==0, {},
        Switch[t,
               NDFA,  {x[[1]],bb},
               Mealy, {x[[1]],bb[[1]],x[[3]]},
               _,     {x[[1]],bb[[1]]}]]]
   
DeleteTransition[name_,entry_]:=Block[{n,n1},
  n=Position[States[name],entry[[1]]];
  If[Length[n]==0, 
     Message[DeleteFromAutomaton::BadEntry],
     n1=Position[name[[3,n[[1,1]]]],
                 entry[[2]]];        
     If[Length[n1]==0, Message[DeleteFromAutomaton::BadEntry],
        If[name[[1]]=!=NDFA,
           If[SameQ[entry[[3]],
                    If[SameQ[name[[1]],Moore],
                      name[[2,name[[3,n[[1,1]],n1[[1,1]],2]],1]],
                      name[[2,name[[3,n[[1,1]],n1[[1,1]],2]]]]]],
           name[[3,n[[1,1]]]]=Drop[name[[3,n[[1,1]]]],
                            {n1[[1,1]],n1[[1,1]]}],
           Message[DeleteFromAutomaton::BadEntry]],
           name[[3,n[[1,1]],n1[[1,1]],2]] =
             Complement[name[[3,n[[1,1]],n1[[1,1]],2]],
                        If[ListQ[entry[[3]]], entry[[3]],
                                             {entry[[3]]}]]];
  name]]]
DeleteFinalState[name_,entry_]:=Block[{n},
  n=Position[States[name],entry];
  If[Length[n]==0,
     Message[DeleteFromAutomaton::BadEntry],
     name[[5]] = Complement[name[[5]],{name[[2,n[[1,1]]]]}];
     name]]
DeleteFromAlphabet[name_,entry_] :=
 Block[{},
   name[[7]] = Complement[name[[7]],{entry}];
   name[[3]] = Table [ Select[name[[3,i]],
                              UnsameQ[#[[1]],entry]&],
                      {i,Length[name[[3]]]}];
   name]
AddToAutomaton[name_,State,new_]:= AddState[name,new]
AddToAutomaton[name_,Transition,new_]:= AddTransition[name,new] 
AddToAutomaton[name_,FinalState,new_]:= AddFinalState[name,new]
AddToAutomaton[name_,InputLetter,new_]:= AddToAlphabet[name,new]
AddState[x_,y_]:= 
   If[SameQ[x[[1]],Moore],
    If[InvalidMooreState[x,y],
        Message[AddToAutomaton::InvalidMooreState],
        If[MemberQ[Transpose[x[[2]]][[1]],y[[1]]],
           Message[AddToAutomaton::RedundantState],
          x = Automaton[ x[[1]], Append[x[[2]],y], Append[x[[3]],{}],
                     x[[4]], x[[5]], x[[6]], x[[7]]]]],
      If[MemberQ[x[[2]],y], Message[AddToAutomaton::RedundantState], 
      x = Automaton[ x[[1]], Append[x[[2]],y], Append[x[[3]],{}],
                     x[[4]], x[[5]], x[[6]], x[[7]]]]]
InvalidMooreState[aut_,x_]:=And[AutomatonType[aut]===Moore,
                                Or[!ListQ[x],Length[x]!=2]]
AddFinalState[x_,y_]:=
   If[AutomatonType[x]=!=Moore,
      If[!MemberQ[States[x],y],
         Message[AddToAutomaton::BadFinalState],
         AppendTo[x[[5]],x[[2,Position[States[x],y][[1,1]]]]];
         x],
      If[!MemberQ[Transpose[States[x]][[1]],y],
         Message[AddToAutomaton::BadFinalState],
         AppendTo[x[[5]],
                 x[[2,Position[Transpose[States[x]][[1]],y][[1,1]]]]];
         x]]
AddTransition[x_,y_]:=
   Block[{st1,st2,lst2,st3,dest},
   dest=If[And[SameQ[NDFA,x[[1]]],!ListQ[y[[3]]]],{y[[3]]},y[[3]]];
   If[Or[!ListQ[y],If[AutomatonType[x]===Mealy,4,3]=!=Length[y]],
        Message[AddToAutomaton::InvalidTransition],
        st1=Position[x[[2]],y[[1]]];
        If[AutomatonType[x]=!=NDFA, 
           st2=Position[x[[2]],y[[3]]];
           lst2=Length[st2],
           st2=Map[Position[x[[2]],#]&, dest];
           lst2=Apply[Times,Map[Length,st2]]];
        If[Or[(Length[st1] lst2 == 0),
              And[!MemberQ[x[[7]],y[[2]]],UnsameQ[y[[2]],epsilon]]], 
           Message[AddToAutomaton::InvalidTransition],
           st1=st1[[1,1]];  (* position of origin *)
           st2=If[AutomatonType[x]=!=NDFA,
            (* position(s) of dest *)
                  st2[[1,1]],Map[#[[1,1]]&,st2]];
           st3=Position[x[[3,st1]],{y[[2]],___}];
           If[UnsameQ[x[[1]],NDFA],
              If[Length[st3] > 0,
                 Message[AddTransition::ConflictingTransition],
                 PrependTo[x[[3,st1]],
                           If[SameQ[AutomatonType[x],Mealy],
                              {y[[2]],st2,y[[4]]},
                              {y[[2]],st2}]]],
                 If[Length[st3] > 0,
                    x[[3,st1,st3[[1,1]],2]] = 
                      Union[x[[3,st1,st3[[1,1]],2]],
                            st2],
                    PrependTo[x[[3,st1]],
                              {y[[2]],st2}]]]]];
       x]
       
AddToAlphabet[x_,entry_]:=
  Block[{},
        x[[7]] = Union[x[[7]],{entry}];
        x]
        
(* Acceptor Functions *)

ShowExecution[auto_Automaton,st_List,opts___Rule] :=
  Block[{ll,trace,graph,i},
        ll=SameQ[auto[[1]],DFA];
        trace=AcceptedBy[auto,st,STATES];
        graph=ShowAutomaton[auto,opts,True];
        Do[Show[Graphics[Join[If[ll,If[trace[[i]]==={},{},
                                {Disk[embedding[[trace[[i]]]],
                                   size]}],
                                Map[Disk[embedding[[#]],size]&,
                                    trace[[i]]]],
                               graph],
                   { PlotRange->Automatic ,
                     AspectRatio->1}]],
            {i,Length[trace]}]]
AcceptedBy[auto:Automaton[DFA,___],st_List,t_:False]:=
  Block[{tr,trace},
        tr[s_,i_]:= tr[s,i]=NextState[s,i,auto];
        
        trace = FoldList[ tr, auto[[4]], st];
        Switch[t,
               False,MemberQ[auto[[5]],Last[trace]],
               STATES,trace,
               _ ,{MemberQ[auto[[5]],Last[trace]],
                     Map[auto[[2,#]]&,trace]}]]
AcceptedBy[auto:Automaton[NDFA,___],st_List,t_:False]:=
  Block[{trace,tr,epc,ns,l},
  
         epc[s_]:= epc[s] = EpsilonClosure[s,auto];
         ns[s_,i_]:= ns[s,i]= Union[Flatten[
                              Map[epc[#]&,NextState[s,i,auto]]]];
         tr[s_List,i_] := tr[s,i] = Union[Flatten[Map[ns[#,i]&,
                                                 s]]];
         trace = FoldList[ tr, epc[auto[[4]]], st];
         l = Length[Intersection[auto[[5]],Last[trace]]]>0;
         Switch[t,
                False, l,
                STATES, trace,
                _ , {l,Map[auto[[2,#]]&,trace,{2}]}]]
OutputString[auto:Automaton[Moore,___],st_List,t_:False]:=
   Block[{tr,trace,l},
         tr[s_,i_]:= tr[s,i]=NextState[s,i,auto];
        
        trace = FoldList[ tr, auto[[4]], st];
        l= Map[If[{}=!=#,auto[[2,#,2]],Null]&,trace];
        If[t=!=False, {l,Map[auto[[2,#,1]]&,trace]}, l]]
OutputString[auto:Automaton[Mealy,___],st_List,t_:False]:=
  Block[{tr,tr1,trace,t1},
         tr[s_,i_]:= Block[{g},
                        g=NextMealyState[s,i,auto];
                        tr1[s,i]=g[[2]];
                        tr[s,i]=g[[1]]];
        trace = FoldList[ tr, auto[[4]], st];
        t1=MapIndexed[tr1[trace[[#2[[1]]]],#]&,st];
        If[t===False,t1,{Map[auto[[2,#]]&,trace],t1}]]
NextState[st_,char_,aut_Automaton]:=
   If[Length[#]>0, #[[1,2]], #]& [Cases[aut[[3,st]],{char,__}]]
NextState[{},char_,auto_Automaton] := {}
NextMealyState[st_,char_,aut:Automaton[Mealy,___]]:=
   If[Length[#]>0, {#[[1,2]],#[[1,3]]},
                   {{},{}}]& [Cases[aut[[3,st]],{char,__}]]
NextMealyState[{{},{}},char_,auto:Automaton[Mealy,___]] := {{},{}}
NextEpsilonState[aut_,st_]:=
  Block[{t=Cases[aut[[3,st]],{epsilon,_}]},
  If[Length[t]==0, {},
     Apply[Union,Transpose[t][[2]]]]]
  
LanguageSize[auto_Automaton,n_Integer] :=
  Length[Language[auto,n]]
Language[ auto:Automaton[DFA,___], n_Integer ] := 
Block[  {M1,L,k,p0,F,A,f,prep,acc,words,toword},
		k  = Alphabet[auto];
		p0 = auto[[4]];
		F  = auto[[5]];
	

		f[s_,i_] := f[s,i] = NextState[s,i,auto];
			
		prep[ l_List, a_ ] := 
			Map[ Prepend[#,a]&, l ];
			
		L[s_,p_,q_] := L[s,p,q] =
		If[ s===0,
			If[ p===q, {{}}, {} ],
			tmp = Map[ prep[ L[s-1,f[p,#],q],#]&, k ];
			Union[ Flatten[tmp,1] ]
		];
		
		acc[s_] := 
			Union[ Flatten[ Map[ L[s,p0,#]&, F ], 1 ] ];
	
		words[s_] := acc[s];
		
		If[ n >= 0,
			words[n],
			Flatten[Map[ words, Range[0,-n] ],1] 
		]
];
Language[ auto:Automaton[NDFA,___], n_Integer ] := 
Block[  {M1,L,k,p0,F,A,f,prep,acc,words,toword,i,j,ll,ns,
         aut=RemoveEpsilonMoves[auto]},
		k  = Alphabet[aut];
		p0 = aut[[4]];
		F  = aut[[5]];
	
        ns[s_,i_]:= ns[s,i]= NextState[s,i,aut];
		prep[ l_List, a_ ] := 
			Map[ Prepend[#,a]&, l ];
		L[s_,p_,q_] := L[s,p,q] =
		If[ s===0,
			If[ p===q, {{}}, {} ],
			tmp = Map[ prep[Union[Flatten[
			                       Map[L[s-1,#,q]&,
			                           ns[p,#]],1]  ],#]&, k ];
		
			Union[ Flatten[tmp,1] ]
		];
		
		acc[s_] := Union[Flatten[Map[ L[s,p0,#]&, F ],1] ];
			
	
	    words[0] = If[MemberQ[F,p0],{epsilon},{}];
		words[s_] := acc[s];
		
		If[ n >= 0,
			words[n],
			Flatten[Map[ words, Range[0,-n] ],1] 
		]]
		
(* NDFA Simplifying Functions *)

RemoveEpsilonMoves[x:Automaton[NDFA,___]]:= 
   Block[{i},
         epsilonClosure=Table[EpsilonClosure[i,x],
                               {i,Length[x[[2]]]}];
         Automaton[x[[1]],x[[2]],
                   Table[ComputeNewStateTransitions[x,i],
                         {i,Length[x[[2]]]}],x[[4]],
                   If[Length[Intersection[x[[5]],
                      epsilonClosure[[x[[4]]]]]]==0,
                      x[[5]],
                      Union[x[[5]],{x[[4]]}]],
                   x[[6]],x[[7]]]]
         
RemovesEpsilonMoves[x_Automaton] := x
EpsilonClosure[st_,aut_Automaton]:=
  Block[{marked,state,stack,res},
        marked[x_Integer] := False;
        marked[st] = True;
        stack={st};
        res = {st};
        While[Length[stack]>0,
              state=First[stack];
              stack=Rest[stack];
              Map[If[!marked[#],
                     PrependTo[stack,#];
                     PrependTo[res,#];
                     marked[#]=True]&,
                  NextEpsilonState[aut,state]]];
       res]
ComputeNewStateTransitions[x_,i_]:=
  Block[{symbols,j,k},
  symbols= Complement[Apply[Union,  (* the symbols encountered by the e-clos *)
                 Map[If[Length[x[[3,#]]]>0,
                     Transpose[x[[3,#]]][[1]],{}]&,
                     epsilonClosure[[i]]]],{epsilon}];
  If[Length[symbols]>0,
     Table[{symbols[[k]],
            Apply[Union,
                  Map[epsilonClosure[[#]]&,
                      Apply[Union,
                            Table[NextState[epsilonClosure[[i,j]],
                                            symbols[[k]],x],
                                  {j,Length[epsilonClosure[[i]]]}]]]]},
            {k,Length[symbols]}],
     {}]]
NDFAToDFA[y:Automaton[NDFA,___]]:=Block[
   {stack,count,states,trans,final,st,oldtrans,pos,ntrans,x,j},
   x=RemoveEpsilonMoves[y];
   trans={{}};
   states={{x[[4]]}};
   stack={{{x[[4]]},1}};
   count=1;
   While[ Length[stack] > 0,
   
      pos=stack[[1,2]];
      st=stack[[1,1]];
      stack=Rest[stack];
      oldtrans=Union @@ Map[x[[3,#]]&,st];
      If[Length[oldtrans]>0,
      ntrans={Union[Transpose[oldtrans][[1]]],{}};
      ntrans[[2]]=Map[Apply[Union, 
                      Map[oldtrans[[#[[1]],2]]&,
                          Position[oldtrans,#]]]&,ntrans[[1]]];
      Do[ stt=Position[states,ntrans[[2,j]]];
          If[Length[stt]==0,
             count++;
             AppendTo[states,ntrans[[2,j]]];
             PrependTo[stack,{ntrans[[2,j]],count}];
             AppendTo[trans,{}];
             ntrans[[2,j]]=count,
             ntrans[[2,j]]=stt[[1,1]]],
          
         {j,Length[ntrans[[1]]]}];
         
      trans[[pos]]=Transpose[ntrans]]];
      
   final=Apply[Union,Map[If[Length[(pos=Position[states,#])] > 0,
                            Transpose[pos][[1]],{}]&,
                         x[[5]]]];
                         
   Automaton[DFA,Range[Length[states]],trans,1,final,{},x[[7]]]]
DFAToNDFA[x:Automaton[DFA,___]]:=
                        Automaton[NDFA,x[[2]],
                                  Map[{#[[1]],{#[[2]]}}&,x[[3]],{2}],
                                  x[[4]],x[[5]],x[[6]],x[[7]]]

(* Automata Properties Checking Functions *)

EmptyAutomatonQ[x:Automaton[DFA,___] | x:Automaton[NDFA,___]]:=
    Intersection[ReachableStates[x],x[[5]]]==={}
ReachableStates[aut_Automaton]:=
  Block[{marked,state,stack,res},
        marked[x_Integer] := False;
        marked[aut[[4]]] = True;
        stack={aut[[4]]};
        res = {aut[[4]]};
        While[Length[stack]>0,
              state=First[stack];
              stack=Rest[stack];
              Map[If[!marked[#],
                     PrependTo[stack,#];
                     PrependTo[res,#];
                     marked[#]=True]&,
                  NextPossibleStates[aut,state]]];
       res] 
NextPossibleStates[aut:Automaton[NDFA,___],state_] :=
      If[Length[aut[[3,state]]]==0, {},
         Union @@ Transpose[aut[[3,state]]][[2]]]
NextPossibleStates[aut_Automaton,state_]:=
      If[Length[aut[[3,state]]]==0, {},
         Union[Transpose[aut[[3,state]]][[2]]]]
 FiniteAutomatonQ[au:Automaton[DFA,___] |
                  au:Automaton[NDFA,___]]:= 
   Block[{gaut,relevantStates,aut},
         aut = If[SameQ[au[[1]],NDFA], RemoveEpsilonMoves[au],
                  au];
         gaut=AutomatonToGraph[aut];
         relevantStates=ClosureInGraph[
                             ReverseGraph[gaut],aut[[5]]];
         gaut=Table[Select[gaut[[i]],MemberQ[relevantStates,#]&],
              {i,Length[gaut]}];
         If[!MemberQ[relevantStates,aut[[4]]],True,
            !CycleFromQ[gaut,aut[[4]]]]]
                       
ReverseGraph[graph_]:=
  Table[Map[#[[1]]&,Position[graph,i]],
        {i,Length[graph]}]
AutomatonToGraph[aut_Automaton]:=
   Table[If[Length[aut[[3,i]]]==0, {},
          Union[Flatten[Transpose[aut[[3,i]]][[2]]]]],
          {i,Length[aut[[2]]]}]
          
CycleFromQ[graph_,vertex_]:=
        Block[{dfcount=0,queue={vertex},back=False,
               mat,mat1},
              
          mat[x_Integer] := unvisited;
          mat1[x_Integer] := 0;
          
          While[And[!back,Length[queue]>0],
                vert=First[queue]; 
                queue=Rest[queue];
                If[Head[vert]=== Marker, mat1[vert[[1]]]=Done,
                   If[mat[vert] === unvisited,
                      neighs=graph[[vert]];
                      mat[vert]=dfcount; 
                      PrependTo[queue, Marker[vert]];
                      dfcount++; 
                      Do[If[mat[neighs[[i]]] === unvisited,
                            PrependTo[queue,neighs[[i]]],
                            If[mat1[neighs[[i]]]=!=Done,
                               back=True]],
                        {i,Length[neighs]}]]]];
          back]
ClosureInGraph[graph_,init_]:=Block[{new},
        new=Union[init,Apply[Union,Map[graph[[#]]&,init]]];
        If[Length[init]==Length[new], 
           new,
           ClosureInGraph[graph,new]]]
EquivalentAutomataQ[x_Automaton,y_Automaton]:=
     IsomorphicAutomataQ[
          MinimizeAutomaton[x],
          MinimizeAutomaton[y]]
IsomorphicAutomataQ[x:Automaton[DFA,___],
                    y:Automaton[DFA,___]]:=
  Block[{map,stack,term=False},
  
    map[a_Integer,b_Integer] := 0;
  
    If[Or[Length[x[[2]]]!=Length[y[[2]]],
          Length[x[[4]]]!=Length[y[[4]]],
          UnsameQ[x[[1]],y[[1]]]], False,
       map[1,x[[4]]]=y[[4]];
       map[2,y[[4]]]=x[[4]];
       stack={x[[4]]};
       While[And[Length[stack]>0,!term],
             st=First[stack];
             stack=Rest[stack];
             Do[nx=x[[3,st,i,2]];
                pl=Position[y[[3,map[1,st]]],x[[3,st,i,1]]];
                If[Length[pl]==0, term=True,
                ny=y[[3,map[1,st],pl[[1,1]],2]];
                If[map[1,nx]==0,
                   map[1,nx]=ny;
                   map[2,ny]=nx;
                   PrependTo[stack,nx],
                   
                   If[map[1,nx]!=ny,  term=True]]],
               {i,Length[x[[3,st]]]}]];
      !Or[term,x[[5]]!=Map[map[2,#]&,y[[5]]]]]]
      
(* Set Operations On Automata *)

ComplementAutomaton[x:Automaton[NDFA,___]] :=
  ComplementAutomaton[NDFAToDFA[x]]
ComplementAutomaton[x:Automaton[DFA,___]]:=
  Automaton[x[[1]],x[[2]],x[[3]],x[[4]],
            Complement[Range[Length[x[[2]]]],x[[5]]],
            x[[6]],x[[7]]]
ReverseAutomaton[x:Automaton[DFA,___]] :=
  ReverseAutomaton[DFAToNDFA[x]]
ReverseAutomaton[x:Automaton[NDFA,___]] :=
  Block[{f,g},
        Automaton[NDFA,
                  If[(f=Length[x[[5]]])>1, 
                      Append[x[[2]],g=newStateName[x[[2]]]],
                             x[[2]]],
                      Join[reverseTransitions[x[[3]]],
                           {{{epsilon,x[[5]]}}}],
                      If[f>1, g, x[[5,1]]],
                      {x[[4]]},
                      If[f>1,{},x[[6]]],
                      x[[7]]]]
newStateName[l_List]:=
  Block[{t=Length[l]+1},
        While[MemberQ[l,t], t++];
        t]
reverseTransitions[l_List]:=
  Block[{temp,tran=Table[{},{Length[l]}],i,j,k},
        temp[s_,i_] := 0;
        Do[ If[temp[l[[i,j,2,k]],l[[i,j,1]]] == 0,
               temp[l[[i,j,2,k]],l[[i,j,1]]] = Length[tran[[i]]]+1;
               PrependTo[tran[[l[[i,j,2,k]]]],
                         {l[[i,j,1]],{i}}],
               tran[[l[[i,j,2,k]],
                    temp[l[[i,j,2,k]],
                    l[[i,j,1]]],2]] = 
                    Union[tran[[l[[i,j,2,k]],temp[l[[i,j,2,k]],
                             l[[i,j,1]]],2]],{i}]],
           {i,Length[l]},{j,Length[l[[i]]]},{k,Length[l[[i,j,2]]]}];
           tran]
 
UnionAutomata[x:Automaton[DFA,___],y_Automaton]:=
    UnionAutomata[DFAToNDFA[x],y]
UnionAutomata[x_Automaton,y:Automaton[DFA,___]]:=
    UnionAutomata[x,DFAToNDFA[y]]
UnionAutomata[x:Automaton[NDFA,___],
              y:Automaton[NDFA,___]]:=
     Block[{l},
           l=Length[x[[2]]];
           Automaton[
             NDFA,
             Range[1+l+Length[y[[2]]]],
             Join[{{{epsilon,{x[[4]]+1,y[[4]]+1+l}}}},
                  Map[{#[[1]],Map[(#+1)&,#[[2]]]}&,x[[3]],{2}],
                  Map[{#[[1]],Map[(#+1+l)&,#[[2]]]}&,y[[3]],{2}]],
             1,
             Union[Map[(#+1)&,x[[5]]],Map[(#+1+l)&,y[[5]]]],
             UnionEmbedding[x[[6]],y[[6]]],
             Union[x[[7]],y[[7]]]]]
UnionEmbedding[{},e2_] := {}
UnionEmbedding[e1_,{}] := {}
UnionEmbedding[e1_,e2_] :=
   Join[ {{0.1,0.5}},
         Map[{#[[1]] 0.7 + 0.1,
              (#[[2]] + 1)/2}&, NormalizeVertices[e1]],
         Map[{#[[1]] 0.7 + 0.1,
              #[[2]]/2}&, NormalizeVertices[e2]]] 
IntersectAutomata[x:Automaton[DFA,___],
                  y:Automaton[DFA,___]]:=
 Block[{trans,stateNum,i,j,i1,j1,px,py,xlen,ylen,sink},
 
       stateNum[i_,j_]:=(i-1) ylen + j;
       ylen=Length[y[[2]]];
       xlen=Length[x[[2]]];
       sink= ylen xlen +1;
       trans=Table[{},{sink}];
       Do[syms=If[Length[x[[3,i]]] + Length[y[[3,j]]]==0, {},
                  Union[Transpose[Union[x[[3,i]],y[[3,j]]]][[1]]]];
                  
          trans[[stateNum[i,j]]] =
                Map[(px=Position[x[[3,i]],syms[[#]]];
                     py=Position[y[[3,j]],syms[[#]]];
                     {syms[[#]],
                       If[Length[px] Length[py]==0,
                          sink,
                          stateNum[x[[3,i,px[[1,1]],2]],
                                   y[[3,j,py[[1,1]],2]]]]})&,
                         
                  Range[Length[syms]]],
         {i,xlen},{j,ylen}];
         trans[[sink]]=Map[{#,sink}&,Union[x[[7]],y[[7]]]];
    Automaton[DFA,
              Range[sink],
              trans,
              stateNum[x[[4]],y[[4]]],
              Flatten[Table[stateNum[x[[5,i]],y[[5,j]]],
                   {i,Length[x[[5]]]},{j,Length[y[[5]]]}]],
              {},
              Union[x[[7]],y[[7]]]]]
IntersectAutomata[x:Automaton[NDFA,___],y:Automaton[DFA,___]] :=
    IntersectAutomata[x,DFAToNDFA[y]]
IntersectAutomata[x:Automaton[DFA,___],y:Automaton[NDFA,___]] :=
    IntersectAutomata[DFAToNDFA[x],y]
IntersectAutomata[x1:Automaton[NDFA,___],
                  y1:Automaton[NDFA,___]]:=
 Block[{trans,stateNum,i,j,i1,j1,xlen,ylen,sink,px,py,syms,
        x=RemoveEpsilonMoves[x1],y=RemoveEpsilonMoves[y1]},
       stateNum[i_,j_]:=(i-1) ylen + j;
       ylen=Length[y[[2]]];
       xlen=Length[x[[2]]];
       sink= ylen xlen +1;
       trans=Table[{},{sink}];
       Do[syms=If[Length[x[[3,i]]] + Length[y[[3,j]]]==0, {},
                  Union[Transpose[Union[x[[3,i]],y[[3,j]]]][[1]]]];
                 
          trans[[stateNum[i,j]]] =
                Map[(px=Position[x[[3,i]],syms[[#]]];
                     py=Position[y[[3,j]],syms[[#]]];
                     {syms[[#]],
                       If[Length[px] Length[py]==0,
                          {sink},
                          Union[Flatten[
                            Table[stateNum[x[[3,i,px[[1,1]],2,i1]],
                                           y[[3,j,py[[1,1]],2,j1]]],
                                 {i1,Length[x[[3,i,px[[1,1]],2]]]},
                                 {j1,Length[y[[3,j,py[[1,1]],2]]]}]]]]})&,
                         
                  Range[Length[syms]]],
         {i,xlen},{j,ylen}];
         trans[[sink]]=Map[{#,{sink}}&,Union[x[[7]],y[[7]]]];
    Automaton[NDFA,
              Range[sink],
              trans,
              stateNum[x[[4]],y[[4]]],
              Flatten[Table[stateNum[x[[5,i]],y[[5,j]]],
                   {i,Length[x[[5]]]},{j,Length[y[[5]]]}]],
              {},
              Union[x[[7]],y[[7]]]]]
KleeneAutomaton[x:Automaton[DFA,___]] :=
  KleeneAutomaton[DFAToNDFA[x]]
KleeneAutomaton[x:Automaton[NDFA,___]] :=
  Block[{l},
    Automaton[
      NDFA,
      x[[2]],
      MapIndexed[If[MemberQ[x[[5]],#2[[1]]],
                    If[Length[(l=Position[#1,epsilon])] == 0,
                       Append[#1,{epsilon,{x[[4]]}}],
                       ReplacePart[#1,
                                   Union[Append[#1[[l[[1,1]],2]],
                                                x[[4]]]],
                                   {l[[1,1]]}]],
                                   #1]&,
                         x[[3]]],
              x[[4]],
              Union[Append[x[[5]],x[[4]]]],
              x[[6]],
              x[[7]]]]
              
(* Automata Minimization Function *)

MinimizeAutomaton[x:Automaton[NDFA,___]] :=
   MinimizeAutomaton[NDFAToDFA[x]]
MinimizeAutomaton[z_Automaton]:=
  Block[{temp,nsmap,ns,stt,i,x},
        x=CompleteTransitionFunction[z];
        
        ns=FindEquivalenceClasses[x];
        
        stt=Position[ns,x[[4]]][[1,1]];
        ns=Prepend[Drop[ns,{stt,stt}],ns[[stt]]];
        nsmap[y_]:= nsmap[y]=Position[ns,y][[1,1]];
        temp=If[Length[ns] == Length[x[[2]]], x,
                Automaton[DFA,Range[Length[ns]],
                          Table[Union[Map[{#[[1]],nsmap[#[[2]]]} &,
                                  Apply[Union, 
                                        x[[3,#]]& /@  ns[[i]]]]],
                               {i,Length[ns]}],
                          nsmap[x[[4]]],
                          Union[Map[nsmap[#]&,x[[5]]]],
                          {},x[[7]]]];
        stt=ReachableStates[temp];
        Do[If[!MemberQ[stt,i],
              DeleteState[temp,i]],
              {i,Length[temp[[2]]]}];
        temp]
CompleteTransitionFunction[x_Automaton] :=
 Block[{i,l,
        f=Length[Flatten[x[[3]],1]] == Length[x[[2]]] Length[x[[7]]]},
  Automaton[DFA,If[f, 
                   x[[2]], 
                   Append[x[[2]],l=newStateName[x[[2]]]]],
            Join[Table[Join[x[[3,i]],Map[{#,l}&,
                                    Complement[x[[7]],
                                    If[Length[x[[3,i]]]==0, {},
                                       Transpose[x[[3,i]]][[1]]]]]],
                       {i,Length[x[[2]]]}],    
                 If[!f,Map[{#,l}&,x[[7]]],{}]],
            x[[4]],x[[5]],If[f,x[[6]],{}],x[[7]]]]
                  
FindEquivalenceClasses[aut:Automaton[DFA,___]]:=Block[
   {k=3,b,br,inv,l,term=False,ch,ca,ci,i,j,
    kk,al=Length[aut[[7]]],ns},
   
   ns[s_,i_] := ns[s,i] = NextState[s,i,aut];
   
   (* Step 1 *)
   
   inv=InvertTransitions[aut];
  
   (* Step 2,3 *)
   
   b={aut[[5]],Complement[Range[Length[aut[[2]]]],aut[[5]]]};
   
   br=Table[Select[b[[j]],
                   Length[Position[inv[[#]],aut[[7,i]]]]>0 &],
            {j,2},{i,al}];
      
   (* Step 4 *)
   
   l=Table[If[Length[br[[1,i]]] <= Length[br[[2,i]]], {1}, {2}],
           {i,al}];
   
   While[And[!term,Length[b]<Length[aut[[2]]]],
   
   (* Step 5 *)
   
     ch= ChooseSymbolAndClass[1,l];
     If[Length[ch]==0, term=True,
       ca=ch[[1]];
       ci=ch[[2]]; 
   
   (* Step 6 *)
   
      l[[ca]]=Rest[l[[ca]]];
      kk=k; 
    
   (* Step 7 *)
    
      Do[ If[Step7Condition[b[[j]],aut[[7,ca]],br[[ci,ca]],aut],
          
           (* Step 7a *)
           
           bp = Select[b[[j]],
                       MemberQ[br[[ci,ca]],ns[#,aut[[7,ca]]]]&];
           bpp = Complement[b[[j]],bp];
           
           (* Step 7b *)
      
           b[[j]]=If[Length[bp]>0,bp,bpp];
           If[Length[bpp] Length[bp] >0,
           AppendTo[b,bpp];
           br[[j]]=Table[
                 Select[b[[j]],
                  (Length[Position[inv[[#]],aut[[7,ii]]]]>0)&],
                 {ii,al}];
           AppendTo[br,Table[
                 Select[b[[k]],
                 (Length[Position[inv[[#]],aut[[7,ii]]]]>0)&],
                 {ii,al}]];
           
           (* Step 7c *)
          
           Do[l[[aa]]=Union[AppendTo[l[[aa]],
           If[And[!MemberQ[l[[aa]],j],Length[br[[j,aa]]]>0,
                  Length[br[[j,aa]]] <= Length[br[[k,aa]]]],
             j,k]]],
             {aa,al}];
             
            (* Step 7d *)
           
           k++]],
         {j,kk-1}]]];
   
   b]
Step7Condition[se_,sym_,dse_,aut_]:=
   If[Length[se]==0, False,
      If[Apply[Times,
         Map[If[MemberQ[dse,aut[[3,First[se],#[[1]],2]]],0,1]&,
             Position[aut[[3,First[se]]],sym]]]==0, 
         True,
         Step7Condition[Rest[se],sym,dse,aut]]]
ChooseSymbolAndClass[count_,l_]:=
       If[Length[l]==0, {},
          If[Length[l[[1]]]==0, 
             ChooseSymbolAndClass[count+1,Rest[l]],
             {count,l[[1,1]]}]]

InvertTransitions[aut_]:=
    Block[{pos,syms,rev,p,i},
          Table[
                pos=Position[aut[[3]],i];
                syms={};
                rev={};
                Map[(p=Position[syms,aut[[3,#[[1]],#[[2]],1]]];
                    If[Length[p]>0,AppendTo[rev[[p[[1,1]]]],#[[1]]],
                    AppendTo[syms,aut[[3,#[[1]],#[[2]],1]]];
                    AppendTo[rev,{#[[1]]}]])&,
                    pos];
                If[Length[syms]>0,
                   Transpose[{syms,rev}],
                   {}],
               {i,Length[aut[[2]]]}]]
               
(* Regular Expressions Handling Functions *)

RegExp[x_String] := RegAtom[x]
RegExp[x_Symbol] := RegAtom[x]
RegExp[x:From[_,_]] := RegPlusFrom @@ StringRange[x[[1]],
                                                  x[[2]]]
RegExp[x_RP] := RegPlus @@ Map[RegExp[#]&,x]
RegExp[x_RC] := RegConcat @@ Map[RegExp[#]&,x]
RegExp[x:RK[y_]] := RegKleene[RegExp[y]]
RegExpQ[exp_]:=MemberQ[
                   {RegAtom,RegPlus,RegConcat,RegKleene,RegPlusFrom},
                    Head[exp]]
RegExp[x_?RegExpQ] := x
StringRange[x_String,y_String]:=
   Block[{asx,asy},
         asx=ToCharacterCode[x][[1]];
         asy=ToCharacterCode[y][[1]];
         If[And[IntegerQ[asx],IntegerQ[asy], asy >= asx],
              
            Table[RegAtom[FromCharacterCode[i]],
                                    {i,ToCharacterCode[x][[1]],
                                    ToCharacterCode[y][[1]]}],
            Message[StringRange::InvalidSpec]]]
Format[x_RegAtom]:= x[[1]]
Format[RegPlus[y___]]:= PrecedenceForm[Infix[{y}," + "],100]
Format[RegPlusFrom[y___]]:= PrecedenceForm[Infix[{y}," + "],100]
Format[RegConcat[y___]]:= PrecedenceForm[Infix[{y}," "],200]
Format[x_RegKleene]:= Infix[{x[[1]],Superscript["*"]},""]
EpsilonAutomata[]:=
       Block[{st=stateCounter},
             stateCounter++;
             {{st},{},st,{st},{{{0.5,0.25}},{1,1}},{}}]
AtomAutomata[sym_]:=
       Block[{st=stateCounter},
             stateCounter=stateCounter+2;
             {{st,st+1},{{st,sym,{st+1}}}, st,{st+1},
              {{{0.5,0.5},{1.5,0.5}},{2,1}},{sym}}]
BuildUnionAutomata[auts_]:=
  Block[{st=stateCounter,tauts=Transpose[auts],n=Length[auts],v,
         toty,tot0,dx,maxx=Max[Map[#[[5,2,1]]&,auts]]},
         toty=tot0=Sum[auts[[i,5,2,2]],{i,n}];
         stateCounter=stateCounter+2;
         {Range[st+1],
          Join[Map[{st,epsilon,{#}}&, tauts[[3]]],
               Flatten[Map[{#,epsilon,{st+1}}&, tauts[[4]],{2}],1],
               Apply[Join,tauts[[2]]]],
          st,{st+1},
          {Join[Join @@ 
                Table[(dx= (maxx - auts[[i,5,2,1]])/2.;
                       toty -= auts[[i,5,2,2]];
                       Map[{#[[1]] + dx + 1,
                            #[[2]] + toty}&,auts[[i,5,1]]]),
                      {i,n}],
               {{0.5,tot0/2.},{maxx+1.5,tot0/2.}}],{2+maxx,tot0}},
               Apply[Union,tauts[[6]]]}]
BuildConcatAutomata[auts_]:=
  Block[{tauts=Transpose[auts],n=Length[auts],totx=0,v,
         maxy=Max[Map[#[[5,2,2]]&,auts]]},
        {Range[stateCounter-1],
        Join[Apply[Join,tauts[[2]]],
             Table[{auts[[i,4,1]],epsilon,{auts[[i+1,3]]}},
                   {i,n-1}]],
        auts[[1,3]],auts[[Length[auts],4]],
        {Join @@ Table[(dy = (maxy - auts[[i,5,2,2]])/2. ;
                      v=Map[{#[[1]] + totx, #[[2]]+dy}&,auts[[i,5,1]]];
                      totx += auts[[i,5,2,1]];
                      v),
                      {i,n}],{totx,maxy}},
        Apply[Union,tauts[[6]]]}]
BuildKleeneAutomata[aut_]:=Block[{st=stateCounter},
            stateCounter=stateCounter+2;
           {Range[st+1],
            Join[aut[[2]],{{st,epsilon,{st+1}},
                           {st,epsilon,{aut[[3]]}},
                 {aut[[4,1]],epsilon,{st+1}},
                 {aut[[4,1]],epsilon,{aut[[3]]}}}],
            st,{st+1},
            {Join[Map[{#[[1]] +1, #[[2]]}&, aut[[5,1]]],
                 {{0.5,aut[[5,2,2]]/2.},
                  {aut[[5,2,1]]+1.5,aut[[5,2,2]]/2.}}],
                  {aut[[5,2,1]]+2, aut[[5,2,2]]}},
                  
            aut[[6]]}]
RegExpToAutomaton[exp_?RegExpQ]:=Block[{temp},
       stateCounter=1;
       temp=BuildAutomata[exp];
       MakeAutomaton @@ Join[{NDFA},
        Take[temp,4],
             {temp[[6]],Embedding->temp[[5,1]]}]]
BuildAutomata[x_RegAtom] := If[SameQ[x[[1]],epsilon],
                               EpsilonAutomata[],
                               AtomAutomata[x[[1]]]]
BuildAutomata[x_RegPlusFrom | x_RegPlus] :=
              BuildUnionAutomata[Map[BuildAutomata[x[[#]]]&,
                                 Range[Length[x]]]]
BuildAutomata[x_RegConcat] :=
              BuildConcatAutomata[Map[BuildAutomata[x[[#]]]&,
                                  Range[Length[x]]]]
BuildAutomata[x_RegKleene] :=
              BuildKleeneAutomata[BuildAutomata[x[[1]]]]
InRegSetQ[reg_?RegExpQ,st_List] :=
  AcceptedBy[RegExpToAutomaton[reg],st]
  
(* Automaton Drawing Functions *)

PrettyPrint[x:Automaton[Moore,___]]:=
   Block[{i,j,vv=Table[" ",{Length[x[[2]]]+1},{Length[x[[7]]]+1}]},
         Do[vv[[i+1,1]]=x[[2,i]]; 
            Map[(vv[[i+1,Position[x[[7]],#[[1]]][[1,1]]+1]]=
                 x[[2,#[[2]],1]])&,
                  x[[3,i]]],
            {i,Length[x[[2]]]}];
         Do[vv[[1,i+1]]=x[[7,i]],{i,Length[x[[7]]]}];
         
         Print[ "Automaton of type ",AutomatonType[x]];
         Print["Initial state is :",InitialState[x]];
         vv // MatrixForm]
PrettyPrint[x_Automaton]:=
   Block[{i,j,vv,
          syms=If[And[SameQ[x[[1]],NDFA],
                  Length[Position[x[[3]],epsilon]]>0],
                  Append[x[[7]],epsilon],x[[7]]]},
         vv=Table[" ",{Length[x[[2]]]+1},{Length[syms]+1}];
         Do[vv[[i+1,1]]=x[[2,i]]; 
            Map[(vv[[i+1,Position[syms,#[[1]]][[1,1]]+1]]=
                 If[And[ListQ[#[[2]]],Length[#[[2]]]==1],
                    x[[2,#[[2,1]]]], x[[2,#[[2]]]]])&,
                x[[3,i]]],
            {i,Length[x[[2]]]}];
         Do[vv[[1,i+1]]=syms[[i]],{i,Length[syms]}];
         
         Print[ "Automaton of type ",AutomatonType[x]];
         Print["Initial state is :",InitialState[x]];
         If[MemberQ[{DFA,NDFA},
              x[[1]]],Print["Final states are :",FinalStates[x]]];
         vv // MatrixForm]
ShowAutomaton[x_Automaton,opts___Rule,
              og_Symbol:False]:=
  Block[{pict,opt1,opt2,i,l=Length[x[[2]]]},
   opt1= Embedding /. {opts} /. Options[ShowAutomaton];
   opt2= Colored /. {opts} /. Options[ShowAutomaton];
   opt3= StateSize /. {opts} /. Options[ShowAutomaton];
   
   embedding= Switch[ opt1,
                  Automatic, If[Length[x[[6]]]>0,
                                x[[6]], CircularEmbedding[x]],
                  Spring, SpringEmbedding[x],
                  Grid,   GridEmbedding[x],
                  Circular, CircularEmbedding[x],
                  _List, opt1];
 
    
   Clear[self,stateAngles,labels];
   stateAngles[i_]:= {};
   labels[i_,j_]:={};
   self[i_] := False;
   
   stateAngles[x[[4]]]={{0,npi/2.}};
   
   mindistance =  MinimumStateDistance[embedding];
   size = mindistance opt3;
   If[x[[1]] =!= NDFA, 
    MapIndexed[If[#2[[1]]==#1[[2]],
              self[#2[[1]]] = True;
              
              AppendTo[labels[#2[[1]],#2[[1]]],#1[[1]]],
              
              AppendTo[stateAngles[#2[[1]]],
                      {#1[[2]],
                       npi/12. + 
                       angleCalc[#2[[1]],#1[[2]]]}];
              AppendTo[labels[#2[[1]],#1[[2]]],#1[[1]]];    
              AppendTo[stateAngles[#1[[2]]],
                      {-#2[[1]],
                      - npi/12. + 
                        angleCalc[#1[[2]],#2[[1]]]}]]&,
              x[[3]],{2}],
    MapIndexed[Table[If[#2[[1]]==#1[[2,i]],
              self[#2[[1]]] = True;
              AppendTo[labels[#2[[1]],#2[[1]]],#1[[1]]],
              AppendTo[stateAngles[#2[[1]]],
                      {#1[[2,i]],
                       npi/12. + angleCalc[#2[[1]],#1[[2,i]]]}];
              AppendTo[labels[#2[[1]],#1[[2,i]]],#1[[1]]];
              AppendTo[stateAngles[#1[[2,i]]],
                      {-#2[[1]],
                      - npi/12. + 
                      angleCalc[#1[[2,i]],#2[[1]]]}]],
                    {i,Length[#1[[2]]]}]&,
              x[[3]],{2}]];
   
   Do[ 
      stateAngles[i] = Sort[Union[stateAngles[i]], 
                           (#1[[2]]<#2[[2]])&],
      {i,Length[x[[2]]]}];
  arrange[l,npi/6.];
  pict=Join[Apply[Join,
         Table[Join[Switch[opt2,
                           False, {},
                           True, {Hue[(i-1.)/l,0.8,0.8]},
                           _List, {opt2[[i]]}],
                    {Circle[embedding[[i]],  size]},
                    If[MemberQ[x[[5]],i],
                      {Circle[embedding[[i]],size/1.5]},{}],
                      {Text[x[[2,i]],embedding[[i]]]},
                      arcsFrom[i, size],
                      If[self[i],SelfArc[i, size],{}]],
               {i,l}]],
          {RGBColor[0,0,0],StartSign[embedding[[x[[4]]]], size]}];


   If[og=!=False, pict,
      Show[Graphics[pict,  
           { PlotRange->Automatic, AspectRatio->Automatic}]]]]
               
angleCalc[i_,j_] :=
        FindRelativeAngle[embedding[[i,1]],embedding[[i,2]],
                          embedding[[j,1]],embedding[[j,2]]]
    
StartSign[{x_,y_},r_] := {Line[{{x,y+2r},{x,y+r}}],
                          Polygon[{{x,y+r},{x- r/3.,y+3. r/2},
                                   {x+ r/3.,y+3. r/2}}] }
arrange[n_,pad_] :=
  Block[{dis,an,i,j},
    Do[lenstat=Length[stateAngles[i]];
       an=False;
       dis=npi/6.;
       Clear[stan];
       Table[stan[j,1]=stateAngles[i][[j,1]];
             stan[j,2]=stateAngles[i][[j,2]],
             {j,lenstat}]; 
       tab=Table[{stan[j,2]-pad,stan[j,2]+pad},{j,lenstat}];
      
    While[And[!an,dis>0],
          an=try10[dis,pad];
          dis -= npi/36.];
          
    stateAngles[i] = Table[{stan[j,1],stan[j,2]},{j,lenstat}],
    
      {i,n}]]
try10[dis_,pad_]:=
  Block[{t,i,j},
   Do[ t=True;
    Do[If[-pad < (y=stan[i+1,2]-stan[i,2]) < dis,
          pert[i+1,i,dis-y];t=False,
          If[-pad < (y=stan[i+1,2]+2. npi-stan[i,2]) < dis,
            pert[i+1,i,dis-y];t=False]],
             
       {i,lenstat-1}];
    If[-pad < (y=stan[1,2]-stan[lenstat,2]) < dis,
          pert[1,lenstat,dis-y];t=False,
          If[-pad < (y=stan[1,2]+2. npi-stan[lenstat,2]) < dis,
            pert[1,lenstat,dis-y];t=False]],
       
       {j,3}];
       
       t]
pert[i_,j_,del_] := 
  Block[{prev=stan[i,2],post=stan[j,2]},
        stan[i,2] = Mod[Min[stan[i,2]+del/2., tab[[i,2]]],
                      2. npi];
        stan[j,2] = Mod[Max[stan[j,2]-del/2., tab[[j,1]]],
                      2. npi];
        If[prev<stan[i,2],
           tab[[i]] -= 2. npi];
        If[post>stan[j,2],
           tab[[j]] += 2. npi]]
          
(* Embedding Calculating Functions *)

SpringEmbedding[x_Automaton, step_:10, inc_:0.15] :=
  Block[{new,old,g,n=Length[x[[2]]],i,u,g1},
        g= ToWeightedGraph[x[[2]],x[[3]],x[[4]],x[[7]]];
       
        g1={g,InitialEmbedding[x]};
    
        old=new=g1[[2]];
        Do[ 
          Do[ 
             new[[u]] = old[[u]] + inc * CalculateForce[u,g1,old],
             {u,n}];
             old=new,
             {i,step} ];
             new]
CalculateForce[u_Integer,g_,em_List] :=
 Block[{n=Length[g[[1]]],stc=0.25,gr=10.0,e=g[[1]],
        f={0.0,0.0},spl=1.0,v,dsquared,
        conn},
      Do [ 
        dsquared= Max[0.001,Apply[Plus, (em[[u]]-em[[v]])^2]];
        f+=If[e[[u,v]]==0,(gr/dsquared)
             (em[[u]]-em[[v]]), 
             - stc Log[dsquared/spl] (em[[u]]-em[[v]])],
             {v,n}];
             f]
InitialEmbedding[x_,type_:Circular]:= 
   Switch[ type,
           Circular,  CircularEmbedding[x],
           Square,    SquareEmbedding[x]]
GridEmbedding[x_Automaton] := 
   Block[{marked=Table[0,{Length[x[[2]]]}],
          state,stack,counter=1,ns,diagonals,sizex,sizey,i,j},
          sizex=N[Floor[Sqrt[Length[x[[2]]]-1]]+1];
          sizey=N[Ceiling[Length[x[[2]]]/sizex]];
          diagonals=Sort[Flatten[Table[{i,j},
                        {i,sizey},{j,sizex}],1],
                              (#1[[1]]+#1[[2]]<
                               #2[[1]]+#2[[2]])&];
                            
         marked[[x[[4]]]]=1;
         stack={x[[4]]};
         While[Length[stack]>0,
              state=First[stack];
              stack=Rest[stack];
              Map[If[marked[[#]]==0,
                     counter++;
                     marked[[#]]=counter;
                     PrependTo[stack,#]]&,
                  NextPossibleStates[x,state]];
              If[Length[stack]==0,
                 stack=If[Length[ns=Position[marked,0]]>0,
                          (marked[[ns[[1,1]]]]= ++counter;
                           {ns[[1,1]]}),{}]]];
         Map[{#[[1]],1-#[[2]]}&,
                 Table[diagonals[[marked[[i]]]],
                       {i,Length[x[[2]]]}] / sizex]]
SquareVertices[n_Integer] := 
 Block [{s=Ceiling[N[Sqrt[n]]],i},
  Table[{Mod[i,s],s-Quotient[i,s]}+
         {Random[],Random[]} /3 ,{i,0,n-1}]]
CircularEmbedding[x_Automaton] := 
   Block[{temp=CircularEmbedding[Length[x[[2]]]],min},
         min=Position[temp,{Min[Transpose[temp][[1]]],_}][[1,1]];
         If[x[[4]]>min, RotateRight[temp,x[[4]]-min],
                        RotateLeft[temp, min-x[[4]]]]]
CircularEmbedding[n_Integer]:=
  Block[{i,x=N[2. npi/n]},
   Chop[Table[N[{(Cos[x i]), (Sin[x i])}],{i,n}]]]
ToWeightedGraph[st_,trans_,init_,syms_]:=
  Block[{pp,i,j,edges},
  
    edges[i_,j_] := 0;
    
    Do[ If[ListQ[trans[[i,j,2]]],
           Map[ (edges[i,#]++) &, trans[[i,j,2]]],
           edges[i,trans[[i,j,2]]]++],
      {i,Length[st]},{j,Length[trans[[i]]]}];
    Table[ edges[i,j],{i,Length[st]},{j,Length[st]}]]
MinimumStateDistance[v_List] :=
 Block[{i,j},
  If[Length[v]==1, 1,
  Min[Table[If[i==j,1,
              N[Sqrt[Apply[Plus,(v[[i]]-v[[j]])^2]]]],
      {i,Length[v]},{j,i-1}]]]]
(* Arc Drawing Functions *)

Up[i_,j_] := Up[i,j]=If[Or[embedding[[j,1]]>embedding[[i,1]],
                          And[embedding[[j,1]]==embedding[[i,1]],
                          embedding[[i,2]]>embedding[[j,2]]]],
                          1,-1]
arcsFrom[i_,r_] :=
  Block[{l,mi= -i},
    Flatten[
        Map[(l=Position[stateAngles[#[[1]]],{mi,_}][[1,1]];
            
            DrawEdge[calcPoint[embedding[[i,1]],embedding[[i,2]],
                               #[[2]],r],
                     calcPoint[embedding[[#[[1]],1]],
                               embedding[[#[[1]],2]],
                               stateAngles[#[[1]]][[l,2]],r],
                     MakeArcLabel[labels[i,#[[1]]]],
                     r,npi/3.,#[[1]],
                     stateAngles[#[[1]]][[l,2]]])&,
                   
            Select[stateAngles[i],(#[[1]]>0)&]],
            1]]
calcPoint[cx_,cy_,ang_,r_] :=
   {calcx[cx,ang,r],calcy[cy,ang,r]}
calcx = Compile[{cx,ang,r},
                cx+ r Cos[ang]]
calcy = Compile[{cy,ang,r},
                cy+ r Sin[ang]]
DrawEdge[{x1_,y1_},{x2_,y2_},lab_,r_,angle_,aj_,ang3_]:=
  Block[{xc,yc,ang,cen,baset,angt,ang1,
         ang2,c,r1,p1,angcenter,h1,up,rad,circrad,arrowang},
  
        up= If[Or[x2>x1,And[x1==x2,y1>y2]],1,-1];
        r1=distance[x1,y1,x2,y2]/(2. Sin[angle/2.]);
        cen=FindCenter[x1,y1,x2,y2,angle,r1,up];
        xc=cen[[1]]; 
        yc=cen[[2]];
        circrad = distance[x2,y2,xc,yc];
        
        ang1=FindRelativeAngle[xc,yc,x1,y1];
        ang2=FindRelativeAngle[xc,yc,x2,y2];
        arrowang = npi/54. Sqrt[mindistance/circrad];
        
        ang=If[ang1<ang2,
               If[ang2-ang1<npi,
                  angt= ang2 - arrowang ;
                  {ang1,ang2},
                  angt = ang2 + arrowang ;
                  {ang2, ang1 + 2. npi}],
               If[ang1-ang2 < npi,
                  angt= ang2 + arrowang ;
                  {ang2,ang1},
                  angt = ang2 - arrowang; 
                  {ang1, ang2 + 2. npi}]];
                
        baset=calcPoint[xc,yc,angt,circrad];
    
        p1=calcTrig[baset[[1]],baset[[2]],x2,y2];
      
        angcenter=Mod[(ang[[1]]+ang[[2]])/2, 2. npi];
        h1=If[x1==x2, { up, 0 },
              If[y1==y2, {0, up},
                 Switch[Floor[angcenter/npi/2.],
                        0, {1,1},
                        1, {-1,1},
                        2, {-1,-1},
                        3, {1,-1}]]]; 
                     
        rad= r1/ (2 Sin[angle/2]); 
               
        {Polygon[{p1[[1]],{x2,y2},p1[[2]]}],
         Circle[{xc,yc},r1/(2 Sin[angle/2]),ang],
         Text[lab,{xc+  rad Cos[angcenter], 
                   yc+ rad Sin[angcenter]},h1]}]
calcTrig[{xb_,yb_},{xp_,yp_}] := calcTrig[xb,yb,xp,yp]
calcTrig[xb_,yb_,xp_,yp_] := 
  Block[{m,n,r,x1,y1},
        r=distance[xb,yb,xp,yp] / Tan[npi/3.];
        m = (xb-xp) / (yp-yb);
        n = ArcTan[m];
        x1 = xb - r Cos[n];
        y1 = yb - r Sin[n];
        {{x1,y1},{2 xb - x1, 2 yb - y1}}]
SelfArc[i_,r_] :=
   Block[{angle,temp,temp1,pos,centerDistance=1.4 r,
          ang1,ang2,labelAngle,j,x=embedding[[i,1]],
          y=embedding[[i,2]],tanpoint,baset,arcCenter,p1},
          
          angle=Switch[
                 Length[stateAngles[i]],
                 0, npi,
                 1, Mod[stateAngles[i][[1,2]]+ npi, 2. npi],
                 _, temp=Sort[Map[Mod[#[[2]],2. npi]&,
                                   stateAngles[i]]];
                    temp1=Table[
                                If[j==1, 
                                   2. npi+temp[[1]]-Last[temp],
                                   temp[[j]]-temp[[j-1]]],
                               {j,Length[temp]}];
                                     
                 pos=Position[temp1,Max[temp1]][[1,1]];
                 angle=N[If[pos==1, 
                         (temp[[1]]+Last[temp])/2. - npi,
                         (temp[[pos]]+temp[[pos-1]])/2]]];
         
          labelAngle= Mod[angle-npi/2.,2. npi];
          ang1=N[angle- 7. npi / 6.];
          ang2=N[angle- 5. npi / 6.];
          arcCenter = calcPoint[x,y,angle,centerDistance];
          baset = calcPoint[arcCenter[[1]],arcCenter[[2]],
                            ang2 + npi/4., r/2.];
          
          tanpoint= calcPoint[arcCenter[[1]],arcCenter[[2]],
                              ang2,r/2.];    
          p1 = calcTrig[baset,tanpoint];
          {Circle[arcCenter,
                  r/2,{ang2,N[ang1+2 npi]}],
           Polygon[{p1[[1]],tanpoint,p1[[2]]}],
           Text[MakeArcLabel[labels[i,i]],
                N[{x+ centerDistance Cos[angle] + 
                   r/2 Cos[labelAngle],
                   y+ centerDistance Sin[angle] +
                   r/2 Sin[labelAngle]}],
                  Switch[Floor[Mod[ angle - npi/2, 2. npi]/ (npi/2.)],
                         0, {-1,-1},
                         1, {1,-1},
                         2, {1,1},
                         3, {-1,1}] ]}]
MakeArcLabel[labs_] := 
     Block[{lab=Table[",",{2 Length[labs]-1}],i},
           Do[lab[[2 i -1]]=If[labs[[i]]===epsilon,"e",
                            ToString[labs[[i]]]],
             {i,Length[labs]}];
           Apply[StringJoin,lab]]  
distance=Compile[{x1,y1,x2,y2},
                 Sqrt[(x1-x2)^2+(y1-y2)^2]]
FindCenter[x1_,y1_,x2_,y2_,angle_,r_,up_]:=
  Block[{aa,bb,cc,x0,y0,mm,nn,res,v},
  
  Which[x1==x2,
           mm= r Cos[angle/2];
           y0= (y1+y2)/2;
           {x1 - up mm,y0},
           
       y1==y2,
           mm= r Cos[angle/2];
           x0=(x1+x2)/2;
           {x0,y1- up mm},
           
       True, 
            x0=(x1+x2)/2; y0=(y1+y2)/2;
            mm=(x1-x2)/(y2-y1);
            nn=y0- mm x0;
            aa= 1 + mm^2;
            bb= 2 mm nn - 2 x0 -2 mm y0;
            cc= nn^2 -  r^2 Cos[angle/2]^2 +x0^2 -2 nn y0 + y0^2;
            res=SolveEqn[aa,bb,cc];
            v=Transpose[{res,Map[(mm # + nn)&,res]}];
            If[v[[1,2]]> (y1+y2)/2., 
               If[up==1,
                  v[[2]], v[[1]]],
               If[up==1,
                  v[[1]], v[[2]]]]]]
SolveEqn[a_,b_,c_]:= 
   Block[{disc=Sqrt[b^2 -4 a c]},
         {(-b+disc)/(2 a),(-b-disc)/(2 a)}]
FindRelativeAngle = Compile[{x,y,x1,y1},
  Block[{m,at},
        If[x==x1,
           If[y1<y, -npi/2., npi/2.],
           m=(y1-y)/(x1-x);
           at=ArcTan[m];
           Mod[If[x1 > x, at, at+ npi],2 npi-0.0001]]]]
End[]
EndPackage[]
