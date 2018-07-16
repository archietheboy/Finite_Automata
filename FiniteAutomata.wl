(* ::Package:: *)


BeginPackage["FiniteAutomata`"];

Unprotect["FiniteAutomata``*"];
ClearAll["FiniteAutomata``*", "FiniteAutomata``Private`*"];

(******************************************************************************)

FiniteAutomata;
State;
DFA::usage = "DFA[_Association] represents an object of a Deterministic Finite Automata";
NFA::usage = "NFA[_Association] represents an object of a Non-Deterministic Finite Automata";
AutomataQ::usage = "returns TRUE if the argument is a Finite Automata object.";
ChangeDFA::usage = "Changes property values of a Finite Automata object."
CreateTransitionFunction::usage = "Creates the transition function given the rules, the alphabet and the automata states."
CreateDFA::usage = "Creates the DFA object."
ValidSymbolQ::usage = "returns TRUE if symbols or list of symbols belong the the alphabet."
ValidStateQ::usage = "returns TRUE if state or list of states belong FA available states."
StringApply::usage = "pass the string through the finite automata object."
LanguageQ::usage = "returns TRUE if the string is a member of the Automata's language."

(******************************************************************************)

Begin["`Private`"];


(******************************************************************************)

FiniteAutomata::INVsymbols = "The symbols `1` are not a subset of the FA's alphabet `2`.";
FiniteAutomata::INVsymbol = "The symbol `1` is not a member of the FA's alphabet `2`.";
FiniteAutomata::INVstates = "The states `1` are not a subset of the FA's states `2`.";
FiniteAutomata::INVstate = "The state `1` is not a member of the FA's states `2`.";

(******************************************************************************)

DFA/:DFA[fa_]["Alphabet"] := fa["Alphabet"]
DFA/:DFA[fa_]["States"] := fa["States"]
DFA/:DFA[fa_]["TransitionFunction"] := fa["TransitionFunction"]
DFA/:DFA[fa_]["StartingState"] := fa["StartingState"]
DFA/:DFA[fa_]["AcceptingStates"] := fa["AcceptingStates"]
DFA/:DFA[fa_]["CurrentState"] := fa["CurrentState"]
DFA/:DFA[fa_]["TransitionFunction", "Table"] := TransitionFunction[fa]

(******************************************************************************)

Options[State] = {"StartingState" -> False, "AcceptingState" -> False};
State/:(h:(SameQ|Equal))[State[x_, y___], State[a_, b___]] := h[x, a]
State/:MemberQ[l:{_State..}, a_State] := MemberQ[l[[All, 1]], a]
State/:SubsetQ[l:{_State..}, a:{_State..}] := SubsetQ[l[[All, 1]], a[[All, 1]]]
State/:AutomataChangeOptions[State[x_, y___]] := SubsetQ[l[[All, 1]], a[[All, 1]]]

(******************************************************************************)

DFA/:AutomataQ[_DFA] = True;
NFA/:AutomataQ[_NFA] = True;
AutomataQ[___] = False;

(******************************************************************************)

ChangeDFA::INVkey="The keys `1` are not a valid for the Finite Automata object.";
DFA/:ChangeDFA[DFA[fa_], rule_Rule] := 
	If[MemberQ[Keys[fa], rule[[1]]], 
		DFA[Join[fa, <|rule|>]], 
		Message[ChangeDFA::INVkey, {rule[[1]]}]; $Failed
	]
DFA/:ChangeDFA[DFA[fa_], assoc_Association] := 
	If[SubsetQ[Keys[fa], Keys[assoc]], 
		DFA[Join[fa, assoc]], 
		Message[ChangeDFA::INVkey, Keys[assoc]]; $Failed
	]

(******************************************************************************)

Options[CreateTransitionFunction] = {"Alphabet" -> Automatic, "States" -> Automatic, "Autocomplete" -> True};
CreateTransitionFunction::INVform = "The transition function `1` does not have the correct form.";
CreateTransitionFunction[d:{Rule[{_State, _String}, _State]..}, alph:{_String..}, Q:{_State..}]:=
	Module[{tfunc, inputs, states},
	
		states = Union[d[[All, 1, 1]], d[[All, 2]]];
		If[!SubsetQ[Q, states], Message[FiniteAutomata::INVsymbols,states,Q]; Return[$Failed]];
		
		inputs = d[[All,1,2]];
		If[!SubsetQ[alph, inputs], Message[FiniteAutomata::INVstates, inputs, alph]; Return[$Failed]];
		
		tfunc = Association/@GroupBy[d, (#[[1, 1]]&) -> ((#[[1, 2]] -> #[[2]])&)];
	
		If[OptionValue["Autocomplete"], 
			tfunc = 
				KeyValueMap[
					(#1 -> Function[comp, If[comp=={}, #2, Join[#2, AssociationThread[comp -> #1]]]][Complement[alph, Keys[#2]]])&, 
					tfunc]
		];
		
		Sort@Association@tfunc
	]
CreateTransitionFunction[d:{Rule[{_String, _String}, _String]..}, alph:{_String..}, Q:{_State..}] :=
	CreateTransitionFunction[
		Replace[d, ({istate_String, input_String} -> ostate_String) :> ({State[istate], input} -> State[ostate]), {1}],
		alph, Q]
CreateTransitionFunction[d_, alph:{_String..}, Q:{_State..}] := 
	CompoundExpression[
		Message[CreateTransitionFunction::INVform, d], 
		Return[$Failed]
	]

(******************************************************************************)

CreateDFA::INVaccstate = "The accepting states `1` are not a subset of `2`.";
CreateDFA::INVinitstate = "The starting state `1` is not a member of `2`.";
CreateDFA[Q:{_State..}, alph:{_String..}, d_, q0_State, F:{_State..}] :=
	CompoundExpression[
		If[!SubsetQ[Q,F], Message[CreateDFA::INVaccstate, F, Q]; Return[$Failed]],
		If[!MemberQ[Q, q0], Message[CreateDFA::INVinitstate, q0, Q]; Return[$Failed]],
		With[{tfunc = CreateTransitionFunction[d, alph, Q]},
			If[tfunc === $Failed, Return[$Failed]];
			DFA[<|
				"States" -> Q,
				"Alphabet" -> Sort@alph,
				"TransitionFunction"-> tfunc,
				"StartingState" -> q0,
				"AcceptingStates" -> Sort@F,
				"CurrentState"->q0
				|>]
		]
	]
CreateDFA[Q:{_String..}, alph:{_String..}, d_, q0_String, F:{_String..}] :=
	CreateDFA[State/@Q, alph, d, State[q0], State/@F]

(******************************************************************************)

ValidSymbolQ[alph:{_String..}, a_String]/;(StringLength[a] == 1) := 
	If[MemberQ[alph, a], True, Message[FiniteAutomata::INVsymbol, a, alph]; False]
ValidSymbolQ[alph:{_String..}, x_String]/;(StringLength[x] > 1):= 
	If[SubsetQ[alph, Characters[x]], True, Message[FiniteAutomata::INVsymbols, Characters[x], alph]; False]

(******************************************************************************)

ValidStateQ[Q:{_State..}, s_State] := If[MemberQ[Q, s], True, Message[FiniteAutomata::INVstate, s, Q]; False]
ValidStateQ[Q:{_State..}, s:{_State..}] := If[SubsetQ[Q, s], True, Message[FiniteAutomata::INVstates, s, Q]; False]

(******************************************************************************)

iStringApply[fa_?AutomataQ, ""]:=fa
iStringApply[fa_DFA,x_String] := 
	With[{tfunc=fa["TransitionFunction"]},
		Fold[ChangeDFA[#1, "CurrentState" -> (tfunc[#1["CurrentState"], #2])]&, fa, Characters[x]]
	]

StringApply[fa_?AutomataQ,""] := fa
StringApply[fa_?AutomataQ, x_String] := If[ValidSymbolQ[fa["Alphabet"], x], iStringApply[fa, x], $Failed]
StringApply[fa_, l:{_String..}] := Map[StringApply[fa, #]&, l]
StringApply[fa_, a_, currentstate_State] := 
	If[ValidStateQ[fa["States"], currentstate], StringApply[ChangeDFA[fa, "CurrentState" -> currentstate], a], $Failed]

(******************************************************************************)

LanguageQ[fa_DFA,x_String]:=
	Function[ofa, 
		If[AutomataQ[ofa],
			If[MemberQ[ofa["AcceptingStates"], ofa["CurrentState"]], True, False], 
			$Failed]
	][StringApply[fa, x]]
LanguageQ[fa_DFA,l:{_String..}] := Map[LanguageQ[fa, #]&, l]

(******************************************************************************)


Scan[SetAttributes[#, {Protected, ReadProtected}]&,
     Select[Symbol /@ Names["FiniteAutomata``*"], Head[#] === Symbol &]];

End[];
EndPackage[];
