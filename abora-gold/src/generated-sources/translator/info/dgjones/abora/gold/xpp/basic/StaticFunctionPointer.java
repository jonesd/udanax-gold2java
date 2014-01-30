/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.xpp.basic;

import info.dgjones.abora.gold.java.AboraHeaper;
import info.dgjones.abora.gold.java.missing.smalltalk.AboraClass;
import info.dgjones.abora.gold.java.missing.smalltalk.Array;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.StaticFunctionPointer;
import java.io.PrintWriter;

/**
 * An object that represents in Smalltalk the equivalent of a pointer to a class static
 * function in C++. When it gets the invokeFunction:with:... message, it sends mySelector to
 * the class, with the arguments of the invokeFunction message. This does the same thing that
 * the translated code would do if this were in fact a function pointer.
 */
public class StaticFunctionPointer extends AboraHeaper {

	protected AboraClass myClass;
	protected String mySelector;
/*
Xanadu-Xpp-Basic.st:39:
Object subclass: #StaticFunctionPointer
	instanceVariableNames: '
		myClass {Class}
		mySelector {Symbol}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Xpp-Basic'!
*/
/*
Xanadu-Xpp-Basic.st:47:
StaticFunctionPointer comment:
'An object that represents in Smalltalk the equivalent of a pointer to a class static function in C++. When it gets the invokeFunction:with:... message, it sends mySelector to the class, with the arguments of the invokeFunction message. This does the same thing that the translated code would do if this were in fact a function pointer.'!
*/
/*
Xanadu-Xpp-Basic.st:128:
StaticFunctionPointer class
	instanceVariableNames: ''!
*/
public StaticFunctionPointer(AboraClass classx, String selector) {
	myClass = classx;
	mySelector = selector;
/*
Xanadu-Xpp-Basic.st:53:StaticFunctionPointer methodsFor: 'create'!
create: class {Class} with: selector {Selector}
	myClass := class.
	mySelector := selector.!
*/
}
public Object invokeFunction() {
	return myClass.perform(mySelector);
/*
Xanadu-Xpp-Basic.st:59:StaticFunctionPointer methodsFor: 'invoking'!
invokeFunction
	^myClass perform: mySelector!
*/
}
public Object invokeFunction(Object arg1) {
	return myClass.perform(mySelector, arg1);
/*
Xanadu-Xpp-Basic.st:63:StaticFunctionPointer methodsFor: 'invoking'!
invokeFunction: arg1
	^myClass perform: mySelector
		with: arg1!
*/
}
public Object invokeFunction(Object arg1, Object arg2) {
	return myClass.perform(mySelector, arg1, arg2);
/*
Xanadu-Xpp-Basic.st:68:StaticFunctionPointer methodsFor: 'invoking'!
invokeFunction: arg1 with: arg2
	^myClass perform: mySelector
		with: arg1
		with: arg2!
*/
}
public Object invokeFunction(Object arg1, Object arg2, Object arg3) {
	return myClass.perform(mySelector, arg1, arg2, arg3);
/*
Xanadu-Xpp-Basic.st:74:StaticFunctionPointer methodsFor: 'invoking'!
invokeFunction: arg1 with: arg2 with: arg3
	^myClass perform: mySelector
		with: arg1
		with: arg2
		with: arg3!
*/
}
public Object invokeFunction(Object arg1, Object arg2, Object arg3, Object arg4) {
	return myClass.perform(mySelector, arg1, arg2, arg3, arg4);
/*
Xanadu-Xpp-Basic.st:81:StaticFunctionPointer methodsFor: 'invoking'!
invokeFunction: arg1 with: arg2 with: arg3 with: arg4
	^myClass perform: mySelector
		with: arg1
		with: arg2
		with: arg3
		with: arg4!
*/
}
public Object invokeFunction(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5) {
	Array args;
	args = new Array(5);
	args.basicAtPut(1, arg1);
	args.basicAtPut(2, arg2);
	args.basicAtPut(3, arg3);
	args.basicAtPut(4, arg4);
	args.basicAtPut(5, arg5);
	return myClass.perform(mySelector, args);
/*
Xanadu-Xpp-Basic.st:89:StaticFunctionPointer methodsFor: 'invoking'!
invokeFunction: arg1 with: arg2 with: arg3 with: arg4 with: arg5
	| args |
	args := Array new: 5.
	args basicAt: 1 put: arg1.
	args basicAt: 2 put: arg2.
	args basicAt: 3 put: arg3.
	args basicAt: 4 put: arg4.
	args basicAt: 5 put: arg5.
	^myClass perform: mySelector
		withArguments: args!
*/
}
public Object invokeFunction(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6) {
	Array args;
	args = new Array(6);
	args.basicAtPut(1, arg1);
	args.basicAtPut(2, arg2);
	args.basicAtPut(3, arg3);
	args.basicAtPut(4, arg4);
	args.basicAtPut(5, arg5);
	args.basicAtPut(6, arg6);
	return myClass.perform(mySelector, args);
/*
Xanadu-Xpp-Basic.st:101:StaticFunctionPointer methodsFor: 'invoking'!
invokeFunction: arg1 with: arg2 with: arg3 with: arg4 with: arg5 with: arg6
	| args |
	args := Array new: 6.
	args basicAt: 1 put: arg1.
	args basicAt: 2 put: arg2.
	args basicAt: 3 put: arg3.
	args basicAt: 4 put: arg4.
	args basicAt: 5 put: arg5.
	args basicAt: 6 put: arg6.
	^myClass perform: mySelector
		withArguments: args!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(myClass.name());
	oo.print("::");
	oo.print(mySelector);
/*
Xanadu-Xpp-Basic.st:116:StaticFunctionPointer methodsFor: 'printing'!
printOn: oo
	oo << myClass name << '::' << mySelector!
*/
}
public String selector() {
	return mySelector;
/*
Xanadu-Xpp-Basic.st:121:StaticFunctionPointer methodsFor: 'accessing'!
selector
	^mySelector!
*/
}
public AboraClass staticClass() {
	return myClass;
/*
Xanadu-Xpp-Basic.st:124:StaticFunctionPointer methodsFor: 'accessing'!
staticClass
	^myClass!
*/
}
/**
 * You should only call this from Smalltalk code. For translatable code, use
 * SomeClass pointerToStaticMember: #selector:with:
 */
public static StaticFunctionPointer make(AboraClass classx, String selector) {
	return new StaticFunctionPointer(classx, selector);
/*
Xanadu-Xpp-Basic.st:135:StaticFunctionPointer class methodsFor: 'creation'!
make: class {Class} with: selector {Symbol}
	"You should only call this from Smalltalk code. For translatable code, use
		SomeClass pointerToStaticMember: #selector:with:"
	^self create: class with: selector!
*/
}
public StaticFunctionPointer() {
/*

Generated during transformation
*/
}
public StaticFunctionPointer(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
